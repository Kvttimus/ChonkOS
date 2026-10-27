#include "keyboard.h"
#include "interrupts.h"
#include "memory.h"
#include "x86.h"

#define KBD_DATA_PORT           0x60
#define KBD_PPI_PORT_B          0x61        // PC/XT only: bit 7 clears the keyboard
#define KBD_BUFFER_SIZE         32          // power of 2, so indexes can wrap w/ a mask

#define SCANCODE_RELEASED       0x80        // set in the scancode when a key is let go
#define SCANCODE_EXTENDED       0xE0        // prefix for keys added on the 101-key keyboard
#define SCANCODE_PAUSE          0xE1        // prefix for the 6 byte Pause key sequence

#define SC_CTRL                 0x1D
#define SC_LEFT_SHIFT           0x2A
#define SC_RIGHT_SHIFT          0x36
#define SC_ALT                  0x38
#define SC_CAPS_LOCK            0x3A
#define SC_NUM_LOCK             0x45
#define SC_SCROLL_LOCK          0x46
#define SC_KEYPAD_FIRST         0x47        // keypad 7/Home
#define SC_KEYPAD_LAST          0x53        // keypad ./Del

// BIOS data area: keyboard flags, so we start w/ the same Caps/Num Lock state the BIOS had
#define BDA_KEYBOARD_FLAGS      MK_FP(0x0040, 0x0017)
#define BDA_FLAG_NUM_LOCK       0x20
#define BDA_FLAG_CAPS_LOCK      0x40

// BIOS model byte at F000:FFFE, FF = PC, FE/FB = XT, FC = AT & later
#define BIOS_MODEL_BYTE         MK_FP(0xF000, 0xFFFE)

// US layout, scancodes 00h-39h
static const char g_Lower[] =
    "\0\x1B" "1234567890-=\b"
    "\tqwertyuiop[]\r"
    "\0asdfghjkl;'`"
    "\0\\zxcvbnm,./\0"
    "*\0 ";
static const char g_Upper[] =
    "\0\x1B" "!@#$%^&*()_+\b"
    "\tQWERTYUIOP{}\r"
    "\0ASDFGHJKL:\"~"
    "\0|ZXCVBNM<>?\0"
    "*\0 ";

// keypad 47h-53h w/ Num Lock on, 0 = not a digit key
static const char g_Keypad[] = "789-456+1230.";

static bool g_IsPcXt;

static volatile uint16_t g_Buffer[KBD_BUFFER_SIZE];
static volatile uint8_t g_BufferHead;           // only written by the interrupt handler
static volatile uint8_t g_BufferTail;           // only written by KBD_GetKey

static bool g_Shift, g_Ctrl, g_Alt, g_CapsLock, g_NumLock;
static bool g_Extended;
static uint8_t g_SkipBytes;

static volatile bool g_BreakRequested;
static KBD_BreakHandler g_BreakHandler;

static void KBD_Push(uint16_t key)
{
    uint8_t next = (g_BufferHead + 1) & (KBD_BUFFER_SIZE - 1);

    // buffer full, drop the key (the BIOS beeps here)
    if (next == g_BufferTail)
    {
        return;
    }

    g_Buffer[g_BufferHead] = key;
    g_BufferHead = next;
}

static void KBD_ProcessScancode(uint8_t scancode)
{
    bool released = (scancode & SCANCODE_RELEASED) != 0;
    bool extended = g_Extended;
    uint8_t code = scancode & ~SCANCODE_RELEASED;
    char c = 0;

    if (g_SkipBytes > 0)
    {
        g_SkipBytes--;
        return;
    }
    if (scancode == SCANCODE_EXTENDED)
    {
        g_Extended = true;
        return;
    }
    if (scancode == SCANCODE_PAUSE)
    {
        g_SkipBytes = 5;                        // Pause sends E1 1D 45 E1 9D C5, ignore it
        return;
    }
    g_Extended = false;

    // modifiers (right Ctrl/Alt are the same codes w/ the E0 prefix)
    switch (code)
    {
        case SC_LEFT_SHIFT:
        case SC_RIGHT_SHIFT:
            // 101-key keyboards send fake E0 shifts around some keys, ignore those
            if (!extended)
            {
                g_Shift = !released;
            }
            return;
        case SC_CTRL:
            g_Ctrl = !released;
            return;
        case SC_ALT:
            g_Alt = !released;
            return;
    }

    if (released)
    {
        return;
    }

    switch (code)
    {
        case SC_CAPS_LOCK:
            g_CapsLock = !g_CapsLock;
            return;
        case SC_NUM_LOCK:
            g_NumLock = !g_NumLock;
            return;
        case SC_SCROLL_LOCK:
            // Ctrl+Break: Ctrl+Pause sends E0 46 on a 101-key keyboard, Ctrl+Scroll Lock on an XT
            if (g_Ctrl)
            {
                g_BreakRequested = true;
            }
            return;
    }

    if (g_Ctrl && g_Alt && code == KEY_DELETE)
    {
        x86_Reboot();
    }

    if (code >= SC_KEYPAD_FIRST && code <= SC_KEYPAD_LAST)
    {
        // the gray arrow keys etc. are extended, the keypad ones are digits w/ Num Lock on
        if (!extended && (g_NumLock != g_Shift))
        {
            c = g_Keypad[code - SC_KEYPAD_FIRST];
        }
    }
    else if (extended && code == 0x1C)
    {
        c = '\r';                               // keypad Enter
    }
    else if (extended && code == 0x35)
    {
        c = '/';                                // keypad /
    }
    else if (code < sizeof(g_Lower) - 1)
    {
        c = g_Lower[code];

        if (c >= 'a' && c <= 'z')
        {
            if (g_Ctrl)
            {
                c = c - 'a' + 1;                // Ctrl+A = 1 ... Ctrl+Z = 26
            }
            else if (g_Shift != g_CapsLock)
            {
                c = g_Upper[code];              // Caps Lock only affects letters, & Shift undoes it
            }
        }
        else if (g_Shift)
        {
            c = g_Upper[code];
        }
    }

    KBD_Push(((uint16_t)code << 8) | (uint8_t)c);
}

static void __interrupt __far KBD_Handler()
{
    uint8_t scancode = x86_InByte(KBD_DATA_PORT);

    // the PC & XT keyboard interface won't send the next scancode until it's told this one
    // was read, by pulsing bit 7 of port 61h. AT & later keyboard controllers do this
    // themselves, and that bit means something else there
    if (g_IsPcXt)
    {
        uint8_t portB = x86_InByte(KBD_PPI_PORT_B);
        x86_OutByte(KBD_PPI_PORT_B, portB | 0x80);
        x86_OutByte(KBD_PPI_PORT_B, portB);
    }

    KBD_ProcessScancode(scancode);
    PIC_SendEndOfInterrupt();

    // after the end of interrupt, since the handler might not return (it can end a program)
    if (g_BreakRequested && g_BreakHandler != 0)
    {
        g_BreakRequested = false;
        g_BreakHandler();
    }
}

// BIOS keyboard service (int 16h), so programs that use it get keys from our driver instead of
// waiting forever on the BIOS's buffer, which nothing fills anymore
static void __interrupt __far KBD_BiosHandler(ISR_Registers registers)
{
    uint8_t function = registers.ax >> 8;

    // waiting for a key needs the keyboard interrupt, & the CPU turned interrupts off
    x86_EnableInterrupts();

    switch (function)
    {
        case 0x00:                              // read key: ah = scancode, al = ASCII
        case 0x10:
            registers.ax = KBD_GetKey();
            break;

        case 0x01:                              // check for key: ZF = 1 if none, else ax = key
        case 0x11:
            if (KBD_HasKey())
            {
                registers.ax = KBD_PeekKey();
                registers.flags &= ~FLAG_ZERO;
            }
            else
            {
                registers.flags |= FLAG_ZERO;
            }
            break;

        case 0x02:                              // shift flags in al
        case 0x12:
            registers.ax = (registers.ax & 0xFF00) | KBD_GetShiftFlags();
            break;
    }
}

void KBD_SetBreakHandler(KBD_BreakHandler handler)
{
    g_BreakHandler = handler;
}

uint16_t KBD_PeekKey()
{
    return KBD_HasKey() ? g_Buffer[g_BufferTail] : 0;
}

uint8_t KBD_GetShiftFlags()
{
    // same bits as the BIOS: 0 right shift, 1 left shift, 2 ctrl, 3 alt, 5 num lock, 6 caps lock
    return (g_Shift ? 0x02 : 0) | (g_Ctrl ? 0x04 : 0) | (g_Alt ? 0x08 : 0)
         | (g_NumLock ? 0x20 : 0) | (g_CapsLock ? 0x40 : 0);
}

void KBD_Initialize()
{
    uint8_t model = *(uint8_t __far*)BIOS_MODEL_BYTE;
    uint8_t biosFlags = *(uint8_t __far*)BDA_KEYBOARD_FLAGS;

    g_IsPcXt = (model == 0xFF || model == 0xFE || model == 0xFB);
    g_CapsLock = (biosFlags & BDA_FLAG_CAPS_LOCK) != 0;
    g_NumLock = (biosFlags & BDA_FLAG_NUM_LOCK) != 0;
    g_Shift = g_Ctrl = g_Alt = g_Extended = false;
    g_SkipBytes = 0;
    g_BufferHead = g_BufferTail = 0;
    g_BreakRequested = false;

    // fully replaces the BIOS keyboard handler & keyboard service
    IVT_SetVector(IRQ_TO_VECTOR(IRQ_KEYBOARD), KBD_Handler);
    IVT_SetVector(VECTOR_BIOS_KEYBOARD, (ISR)KBD_BiosHandler);
}

bool KBD_HasKey()
{
    return g_BufferHead != g_BufferTail;
}

uint16_t KBD_GetKey()
{
    uint16_t key;

    // sleep until the keyboard interrupt wakes us up w/ a key
    while (!KBD_HasKey())
    {
        x86_Halt();
    }

    key = g_Buffer[g_BufferTail];
    g_BufferTail = (g_BufferTail + 1) & (KBD_BUFFER_SIZE - 1);
    return key;
}

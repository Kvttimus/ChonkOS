#include "stdio.h"
#include "stdint.h"
#include "stdbool.h"
#include "x86.h"

void putc(char c)
{
    // BIOS teletype needs a carriage return to go back to the start of the line
    if (c == '\n')
    {
        x86_Video_WriteCharTeletype('\r', 0);
    }
    x86_Video_WriteCharTeletype(c, 0);
}

void puts(const char* str)
{
    while (*str)
    {
        putc(*str);
        str++;
    }
}

static const char g_HexChars[] = "0123456789abcdef";
static const char g_HexCharsUpper[] = "0123456789ABCDEF";

static void printf_number(uint32_t value, uint8_t radix, bool negative, uint8_t width, bool zeroPad, bool upper)
{
    char buffer[11];        // 32-bit number has at most 10 decimal digits
    uint8_t pos = 0;
    uint8_t length;

    // digits come out least significant first
    do
    {
        uint8_t digit = (uint8_t)(value % radix);
        value /= radix;
        buffer[pos++] = upper ? g_HexCharsUpper[digit] : g_HexChars[digit];
    } while (value > 0);

    length = pos + (negative ? 1 : 0);

    // "-0042" puts the sign before the zero padding, "  -42" puts it after the space padding
    if (negative && zeroPad)
    {
        putc('-');
    }
    for (; length < width; length++)
    {
        putc(zeroPad ? '0' : ' ');
    }
    if (negative && !zeroPad)
    {
        putc('-');
    }

    while (pos > 0)
    {
        putc(buffer[--pos]);
    }
}

void _cdecl printf(const char* fmt, ...)
{
    // all arguments are on the stack right after fmt, each one is at least a word (2 bytes)
    uint16_t* argp = (uint16_t*)&fmt + 1;

    while (*fmt)
    {
        bool zeroPad = false;
        bool isLong = false;
        uint8_t width = 0;
        uint32_t value;

        if (*fmt != '%')
        {
            putc(*fmt++);
            continue;
        }
        fmt++;

        // flags, width & length modifier
        if (*fmt == '0')
        {
            zeroPad = true;
            fmt++;
        }
        while (*fmt >= '0' && *fmt <= '9')
        {
            width = width * 10 + (*fmt - '0');
            fmt++;
        }
        if (*fmt == 'l')
        {
            isLong = true;
            fmt++;
        }

        switch (*fmt)
        {
            case 'c':
                putc((char)*argp);
                argp++;
                break;

            case 's':
                puts(*(const char**)argp);
                argp++;
                break;

            case '%':
                putc('%');
                break;

            case 'd':
            case 'i':
                if (isLong)
                {
                    int32_t signedValue = *(int32_t*)argp;
                    argp += 2;
                    value = (signedValue < 0) ? 0 - (uint32_t)signedValue : (uint32_t)signedValue;
                    printf_number(value, 10, signedValue < 0, width, zeroPad, false);
                }
                else
                {
                    int16_t signedValue = *(int16_t*)argp;
                    argp++;
                    value = (signedValue < 0) ? 0 - (uint32_t)(int32_t)signedValue : (uint32_t)signedValue;
                    printf_number(value, 10, signedValue < 0, width, zeroPad, false);
                }
                break;

            case 'u':
            case 'x':
            case 'X':
                if (isLong)
                {
                    value = *(uint32_t*)argp;
                    argp += 2;
                }
                else
                {
                    value = *argp;
                    argp++;
                }
                printf_number(value, (*fmt == 'u') ? 10 : 16, false, width, zeroPad, *fmt == 'X');
                break;

            default:
                // unknown specifier, print it as-is so the mistake is visible
                putc('%');
                if (*fmt)
                {
                    putc(*fmt);
                }
                break;
        }

        if (*fmt)
        {
            fmt++;
        }
    }
}

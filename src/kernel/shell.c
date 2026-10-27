#include "shell.h"
#include "stdint.h"
#include "stdbool.h"
#include "stdio.h"
#include "string.h"
#include "memory.h"
#include "alloc.h"
#include "fat.h"
#include "fs.h"
#include "keyboard.h"
#include "memtest.h"
#include "program.h"
#include "timer.h"
#include "x86.h"

#define SHELL_LINE_MAX      80
#define SHELL_PATH_MAX      FS_PATH_MAX
#define SHELL_HISTORY       8
#define SHELL_DRIVES        4               // A: - D:, same as the file system
#define SCREEN_ROWS         25
#define SCREEN_COLUMNS      80
#define COPY_BUFFER_SIZE    8192

#define KEY_CTRL_C          3
#define KEY_ESCAPE          27

// previous command lines, oldest first
static char g_History[SHELL_HISTORY][SHELL_LINE_MAX];
static uint8_t g_HistoryCount;

// lines printed since the last "-- More --"
static uint8_t g_PageLines;

// ------------------------------------------------------------------------------------------------
// helpers
// ------------------------------------------------------------------------------------------------

static bool SHELL_Check(FS_Status status)
{
    if (status != FS_OK)
    {
        printf("%s\n", FS_StatusString(status));
        return false;
    }
    return true;
}

// splits off the first word of line (null terminating it), returns the rest w/o leading spaces
static char* SHELL_NextWord(char* line)
{
    while (*line && *line != ' ')
    {
        line++;
    }
    if (*line)
    {
        *line++ = '\0';
    }
    while (*line == ' ')
    {
        line++;
    }
    return line;
}

static void SHELL_TrimEnd(char* str)
{
    uint16_t length = strlen(str);

    while (length > 0 && str[length - 1] == ' ')
    {
        str[--length] = '\0';
    }
}

// call after printing each line of long output, pauses when the screen is full.
// returns false if the user pressed Esc or Ctrl+C to stop
static bool SHELL_PageLine()
{
    char c;

    if (++g_PageLines < SCREEN_ROWS - 1)
    {
        return true;
    }

    g_PageLines = 0;
    puts("-- More --");
    c = KEY_ASCII(KBD_GetKey());
    puts("\r          \r");
    return c != KEY_ESCAPE && c != KEY_CTRL_C;
}

static bool SHELL_Resolve(const char* input, char* out)
{
    if (!FS_MakeFullPath(input, out))
    {
        printf("Invalid path\n");
        return false;
    }
    return true;
}

static bool SHELL_IsDirectory(const char* path)
{
    FS_Directory directory;
    return FS_OpenDirectory(path, &directory) == FS_OK;
}

static bool SHELL_FileExists(const char* path)
{
    FS_Handle handle;

    if (FS_Open(path, FS_READ, &handle) != FS_OK)
    {
        return false;
    }
    FS_Close(handle);
    return true;
}

// finds the program to run for a command: "hello" -> HELLO.COM or HELLO.EXE in the current
// directory, then in A:\ (where ChonkOS's programs are). Paths & extensions can be typed too
static bool SHELL_FindProgram(const char* command, char* pathOut)
{
    static const char* const extensions[] = { ".COM", ".EXE" };
    const char* name = command;
    const char* dot;
    bool hasPath = strchr(command, '\\') || strchr(command, '/') || strchr(command, ':');
    uint8_t place, i;

    if (strrchr(name, '\\'))
    {
        name = strrchr(name, '\\') + 1;
    }
    if (strrchr(name, '/'))
    {
        name = strrchr(name, '/') + 1;
    }
    dot = strchr(name, '.');

    for (place = 0; place < (hasPath ? 1 : 2); place++)
    {
        char candidate[SHELL_PATH_MAX];

        if (strlen(command) + 8 > SHELL_PATH_MAX)
        {
            return false;
        }
        strcpy(candidate, (place == 0) ? "" : "A:\\");
        strcat(candidate, command);

        if (dot != 0)
        {
            // only .COM & .EXE files can be run
            if ((stricmp(dot, extensions[0]) == 0 || stricmp(dot, extensions[1]) == 0)
                && SHELL_FileExists(candidate))
            {
                return FS_MakeFullPath(candidate, pathOut);
            }
            continue;
        }

        for (i = 0; i < 2; i++)
        {
            uint16_t length = strlen(candidate);

            strcat(candidate, extensions[i]);
            if (SHELL_FileExists(candidate))
            {
                return FS_MakeFullPath(candidate, pathOut);
            }
            candidate[length] = '\0';
        }
    }
    return false;
}

// ------------------------------------------------------------------------------------------------
// line editor
// ------------------------------------------------------------------------------------------------

static void SHELL_Redraw(uint8_t row, uint8_t column, const char* line, uint8_t length, uint8_t* shownLength, uint8_t cursor)
{
    uint8_t i;

    x86_Video_SetCursor(row, column);
    for (i = 0; i < length; i++)
    {
        putc(line[i]);
    }

    // blank out what's left of a longer line from before
    for (; i < *shownLength; i++)
    {
        putc(' ');
    }
    *shownLength = length;

    x86_Video_SetCursor(row, column + cursor);
}

static void SHELL_AddHistory(const char* line)
{
    uint8_t i;

    if (line[0] == '\0' || (g_HistoryCount > 0 && strcmp(g_History[g_HistoryCount - 1], line) == 0))
    {
        return;
    }

    // full: drop the oldest
    if (g_HistoryCount == SHELL_HISTORY)
    {
        for (i = 1; i < SHELL_HISTORY; i++)
        {
            strcpy(g_History[i - 1], g_History[i]);
        }
        g_HistoryCount--;
    }
    strcpy(g_History[g_HistoryCount++], line);
}

// reads a line w/ editing: arrows, Home/End, Delete, Backspace, Up/Down for history,
// Esc clears the line, Ctrl+C cancels it. The line always fits on the prompt's row
static void SHELL_ReadLine(char* line)
{
    uint8_t row, column;
    uint8_t maxLength;
    uint8_t length = 0, cursor = 0, shown = 0;
    uint8_t historyIndex = g_HistoryCount;
    uint8_t i;

    // stop before the last column, writing there would wrap (& maybe scroll the screen)
    x86_Video_GetCursor(&row, &column);
    maxLength = SCREEN_COLUMNS - 1 - column;
    if (maxLength > SHELL_LINE_MAX - 1)
    {
        maxLength = SHELL_LINE_MAX - 1;
    }

    for (;;)
    {
        uint16_t key = KBD_GetKey();
        char c = KEY_ASCII(key);

        if (c == '\r')
        {
            break;
        }
        else if (c == KEY_CTRL_C)
        {
            x86_Video_SetCursor(row, column + length);
            puts("^C");
            length = 0;
            break;
        }
        else if (c == KEY_ESCAPE)
        {
            length = cursor = 0;
        }
        else if (c == '\b')
        {
            if (cursor > 0)
            {
                for (i = cursor; i < length; i++)
                {
                    line[i - 1] = line[i];
                }
                cursor--;
                length--;
            }
        }
        else if ((uint8_t)c >= ' ' && (uint8_t)c != 0x7F)
        {
            if (length < maxLength)
            {
                for (i = length; i > cursor; i--)
                {
                    line[i] = line[i - 1];
                }
                line[cursor++] = c;
                length++;
            }
        }
        else if (c == '\0')
        {
            switch (KEY_SCANCODE(key))
            {
                case KEY_LEFT:
                    if (cursor > 0) cursor--;
                    break;
                case KEY_RIGHT:
                    if (cursor < length) cursor++;
                    break;
                case KEY_HOME:
                    cursor = 0;
                    break;
                case KEY_END:
                    cursor = length;
                    break;
                case KEY_DELETE:
                    if (cursor < length)
                    {
                        for (i = cursor + 1; i < length; i++)
                        {
                            line[i - 1] = line[i];
                        }
                        length--;
                    }
                    break;
                case KEY_UP:
                case KEY_DOWN:
                    if (KEY_SCANCODE(key) == KEY_UP && historyIndex > 0)
                    {
                        historyIndex--;
                    }
                    else if (KEY_SCANCODE(key) == KEY_DOWN && historyIndex < g_HistoryCount)
                    {
                        historyIndex++;
                    }
                    else
                    {
                        break;
                    }

                    // past the newest entry = back to an empty line
                    length = 0;
                    if (historyIndex < g_HistoryCount)
                    {
                        for (; g_History[historyIndex][length] && length < maxLength; length++)
                        {
                            line[length] = g_History[historyIndex][length];
                        }
                    }
                    cursor = length;
                    break;
            }
        }

        SHELL_Redraw(row, column, line, length, &shown, cursor);
    }

    line[length] = '\0';
    x86_Video_SetCursor(row, column + shown);
    putc('\n');
    SHELL_AddHistory(line);
}

// ------------------------------------------------------------------------------------------------
// commands
// ------------------------------------------------------------------------------------------------

void SHELL_PrintDrives()
{
    char letter;

    for (letter = 'A'; letter < 'A' + SHELL_DRIVES; letter++)
    {
        FS_DriveInfo info;
        uint32_t freeBytes = 0;

        if (FS_GetDriveInfo(letter, &info))
        {
            FS_GetFreeSpace(letter, &freeBytes);
            printf("%c:  FAT%u  %-11s  BIOS drive %02xh  %lu KiB, %lu KiB free\n", letter, info.fatType,
                   info.label, info.biosDrive, info.totalBytes / 1024, freeBytes / 1024);
        }
    }
}

static void cmd_drives(char* args)
{
    (void)args;                         // takes no arguments
    SHELL_PrintDrives();
}

static void cmd_cls(char* args)
{
    (void)args;                         // takes no arguments
    clrscr();
}

static void cmd_ver(char* args)
{
    (void)args;                         // takes no arguments
    printf("ChonkOS version %s - 16-bit real mode, 8086 compatible\n", CHONKOS_VERSION);
}

static void cmd_reboot(char* args)
{
    (void)args;                         // takes no arguments
    x86_Reboot();
}

static void cmd_cd(char* args)
{
    // "cd" & "cd X:" print the current directory
    if (args[0] == '\0' || (args[1] == ':' && args[2] == '\0'))
    {
        char drive = args[0] ? toupper(args[0]) : FS_GetDefaultDrive();
        const char* current = FS_GetCurrentDirectory(drive);

        if (current == 0)
        {
            SHELL_Check(FS_INVALID_DRIVE);
            return;
        }
        printf("%c:%s\n", drive, current);
        return;
    }

    SHELL_Check(FS_ChangeDirectory(args));
}

static void cmd_dir(char* args)
{
    char path[SHELL_PATH_MAX];
    FS_Directory directory;
    FS_DirectoryInfo info;
    FS_DriveInfo drive;
    uint16_t files = 0, directories = 0;
    uint32_t totalBytes = 0, freeBytes = 0;
    FS_Status status;

    if (!SHELL_Resolve(args, path) || !SHELL_Check(FS_OpenDirectory(path, &directory)))
    {
        return;
    }

    FS_GetDriveInfo(path[0], &drive);
    g_PageLines = 0;
    printf(" Volume in drive %c is %s\n Directory of %s\n\n", path[0], drive.label, path);
    g_PageLines += 3;

    while ((status = FS_ReadDirectory(&directory, &info)) == FS_OK)
    {
        printf("%-12s ", info.name);
        if (info.attributes & FAT_ATTRIBUTE_DIRECTORY)
        {
            printf("     <DIR>");
            directories++;
        }
        else
        {
            printf("%10lu", info.size);
            files++;
            totalBytes += info.size;
        }

        // FAT date & time: year since 1980, month, day / hours, minutes
        printf("  %04u-%02u-%02u %02u:%02u\n", 1980 + (info.date >> 9), (info.date >> 5) & 0x0F, info.date & 0x1F,
               info.time >> 11, (info.time >> 5) & 0x3F);

        if (!SHELL_PageLine())
        {
            return;
        }
    }
    if (status != FS_END)
    {
        SHELL_Check(status);
    }

    FS_GetFreeSpace(path[0], &freeBytes);
    printf("%10u file(s) %10lu bytes\n%10u dir(s)  %10lu bytes free\n", files, totalBytes, directories, freeBytes);
}

static void cmd_type(char* args)
{
    char path[SHELL_PATH_MAX];
    char buffer[256];
    char last = '\n';
    FS_Handle handle;
    uint16_t bytesRead;
    uint16_t i;

    if (args[0] == '\0')
    {
        printf("Usage: type <file>\n");
        return;
    }
    if (!SHELL_Resolve(args, path) || !SHELL_Check(FS_Open(path, FS_READ, &handle)))
    {
        return;
    }

    while (SHELL_Check(FS_Read(handle, buffer, sizeof(buffer), &bytesRead)) && bytesRead > 0)
    {
        for (i = 0; i < bytesRead; i++)
        {
            putc(buffer[i]);
        }
        last = buffer[bytesRead - 1];
    }
    FS_Close(handle);

    // keep the prompt on its own line even if the file doesn't end w/ a newline
    if (last != '\n')
    {
        putc('\n');
    }
}

static void cmd_copy(char* args)
{
    char* destinationArg = SHELL_NextWord(args);
    char source[SHELL_PATH_MAX], destination[SHELL_PATH_MAX];
    uint8_t __far* buffer;
    FS_Handle sourceHandle, destinationHandle;
    uint16_t bytesRead, written;
    uint32_t total = 0;

    if (args[0] == '\0' || destinationArg[0] == '\0')
    {
        printf("Usage: copy <source> <destination>\n");
        return;
    }
    if (!SHELL_Resolve(args, source) || !SHELL_Resolve(destinationArg, destination))
    {
        return;
    }

    // copying into a directory keeps the file's name
    if (SHELL_IsDirectory(destination))
    {
        const char* name = strrchr(source, '\\') + 1;

        if (strlen(destination) + strlen(name) + 2 > SHELL_PATH_MAX)
        {
            printf("Invalid path\n");
            return;
        }
        if (destination[strlen(destination) - 1] != '\\')
        {
            strcat(destination, "\\");
        }
        strcat(destination, name);
    }

    if (strcmp(source, destination) == 0)
    {
        printf("File cannot be copied onto itself\n");
        return;
    }

    buffer = (uint8_t __far*)kmalloc(COPY_BUFFER_SIZE);
    if (buffer == 0)
    {
        printf("Not enough memory\n");
        return;
    }

    if (SHELL_Check(FS_Open(source, FS_READ, &sourceHandle)))
    {
        if (SHELL_Check(FS_Open(destination, FS_WRITE | FS_CREATE | FS_TRUNCATE, &destinationHandle)))
        {
            while (SHELL_Check(FS_Read(sourceHandle, buffer, COPY_BUFFER_SIZE, &bytesRead)) && bytesRead > 0)
            {
                if (!SHELL_Check(FS_Write(destinationHandle, buffer, bytesRead, &written)))
                {
                    break;
                }
                total += written;
            }
            FS_Close(destinationHandle);
            printf("%lu bytes copied to %s\n", total, destination);
        }
        FS_Close(sourceHandle);
    }

    kfree(buffer);
}

static void cmd_del(char* args)
{
    char path[SHELL_PATH_MAX];

    if (args[0] == '\0')
    {
        printf("Usage: del <file>\n");
        return;
    }
    if (SHELL_Resolve(args, path))
    {
        SHELL_Check(FS_Delete(path));
    }
}

static void cmd_ren(char* args)
{
    char* newName = SHELL_NextWord(args);
    char path[SHELL_PATH_MAX];

    if (args[0] == '\0' || newName[0] == '\0')
    {
        printf("Usage: ren <file> <new name>\n");
        return;
    }
    if (strchr(newName, '\\') || strchr(newName, '/') || strchr(newName, ':'))
    {
        printf("The new name can't have a path, ren only renames in place\n");
        return;
    }
    if (SHELL_Resolve(args, path))
    {
        SHELL_Check(FS_Rename(path, newName));
    }
}

static void cmd_md(char* args)
{
    char path[SHELL_PATH_MAX];

    if (args[0] == '\0')
    {
        printf("Usage: md <directory>\n");
        return;
    }
    if (SHELL_Resolve(args, path))
    {
        SHELL_Check(FS_MakeDirectory(path));
    }
}

static void cmd_rd(char* args)
{
    if (args[0] == '\0')
    {
        printf("Usage: rd <directory>\n");
        return;
    }
    SHELL_Check(FS_RemoveDirectory(args));
}

// "echo text", "echo text > file" (replace the file), "echo text >> file" (add to the end)
static void cmd_echo(char* args)
{
    char path[SHELL_PATH_MAX];
    char* redirect = strchr(args, '>');
    bool append;
    FS_Handle handle;
    uint32_t size;
    uint16_t written;

    if (redirect == 0)
    {
        printf("%s\n", args);
        return;
    }

    append = (redirect[1] == '>');
    *redirect = '\0';
    redirect += append ? 2 : 1;
    while (*redirect == ' ')
    {
        redirect++;
    }
    SHELL_TrimEnd(args);
    SHELL_TrimEnd(redirect);

    if (redirect[0] == '\0' || !SHELL_Resolve(redirect, path))
    {
        printf("Usage: echo <text> > <file>\n");
        return;
    }

    if (!SHELL_Check(FS_Open(path, FS_WRITE | FS_CREATE | (append ? 0 : FS_TRUNCATE), &handle)))
    {
        return;
    }
    if (append)
    {
        FS_GetSize(handle, &size);
        FS_Seek(handle, size);
    }
    if (SHELL_Check(FS_Write(handle, args, strlen(args), &written)))
    {
        SHELL_Check(FS_Write(handle, "\r\n", 2, &written));
    }
    FS_Close(handle);
}

static void cmd_mem(char* args)
{
    uint32_t freeBytes, largestBytes;
    uint16_t blocks;

    if (stricmp(args, "/test") == 0)
    {
        memtest();
        return;
    }

    MEM_Dump();
    MEM_GetStats(&freeBytes, &largestBytes, &blocks);
    printf("%lu bytes free, largest free block %lu bytes\n", freeBytes, largestBytes);
}

static void cmd_time(char* args)
{
    uint32_t date = x86_RTC_GetDate();
    uint32_t time = x86_RTC_GetTime();
    uint32_t seconds = TIMER_GetTicks() * 10 / TIMER_TICKS_PER_10_SECONDS;

    (void)args;                         // takes no arguments

    // the clock gives BCD, which prints right as hex
    if (date == 0xFFFFFFFF || time == 0xFFFFFFFF)
    {
        printf("No real time clock (PC/XT)\n");
    }
    else
    {
        printf("%02x%02x-%02x-%02x %02x:%02x:%02x\n", (uint8_t)(date >> 24), (uint8_t)(date >> 16),
               (uint8_t)(date >> 8), (uint8_t)date, (uint8_t)(time >> 16), (uint8_t)(time >> 8), (uint8_t)time);
    }
    printf("Up for %lu:%02lu:%02lu\n", seconds / 3600, seconds / 60 % 60, seconds % 60);
}

static void cmd_help(char* args);

typedef struct
{
    const char* name;
    void (*function)(char* args);
    const char* help;               // 0 for aliases, so they're not listed twice
} SHELL_Command;

static const SHELL_Command g_Commands[] =
{
    { "cd",      cmd_cd,     "cd [dir]           Show or change the current directory" },
    { "chdir",   cmd_cd,     0 },
    { "cls",     cmd_cls,    "cls                Clear the screen" },
    { "copy",    cmd_copy,   "copy <src> <dst>   Copy a file (dst can be a directory)" },
    { "del",     cmd_del,    "del <file>         Delete a file" },
    { "erase",   cmd_del,    0 },
    { "dir",     cmd_dir,    "dir [dir]          List a directory" },
    { "drives",  cmd_drives, "drives             List drives & free space" },
    { "echo",    cmd_echo,   "echo <text>[>file] Print text, or write it to a file (>> adds to it)" },
    { "help",    cmd_help,   "help               This list" },
    { "md",      cmd_md,     "md <dir>           Make a directory" },
    { "mkdir",   cmd_md,     0 },
    { "mem",     cmd_mem,    "mem [/test]        Show memory blocks (or test the allocator)" },
    { "rd",      cmd_rd,     "rd <dir>           Remove an empty directory" },
    { "rmdir",   cmd_rd,     0 },
    { "reboot",  cmd_reboot, "reboot             Restart the computer" },
    { "ren",     cmd_ren,    "ren <file> <name>  Rename a file or directory" },
    { "rename",  cmd_ren,    0 },
    { "time",    cmd_time,   "time               Show the date, time & uptime" },
    { "type",    cmd_type,   "type <file>        Print a file" },
    { "ver",     cmd_ver,    "ver                Show the ChonkOS version" },
};

#define COMMAND_COUNT (sizeof(g_Commands) / sizeof(g_Commands[0]))

static void cmd_help(char* args)
{
    uint8_t i;

    (void)args;                         // takes no arguments
    g_PageLines = 0;
    for (i = 0; i < COMMAND_COUNT; i++)
    {
        if (g_Commands[i].help != 0)
        {
            printf("%s\n", g_Commands[i].help);
            if (!SHELL_PageLine())
            {
                return;
            }
        }
    }
    printf("X:                 Switch to drive X\n");
    printf("Paths can be relative (docs\\a.txt, ..\\b) or full (C:\\DOCS\\A.TXT).\n");
    printf("Keys: arrows, Home, End, Del to edit, Up/Down for history, Esc to clear.\n");
}

static void SHELL_Execute(char* line)
{
    char* args;
    uint8_t i;

    while (*line == ' ')
    {
        line++;
    }
    args = SHELL_NextWord(line);
    SHELL_TrimEnd(args);

    if (line[0] == '\0')
    {
        return;
    }

    // "A:", "C:" switch the current drive
    if (line[1] == ':' && line[2] == '\0')
    {
        SHELL_Check(FS_SetDefaultDrive(line[0]));
        return;
    }

    for (i = 0; i < COMMAND_COUNT; i++)
    {
        if (stricmp(line, g_Commands[i].name) == 0)
        {
            g_Commands[i].function(args);
            return;
        }
    }

    // not built in, look for a program
    {
        char path[SHELL_PATH_MAX];
        uint16_t exitCode;

        if (!SHELL_FindProgram(line, path))
        {
            printf("Bad command or file name\n");
            return;
        }

        if (PROGRAM_Run(path, args, &exitCode) && PROGRAM_EXIT_TYPE(exitCode) == PROGRAM_EXIT_NORMAL
            && PROGRAM_EXIT_CODE(exitCode) != 0)
        {
            printf("(exit code %u)\n", PROGRAM_EXIT_CODE(exitCode));
        }
    }
}

void SHELL_Run()
{
    char line[SHELL_LINE_MAX];

    for (;;)
    {
        char drive = FS_GetDefaultDrive();

        printf("%c:%s> ", drive, FS_GetCurrentDirectory(drive));
        SHELL_ReadLine(line);
        SHELL_Execute(line);
    }
}

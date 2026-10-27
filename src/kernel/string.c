#include "string.h"

uint16_t strlen(const char* str)
{
    uint16_t length = 0;

    while (*str++)
    {
        length++;
    }
    return length;
}

int strcmp(const char* a, const char* b)
{
    while (*a && *a == *b)
    {
        a++;
        b++;
    }
    return (int)(uint8_t)*a - (int)(uint8_t)*b;
}

int stricmp(const char* a, const char* b)
{
    while (*a && toupper(*a) == toupper(*b))
    {
        a++;
        b++;
    }
    return (int)(uint8_t)toupper(*a) - (int)(uint8_t)toupper(*b);
}

char* strcpy(char* dst, const char* src)
{
    char* start = dst;

    while ((*dst++ = *src++) != '\0');
    return start;
}

char* strcat(char* dst, const char* src)
{
    strcpy(dst + strlen(dst), src);
    return dst;
}

char* strchr(const char* str, char c)
{
    for (; *str; str++)
    {
        if (*str == c)
        {
            return (char*)str;
        }
    }
    return (c == '\0') ? (char*)str : 0;
}

char* strrchr(const char* str, char c)
{
    const char* last = 0;

    for (; *str; str++)
    {
        if (*str == c)
        {
            last = str;
        }
    }
    return (char*)last;
}

char toupper(char c)
{
    return (c >= 'a' && c <= 'z') ? c - 'a' + 'A' : c;
}

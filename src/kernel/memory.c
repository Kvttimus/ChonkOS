#include "memory.h"

void* memcpy(void* dst, const void* src, uint16_t count)
{
    uint8_t* d = (uint8_t*)dst;
    const uint8_t* s = (const uint8_t*)src;

    while (count--)
    {
        *d++ = *s++;
    }
    return dst;
}

void* memset(void* dst, int value, uint16_t count)
{
    uint8_t* d = (uint8_t*)dst;

    while (count--)
    {
        *d++ = (uint8_t)value;
    }
    return dst;
}

int memcmp(const void* a, const void* b, uint16_t count)
{
    const uint8_t* x = (const uint8_t*)a;
    const uint8_t* y = (const uint8_t*)b;

    for (; count > 0; count--, x++, y++)
    {
        if (*x != *y)
        {
            return (*x < *y) ? -1 : 1;
        }
    }
    return 0;
}

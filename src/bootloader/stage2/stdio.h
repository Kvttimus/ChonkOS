#pragma once

void putc(char c);
void puts(const char* str);

// supports %c %s %d %i %u %x %X %%, the 'l' (long) modifier, and a width w/ optional '0' padding
void _cdecl printf(const char* fmt, ...);

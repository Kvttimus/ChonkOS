#pragma once

void clrscr();
void putc(char c);
void puts(const char* str);

// supports %c %s %d %i %u %x %X %%, the 'l' (long) modifier, a width w/ optional '0' padding,
// and '-' to left align strings ("%-12s")
void _cdecl printf(const char* fmt, ...);

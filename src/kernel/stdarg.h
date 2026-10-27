#pragma once

// variadic arguments are on the stack right after the last named one, each one rounded up to a
// whole word (can't push a single byte on the stack). works in any memory model since it
// uses sizeof instead of assuming how big pointers are
typedef unsigned char* va_list;

#define _VA_SIZE(type)          ((sizeof(type) + 1) & ~1)
#define va_start(ap, last)      ((ap) = (va_list)&(last) + _VA_SIZE(last))
#define va_arg(ap, type)        (*(type*)(((ap) += _VA_SIZE(type)) - _VA_SIZE(type)))
#define va_end(ap)              ((void)0)

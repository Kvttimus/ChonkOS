#pragma once
#include "stdint.h"

uint16_t strlen(const char* str);
int strcmp(const char* a, const char* b);
int stricmp(const char* a, const char* b);      // case insensitive
char* strcpy(char* dst, const char* src);
char* strcat(char* dst, const char* src);
char* strchr(const char* str, char c);
char* strrchr(const char* str, char c);
char toupper(char c);

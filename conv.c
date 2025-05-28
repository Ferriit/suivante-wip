#include <stdlib.h>
#include <stdio.h>
#include "conv.h"

int atoi(const char* str) {
    return (int)strtol(str, NULL, 10);
}

char[] itoa(int Num) {
    char* returnval = malloc(sizeof(char) * 20);
    snprintf(returnval, 20, "%d", Num);
    return returnval;
}

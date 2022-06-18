#include "io.h"
#include "types.h"

#include <unistd.h>
#include <stdio.h>

extern void println(const char *message)
{
    printf("%s\n", message);
}

extern void putnum(U8 num)
{
    printf("%llu\n", num);
}


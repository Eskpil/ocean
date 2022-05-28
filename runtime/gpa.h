// General Purpose Allocator

#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <stdint.h>

extern void *gpa_allocate_sized(size_t);


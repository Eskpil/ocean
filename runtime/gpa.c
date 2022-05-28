#include "gpa.h"
#include <stdlib.h>

extern void *gpa_allocate_sized(size_t size) {
    void *memory = malloc(size);
    return memory;
}

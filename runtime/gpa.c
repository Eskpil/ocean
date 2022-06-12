#include "gpa.h"
#include "types.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

extern void *gpa_allocate_sized(size_t size) {
    void *memory = malloc(size);
    return memory;
}

extern void *gpa_allocate_counted(size_t size) {
    void *memory = malloc(size + 8);     

    memset(memory, 0, 8);

    return memory;
}

extern void gpa_memory_set_object_field(
    void *memory,
    u8 offset,
    void *data
) {
    offset += 8; 
    printf("[INFO]: Set object field: %p\n", (const void *)data);

    u8 *data_ref_count = (u8*)(data - 8); 
    *data_ref_count += 1;

    void *location = (void *)(data - offset);
    location = data;
}

extern void gpa_memory_set_num_field(
    void *memory, 
    u8 offset,
    u8 data 
) {
    offset += 8; 

    printf("[INFO]: Set num field: %lld\n", data);

    u8 *location = (u8 *)(data - offset);
    location = data;
}

extern void gpa_memory_set_ptr_field(
    void *memory, 
    u8 offset, 
    void *data
) {
    offset += 8; 
    printf("[INFO]: Set ptr field: %p\n", (const void *)data);

    void *location = (void *)(data - offset);
    location = data;
}

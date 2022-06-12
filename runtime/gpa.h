// General Purpose Allocator

#ifndef RUNTIME_GPA_H_
#define RUNTIME_GPA_H_

#include "types.h"

#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <stdint.h>

struct mem_header {
    int count;
    int check; 
};

extern void *gpa_allocate_sized(size_t);
extern void *gpa_allocate_counted(size_t);
extern void gpa_memory_free(void *memory);

extern void gpa_memory_ref_inc(void *memory);
extern void gpa_memory_ref_dec(void *memory);
extern void gpa_memory_set_object_field(void *memory, u8 offset, void *data);
extern void gpa_memory_set_num_field(void *memory, u8 offset, u8 data);
extern void gpa_memory_set_ptr_field(void *memory, u8 offset, void *data);


#endif // RUNTIME_GPA_H_

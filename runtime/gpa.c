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
    struct mem_header *memory = malloc(size + sizeof(struct mem_header));     

    if (!memory) return NULL;
	memory->count = 1;
	memory->check = (intptr_t)(memory + 1) & 0xFFFFFFFF;
	return memory + 1;
}

extern void *gpa_allocate_counted_from(size_t size, void *prev) { 
    struct mem_header *const prev_header = prev - sizeof(struct mem_header);
    struct mem_header *memory = malloc(size + sizeof(struct mem_header));

    if (!memory) return NULL;

    memory->count = prev_header->count;
    memory->check = prev_header->check;

    return memory + 1;
}

extern void gpa_memory_free(void *memory) {
    free(memory);
}

extern void gpa_memory_ref_inc(
    void *memory        
) {
    struct mem_header* const header = memory - sizeof(struct mem_header);

	if (header->check != ((intptr_t)memory & 0xFFFFFFFF)) {
        printf("Check: %x\n", header->check);
        abort();
    }

	header->count += 1;
} 

extern void gpa_memory_ref_dec(
    void *memory        
) {
    struct mem_header* const header = memory - sizeof(struct mem_header);

	if (header->check != ((intptr_t)memory & 0xFFFFFFFF)) {
        abort();
    }

	header->count -= 1;
	if (header->count == 0) {
        gpa_memory_free(header);
	}

}

extern void gpa_memory_set_object_field(
    void *memory,
    U8 offset,
    void *data
) {
    struct mem_header* const header = memory - sizeof(struct mem_header);

	if (header->check != ((intptr_t)memory & 0xFFFFFFFF)) {
        abort();
    }

    gpa_memory_ref_inc(data);

    void **location = ((void **)memory) + offset;
    *location = data;
}

extern void gpa_memory_set_num_field(
    void *memory, 
    U8 offset,
    U8 data 
) {
    struct mem_header* const header = memory - sizeof(struct mem_header);

	if (header->check != ((intptr_t)memory & 0xFFFFFFFF)) {
        abort();
    }

    U8 *location = ((U8 *)memory) + offset;
    *location = data;
}

extern void gpa_memory_set_ptr_field(
    void *memory, 
    U8 offset, 
    void *data
) {
    struct mem_header* const header = memory - sizeof(struct mem_header);

	if (header->check != ((intptr_t)memory & 0xFFFFFFFF)) {
        abort();
    }

    void **location = ((void **)memory) + offset;
    *location = data;
}

extern void *gpa_memory_get_object_field(
    void *memory,
    U8 offset
) {
    struct mem_header* const header = memory - sizeof(struct mem_header);

	if (header->check != ((intptr_t)memory & 0xFFFFFFFF)) {
        abort();
    }

    void **location = ((void **)memory) + offset;
    return *location;
}

extern U8 gpa_memory_get_num_field(
    void *memory, 
    U8 offset
) {
    struct mem_header* const header = memory - sizeof(struct mem_header);

	if (header->check != ((intptr_t)memory & 0xFFFFFFFF)) {
        abort();
    }

    U8 *location = ((U8 *)memory) + offset;

    return *location;
}

extern void *gpa_memory_get_ptr_field(
    void *memory, 
    U8 offset
) {
    struct mem_header* const header = memory - sizeof(struct mem_header);

	if (header->check != ((intptr_t)memory & 0xFFFFFFFF)) {
        abort();
    }

    void **location = ((void **)memory) + offset;

    return *location;
}

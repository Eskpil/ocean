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

    printf("[INFO]: Allocated counted memory: %p\n", (const void *) memory);

    if (!memory) return NULL;
	memory->count = 1;
	memory->check = (intptr_t)(memory + 1) & 0xFFFFFFFF;
	return memory + 1;
}

extern void gpa_memory_free(void *memory) {
    printf("[INFO]: Freeing memory: %p\n", (const void *) memory);
    free(memory);
}

extern void gpa_memory_ref_inc(
    void *memory        
) {
    printf("[INFO]: Increment reference on: %p\n", (const void *) memory);

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
    printf("[INFO]: Decrement reference on: %p\n", (const void *) memory);
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
    u8 offset,
    void *data
) {
    printf("[INFO]: Set object field: %p\n", (const void *)data);

    struct mem_header* const header = memory - sizeof(struct mem_header);

	if (header->check != ((intptr_t)memory & 0xFFFFFFFF)) {
        abort();
    }

    gpa_memory_ref_inc(data);

    void *location = (void *)(memory - offset);
    location = data;
}

extern void gpa_memory_set_num_field(
    void *memory, 
    u8 offset,
    u8 data 
) {
    printf("[INFO]: Memory: %p\n", (const void *) memory);
    printf("[INFO]: Set num field: %lld\n", data);

    struct mem_header* const header = memory - sizeof(struct mem_header);

	if (header->check != ((intptr_t)memory & 0xFFFFFFFF)) {
        abort();
    }

    u8 *location = (u8 *)(memory - offset);
    location = data;
}

extern void gpa_memory_set_ptr_field(
    void *memory, 
    u8 offset, 
    void *data
) {
    printf("[INFO]: Memory: %p\n", (const void *)memory);
    printf("[INFO]: Set ptr field: %p\n", (const void *)data);

    struct mem_header* const header = memory - sizeof(struct mem_header);

	if (header->check != ((intptr_t)memory & 0xFFFFFFFF)) {
        abort();
    }

    void *location = (void *)(memory - offset);
    location = data;
}

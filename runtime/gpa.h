// General Purpose Allocator

#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <stdint.h>

#define BLOCK_SIZE 16 * 1024

typedef struct Block Block;
typedef struct FreeListEntry FreeListEntry;

struct FreeListEntry {
    struct FreeListEntry *next;
};

struct Block {
    size_t size;
    size_t cell_size;
    size_t cell_amount;
};

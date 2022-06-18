#ifndef RUNTIME_ARRAY_H_
#define RUNTIME_ARRAY_H_

#include "types.h"

struct array {
    U8 size;
    U8 capacity; 
    U8 elem_size;

    void *first;
    void *last;
};

extern struct array *runtime_allocate_array(U8 elem_size, U8 nmb);

extern struct array *runtime_array_append(struct array *array, void *data);
extern void *runtime_array_at(struct array *array, U8 index);
extern void *runtime_delete(struct array *array, U8 index);

extern struct array *runtime_prepopulate_array(void *data, U8 size, U8 nmb);

#endif // RUNTIME_ARRAY_H_

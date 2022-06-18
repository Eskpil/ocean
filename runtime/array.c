#include "array.h"
#include "types.h"
#include "gpa.h"
#include "io.h"

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>

extern struct array *runtime_allocate_array(
    U8 elem_size, 
    U8 nelem
) {
    /* 
     * We need to make sure at least one element is accounted for when 
     * creating the array in case the array expands 0 * 2 = 0.
     */

    if (nelem == 0) {
        nelem = 1;
    }

    size_t struct_size = sizeof(struct array);

    struct array *array = gpa_allocate_counted(struct_size + (nelem * elem_size)); 

    array->size = 0;
    array->capacity = nelem;
    array->elem_size = elem_size;

    // void *data = gpa_allocate_sized(nelem * elem_size);
    void *data = array + struct_size;

    array->first = data;
    array->last = data;

    return array;
}

extern U8 runtime_array_at_num(
    struct array *array,     
    U8 index
) {
    U8 *location = ((U8 *)(array->first + (index * array->elem_size)));

    return *location;
}

extern void *runtime_array_at_ptr(
    struct array *array,     
    U8 index
) {
    void **location = ((void **)(array->first + (index * array->elem_size)));

    return *location;
}

extern void *runtime_array_delete(
    struct array *array,
    U8 index    
) {
    if (array->size - 1 == array->capacity / 2) {
        fprintf(stdout, "TODO: Implement creating the array with smaller size.\n"); 
    }

    if (index > array->size) {
        fprintf(stderr, "Index is out of bounds.\n");
        exit(1);
    } 

    U8 offset = array->elem_size *index;
    void *elem = ((void *)array->last) - offset;

    for (U8 i = index; i < array->size; ++i) {
        U8 offset = array->elem_size * i;

        void *data = ((void *)array->last) - offset; 
        void **location = ((void **)array->last) - (offset + array->elem_size); 

        *location = data;
    }

    return elem;
}

extern struct array *runtime_array_append_num(
    struct array *array,
    U8 data 
) {
    if (array->size == 0) {
        *((U8 *)array->first) = data;
        *((U8 *)array->last) = data;

        array->last += array->elem_size;

        array->size += 1; 

        return array;
    } else if (array->size == array->capacity) {
        U8 capacity = array->capacity * 2;
        struct array *new_array = gpa_allocate_counted_from((array->elem_size * capacity) + sizeof(struct array), array);  

        new_array->size = 0; 
        new_array->capacity = capacity;
        new_array->elem_size = array->elem_size;

        void *data_loc = new_array + sizeof(struct array);

        new_array->first = data_loc;
        new_array->last = data_loc;

        for (U8 i = 0; i < array->size; ++i) {
            U8 offset = array->elem_size * i;
            U8 *new_data = ((U8 *)array->first) + offset;

            if (i == 0) {
                *((U8 *)new_array->first) = *new_data;       
            }
            *((U8 *)new_array->last) = *new_data;

            new_array->last += array->elem_size;
            new_array->size += 1;
        }

        gpa_memory_free(array);

        *((U8*)new_array->last) = data;

        new_array->last += array->elem_size;
        new_array->size += 1;

        return new_array;
    } else {
        *((U8 *)array->last) = data;

        array->last += array->elem_size;

        array->size += 1;

        return array;
    }
}

extern struct array *runtime_array_append_ptr(
    struct array *array,
    void *data 
) {
    if (array->size == 0) {
        *((void **)array->first) = data;
        *((void **)array->last) = data;

        array->last += array->elem_size;

        array->size += 1; 

        return array;
    } else if (array->size == array->capacity) {
        U8 capacity = array->capacity * 2;
        struct array *new_array = gpa_allocate_counted_from((array->elem_size * capacity) + sizeof(struct array), array);  

        new_array->size = 0; 
        new_array->capacity = capacity;
        new_array->elem_size = array->elem_size;

        void *data_loc = new_array + sizeof(struct array);

        new_array->first = data_loc;
        new_array->last = data_loc;

        for (U8 i = 0; i < array->size; ++i) {
            U8 offset = array->elem_size * i;
            void **new_data = ((void **)array->first) + offset;

            if (i == 0) {
                *((void **)new_array->first) = *new_data;       
            }
            *((void **)new_array->last) = *new_data;

            new_array->last += array->elem_size;
            new_array->size += 1;
        }

        gpa_memory_free(array);

        *((void **)new_array->last) = data;

        new_array->last += array->elem_size;
        new_array->size += 1;

        return new_array;
    } else {
        *((void **)array->last) = data;

        array->last += array->elem_size;

        array->size += 1;

        return array;
    }
}

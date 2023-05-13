#ifndef MAIN_H_
#define MAIN_H_

#include "compagnon_uf.h"

uf_partition_t uf_initialise(int size);
void uf_free(uf_partition_t tab, int size);
uf_elem_t uf_find_no(uf_elem_t x);
uf_elem_t uf_find(uf_elem_t x);
void uf_union(uf_elem_t a, uf_elem_t b);

#endif // MAIN_H_

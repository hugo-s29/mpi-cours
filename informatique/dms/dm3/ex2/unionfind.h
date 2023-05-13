#ifndef UNIONFIND_H_
#define UNIONFIND_H_

// Fichier extrait de l'exercice 1
// (et fonction `print` du fichier compagnon)

#include<assert.h>
#include<stdbool.h>
#include<stdio.h>
#include<stdlib.h>
#include<time.h>

struct uf_elem_s {
  int             rank   ;
  int             elem   ;
  struct uf_elem_s* parent ;
};

typedef struct uf_elem_s* uf_elem_t;

typedef uf_elem_t* uf_partition_t;

void print_uf_partition_verbose(uf_partition_t p, int n);
uf_partition_t uf_initialise(int size);
void uf_free(uf_partition_t tab, int size);
uf_elem_t uf_find_no(uf_elem_t x);
uf_elem_t uf_find(uf_elem_t x);
void uf_union(uf_elem_t a, uf_elem_t b);

#endif // UNIONFIND_H_

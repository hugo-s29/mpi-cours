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
uf_partition_t exemple();

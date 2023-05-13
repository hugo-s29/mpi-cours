#include "main.h"

uf_partition_t uf_initialise(int size) {
	uf_partition_t partition = malloc(size * sizeof(uf_elem_t));
	assert(partition != NULL);

	for(int i = 0; i < size; ++i) {
		uf_elem_t elem = malloc(sizeof(struct uf_elem_s));
		assert(elem != NULL);

		elem->rank = 0;
		elem->elem = i;
		elem->parent = elem;

		partition[i] = elem;
	}

	return partition;
}

void uf_free(uf_partition_t tab, int size) {
	for(int i = 0; i < size; ++i) free(tab[i]);
	free(tab);
}

uf_elem_t uf_find_no(uf_elem_t x) {
	while(x->parent != x && x->parent != NULL) {
		x = x->parent;
	}
	return x;
}

uf_elem_t uf_find(uf_elem_t x) {
	if (x->parent == x) return x;

	uf_elem_t key = uf_find(x->parent);
	x->parent = key;
	return key;
}

void uf_union(uf_elem_t a, uf_elem_t b) {
	if(a->rank < b->rank) return uf_union(b, a);

	uf_elem_t rep_a = uf_find(a);
	uf_elem_t rep_b = uf_find(b);

	rep_b->parent = rep_a;
	rep_b->rank = rep_a->rank + rep_b->rank;
}

int main() {
	uf_partition_t ex = exemple();
	print_uf_partition_verbose(ex, 11);

	int elem = uf_find(ex[1])->elem;
	printf("=> %d", elem);

	printf("\n\n");
	print_uf_partition_verbose(ex, 11);
	printf("\n\n\n");

	ex = exemple();

	uf_union(ex[10], ex[1]);
	print_uf_partition_verbose(ex, 11);
}


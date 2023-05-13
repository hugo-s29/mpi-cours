#include<stdio.h>
#include<stdlib.h>


struct matrice_s {
	int nbl;
	int nbc;
	int* tab;
};

typedef struct matrice_s* matrice;

void export(char* filename, matrice m) {
	FILE* f = fopen(filename, "w");
	fprintf(f, "%d\n%d\n", m->nbl, m->nbc);

	int k = 0;
	for(int j = 0; j < m->nbc; ++j) {
		for(int i = 0; i < m->nbl; ++i) {
			fprintf(f, "%d ", m->tab[k++]);
		}
		fprintf(f, "\n");
	}

	fclose(f);
}

matrice import(char* filename) {
	FILE* f = fopen(filename, "r");
	int nbl, nbc;

	fscanf(f, "%d\n", &nbl);
	fscanf(f, "%d\n", &nbc);

	int* tab = malloc(sizeof(int) * nbl * nbc);
	matrice m = malloc(sizeof(struct matrice_s));

	m->tab = tab;
	m->nbl = nbl;
	m->nbc = nbc;

	int k = 0;
	for(int j = 0; j < nbc; ++j) {
		for(int i = 0; i < nbl; ++i) {
			fscanf(f, "%d ", tab + (k++));
		}
		fscanf(f, "\n");
	}



	return m;
}

//int main() {
//	int mat[] = { 1, 2, 3, 4, 2, 3, 4, 5, 3, 4, 5, 6 };
//	struct matrice_s m = { .tab = mat, .nbl = 4, .nbc = 3 };
//
//	export("matrix-data", &m);
//
//	matrice m2 = import("matrix-data");
//	export("matrix-data2", m2);
//}

int main(int argc, char* argv[]) {
	if (argc != 3) {
		printf("usage : ./exec filename_src filename_dst");
    return 1;
  } else {
		matrice m = import(argv[1]);
		for (int i = 0 ; i < m->nbc * m->nbl; i++) {m->tab[i]++;}
		export(argv[2], m);
		return 0;
	}
}

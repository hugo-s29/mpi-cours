#include<stdio.h>
#include<stdlib.h>
#include<stdbool.h>
#include<assert.h>
#include<time.h>
#include<pthread.h> //penser à compiler avec -pthread



struct matrice_s {
	int nbl;  //nombre de lignes
	int nbc;  //nombre de colonnes
	int** tab;//tableau de nbl tableaux de nbc coeffs
};

typedef struct matrice_s* matrice;



matrice create(int n,int m){
  //cree une matrice à n lignes et m colonnes, "vide"
  matrice res = malloc(sizeof(struct matrice_s));
  res->nbl = n;
  res->nbc = m;
  res->tab = malloc(sizeof(int*)*n);
  for(int i=0; i<n ; i++){
    res->tab[i] = malloc(sizeof(int*)*m);
  }
  return res;
}




void libere_matrice(matrice m){
	for(int i=0; i < m->nbl; i++){
		free(m->tab[i]);
	}
	free(m->tab);
	free(m);
}



void affiche_matrice (matrice m){
	for(int j=0; j < m->nbc; j++){printf("------");};
	printf("-\n");
	for(int i=0; i < m->nbl; i++){
		for(int j=0; j < m->nbc; j++){
			printf("%d\t", m->tab[i][j]);
		}
		printf("\n");
	}
	for(int j=0; j < m->nbc; j++){printf("------");};
	printf("-\n");
}

struct product_data_s {
	int i, j;
	matrice a, b, c;
};

typedef struct product_data_s* product_data;


matrice produit(matrice a, matrice b) {
	assert(a->nbc == b->nbl);
	matrice c = create(a->nbl, b->nbc);
	for (int i = 0; i < a->nbl; ++i) {
		for (int j = 0; j < b->nbc; ++j) {
			int sum = 0;
			for (int k = 0; k < a->nbc; ++k) {
				sum += a->tab[i][k] * b->tab[k][j];
			}
			c->tab[i][j] = sum;
		}
	}
	return c;
}

void* produit_process(void* raw_data) {
	product_data data = (product_data)raw_data;

	int i = data->i, j = data->j;
	matrice a = data->a, b = data->b, c = data->c;

	int sum = 0;
	for (int k = 0; k < a->nbc; ++k) {
		sum += a->tab[i][k] * b->tab[k][j];
	}
	c->tab[i][j] = sum;

	return NULL;
}

matrice produit_parallele(matrice a, matrice b) {
	assert(a->nbc == b->nbl);
	matrice c = create(a->nbl, b->nbc);

	product_data data = malloc(sizeof(struct product_data_s) * a->nbl * b->nbc);
	pthread_t* threads = malloc(sizeof(pthread_t) * a->nbl * b->nbc);

	int k = 0;
	for (int i = 0; i < a->nbl; ++i) {
		for (int j = 0; j < b->nbc; ++j) {
			data[k].i = i;
			data[k].j = j;
			data[k].a = a;
			data[k].b = b;
			data[k].c = c;

			pthread_create(threads + k, NULL, produit_process, (void*)(data + k));
			k++;
		}
	}

	for (k = 0; k < a->nbl * b->nbc; ++k) {
		pthread_join(threads[k], NULL);
	}

	free(data);
	free(threads);

	return c;
}



int main(){
  srand(time(NULL));
  for (int k = 0; k < 100; k++) {
    int t0 = time(NULL);
    for (int u = 0; u < 100; u++) {
      matrice m1 = create(100,100);
      for(int i = 0; i < 100; i++) {
	for(int j = 0; j < 100; j++) {
	  m1->tab[i][j] = rand() % 1000;
	}
      }

      matrice m2 = create(100,100);
      for(int i = 0; i < 100; i++) {
	for(int j = 0; j < 100; j++) {
	  m1->tab[i][j] = rand() % 1000;
	}
      }

      matrice prod = produit_parallele(m1,m2);
      libere_matrice(m1);
      libere_matrice(m2);
      libere_matrice(prod);
    }

    int t1 = time(NULL);

    for (int u = 0; u < 100; u++) {
      matrice m1 = create(100,100);
      for(int i = 0; i < 100; i++) {
	for(int j = 0; j < 100; j++) {
	  m1->tab[i][j] = rand() % 1000;
	}
      }

      matrice m2 = create(100,100);
      for(int i = 0; i < 100; i++) {
	for(int j = 0; j < 100; j++) {
	  m1->tab[i][j] = rand() % 1000;
	}
      }

      matrice prod = produit(m1,m2);
      libere_matrice(m1);
      libere_matrice(m2);
      libere_matrice(prod);
    }

    int t2 = time(NULL);

    printf("%d%% => // -> %d, -- -> %d\n", k, t1 - t0, t2 - t1);
  }
  return 0;
}

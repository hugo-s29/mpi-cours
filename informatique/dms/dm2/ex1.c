#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#define NBCOL 784

struct vector_s {
	int size;
	unsigned char* content;
};

typedef struct vector_s* vector;

int max_class_count; // defined later in the file

vector create_zero_vector(int n) {
	vector v = malloc(sizeof(struct vector_s));
	assert(v != NULL);

	v->size = n;
	v->content = malloc(n * sizeof(unsigned char));
	assert(v->content != NULL);

	for(int i = 0; i < n; ++i) {
		v->content[i] = 0;
	}

	return v;
}

void delete_vector(vector v) {
	free(v->content);
	free(v);
}

void print_vector(vector v) {
	printf("(");
	for(int i = 0; i < v->size; ++i) {
		if (i == 0)
			printf("%d", v->content[0]);
		else
			printf(",%d", v->content[i]);
	}
	printf(")");
}

double distance(vector u, vector v) {
	assert(u->size == v->size);

	double sum = 0;
	for(int i = 0; i < u->size; ++i) {
		double dx = u->content[i] - v->content[i];
		sum += dx*dx;
	}

	return sum;//sqrt(sum);
}

struct classified_data_s {
	vector vec;
	int class;
};

struct database_s {
	int size;
	struct classified_data_s* dataset;
};

typedef struct database_s* database;

database create_empty_database(int n) {
	database db = malloc(sizeof(struct database_s));
	assert(db != NULL);

	db->size = n;
	db->dataset = malloc(n * sizeof(struct classified_data_s));
	assert(db->dataset != NULL);

	return db;
}

void delete_database(database db) {
	free(db->dataset);
	free(db);
}

void print_database(database db) {
	printf("{ ");
	
	for(int i = 0; i < db->size; ++i) {
		if(i > 0) printf(", ");

		struct classified_data_s* data = db->dataset + i;
		print_vector(data->vec);
		printf(" ~> %d", data->class);
	}

	printf(" }");
}

typedef struct cell_s* candidates;
struct cell_s {
	int index;
	double distance;
	candidates next;
};

candidates create_list(int ind, double dist) {
	candidates c = malloc(sizeof(struct cell_s));
	assert(c != NULL);
	c->index = ind;
	c->distance = dist;
	c->next = NULL;
	return c;
}

void delete_list(candidates list) {
	candidates current = list;

	while(current != NULL) {
		candidates next = current->next;
		free(current);
		current = next;
	}
}

void print_list(candidates list) {
	printf("[ ");
	candidates current = list;

	while(current != NULL) {
		if(current != list) printf(", ");

		candidates next = current->next;
		printf("{ i : %d, d: %f }", current->index, current->distance);
		current = next;
	}

	printf(" ]");
}

int insertion_list(candidates* list, int r, int k, database db, int i, vector input) {
	double dist = distance(db->dataset[i].vec, input);
	candidates c = create_list(i, dist);

	if(*list == NULL) { // empty list
		*list = c;
		return 1;
	}

	if(dist > (*list)->distance) { // closer than the first one
		c->next = *list;
		*list = c;
	} else {
		// find input's distance in the list
		candidates prev = *list;
		candidates curr = prev->next;
		while (curr != NULL && dist < curr->distance) {
			prev = curr;
			curr = prev->next;
		}
		
		// append input
		prev->next = c;
		c->next = curr;
	}

	if (r < k) {
		return r + 1;
	}

	// cut the end of the list if needed
	int to_remove = (r + 1) - k;
	for (int j = 0; j < to_remove; ++j) {
		candidates temp = *list;
		*list = temp->next;
		free(temp);
	}

	return k;
}

candidates nearest(database db, int k, vector input) {
	if(db->size == 0) return NULL;

	int r = 0;
	candidates list = NULL;
	for(int i = 0; i < db->size; ++i) {
		r = insertion_list(&list, r, k, db, i, input);
	}

	return list;
}

int majority_class(database db, candidates list, int r) {
	int* counts = malloc(max_class_count * sizeof(int));

	for(int i = 0; i < r; ++i) {
		int index = list->index;
		int class = db->dataset[index].class;
		counts[class]++;
		list = list->next;
	}

	int max = 0;
	for(int j = 0; j < max_class_count; ++j)
		if(counts[j] > counts[max]) max = j;

	return max;
}

int classify(database db, int k, vector input) {
	candidates c = nearest(db, k, input);
	return majority_class(db, c, k);
}

/// Using the 'quadrants' database

/*
int max_class_count = 4;

int quadrant(vector v) {
	int x = v->content[0];
	int y = v->content[1];

	if(x <= 127)
		if(y <= 127) return 0;
		else return 1;
	else
		if(y <= 127) return 2;
		else return 3;
}

database make_dataset(int db_size) {
	database db = create_empty_database(db_size);
	
	for(int i = 0; i < db_size; ++i) {
		vector vec = create_zero_vector(2);
		vec->content[0] = rand() % 256;
		vec->content[1] = rand() % 256;
		db->dataset[i].vec = vec;
		db->dataset[i].class = quadrant(vec);
	}

	return db;
}

void main() {
	srand(time(NULL));
	database db = make_dataset(400);

	int k = 1;
	int mat[4][4] = { {0,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,0} };

	for(int i = 0; i < db->size; ++i) {
		vector vec = db->dataset[i].vec;
		int expected = db->dataset[i].class;
		int returned = classify(db, k, vec);

		mat[expected][returned]++;
	}

	for(int i = 0; i < 4; ++i) {
		for(int j = 0; j < 4; ++j) {
			if(j > 0) printf("|");

			if(mat[i][j] < 10)
				printf("  %d", mat[i][j]);
			else if(mat[i][j] < 100)
				printf(" %d", mat[i][j]);
			else
				printf("%d", mat[i][j]);
		}
		printf("\n");
	}
}
*/

/// Using the MNIST database

int max_class_count = 10;
void mnist_input(int n, database *p_db_train, int m, database *p_db_test);


void main() {
	database db_train = NULL;
	database db_test = NULL;
	int test_size = 1000; int train_size = 10000;
	mnist_input(train_size, &db_train, test_size, &db_test);

	int mat[10][10];

	int k = 3;

	for(int i = 0; i < 10; ++i)
		for(int j = 0; j < 10; ++j)
			mat[i][j] = 0;

	for(int i = 0; i < test_size; ++i) {
		vector vec = db_test->dataset[i].vec;
		unsigned char expected = db_test->dataset[i].class;
		unsigned char returned = classify(db_train, k, vec);

		mat[expected][returned]++;
	}

	for(int i = 0; i < 10; ++i) {
		for(int j = 0; j < 10; ++j) {
			if(j > 0) printf("|");

			if(mat[i][j] < 10)
				printf("  %d", mat[i][j]);
			else if(mat[i][j] < 100)
				printf(" %d", mat[i][j]);
			else
				printf("%d", mat[i][j]);
		}
		printf("\n");
	}
}

int int_of_char(char c) {
  switch (c) {
  case '0': return 0;
  case '1': return 1;
  case '2': return 2;
  case '3': return 3;
  case '4': return 4;
  case '5': return 5;
  case '6': return 6;
  case '7': return 7;
  case '8': return 8;
  default:  return 9;
  }
}

unsigned char next_char(FILE* f) {
  unsigned char partial = 0;
  char c = fgetc(f);
  while (true) {
    if (c == ',' || c == '\n') {
      return partial;
    } else {
      partial = partial * 10 + int_of_char(c);
      c = fgetc(f);
    }
  }
}

void mnist_input(int n, database *p_db_train, int m, database *p_db_test) {
  // assumes the file `/MNIST_train.txt' is present
	// creates a training database with the n first lines of the file
	// and stores this in `*p_db_train'
	// creates a testing database with the m first lines of the file
	// and stores this in `*p_db_test'
  FILE *f = fopen("./MNIST-txt/MNIST_train.txt", "r");
  if (f == NULL) {
    printf("the file ./MNIST-txt/MNIST_train.txt couldn't be opened\n");
  }
  database db_train = create_empty_database(n);
  database db_test = create_empty_database(m);
  int class;
  for (int j = 0; j < n; j++) {
    class = next_char(f);
    vector v = create_zero_vector(NBCOL);
    for (int i = 0; i < NBCOL; i++) {
      unsigned char c = next_char(f);
      v->content[i] = c;
    }
    (db_train->dataset)[j].vec = v;
    (db_train->dataset)[j].class = class;
  }
  for (int j = 0; j < m; j++) {
    class = next_char(f);
    vector v = create_zero_vector(NBCOL);
    for (int i = 0; i < NBCOL; i++) {
      unsigned char c = next_char(f);
      v->content[i] = c;
    }
    (db_test->dataset)[j].vec = v;
    (db_test->dataset)[j].class = class;
  }
  fclose(f);
  *p_db_test = db_test;
  *p_db_train = db_train;
}

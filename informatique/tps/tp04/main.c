#include<stdio.h>
#include<stdbool.h>
#include<stdlib.h>
#include<assert.h>

typedef int* solution;

void printBoard(solution sol, int part, int n);
bool isExtValid(solution sol, int n, int col);
bool canBeCompleted(solution sol, int nbl, int n);
void deterministicSolve(int n);
int validPositions(solution sol, int nbl, int n, int* posTab);

int main() {
	int sol[] = { 1, 3, -1, -1 };
	printBoard(sol, 4, 4);

	int pos[4];

	int k = validPositions(sol, 3, 4, pos);

	printf("%d", k);

	return 0;
}

int abs(int n) { return n >= 0 ? n : -n; }

void printBoard(solution sol, int part, int n) {
	for(int i = 0; i < n; ++i) {
		printf("%d\t", i);
		for(int j = 0; j < n; ++j) {
			printf(i < part && sol[i] == j ? "@ " : ". ");
		}
		printf("\n");
	}
}

bool isExtValid(solution sol, int nbl, int col) {
	for(int i = 0; i < nbl; ++i) {
		// lines
		if(sol[i] == col) return false;

		// diagonals : |dx| = |dy|
		if(abs(nbl - i) == abs(sol[i] - col)) return false;
	}

	return true;
}

bool canBeCompleted(solution sol, int nbl, int n) {
	if(nbl == n) return true;

	for(int j = 0; j < n; j++) {
		if(isExtValid(sol, nbl, j)) {
			sol[nbl] = j;
			if(canBeCompleted(sol, nbl + 1, n))
				return true;
		}
	}

	return false;
}

void deterministicSolve(int n) {
	solution sol = malloc(n * sizeof(int));
	assert(sol != NULL);

	for(int i = 0; i < n; i++)
		sol[i] = -1;

	if(canBeCompleted(sol, 0, n))
		printBoard(sol, n, n);

	free(sol);
}

int validPositions(solution sol, int nbl, int n, int* posTab) {
	int k = 0;
	for(int i = 0; i < n; ++i)
		if(isExtValid(sol, nbl, i))
			posTab[k++] = i;
	
	return k;
}


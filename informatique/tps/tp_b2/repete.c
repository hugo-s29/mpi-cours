#include <stdio.h>
#include <stdlib.h>

int main(int argc, char* argv[]) {
	if (argc == 3) {
		int n = atoi(argv[1]);
		char* s = argv[2];

		for(int i = 0; i < n; ++i) {
			printf("%s ", s);
		}
		printf("\n");
	} else {
		printf("usage : %s nb str repete nb fois la chaîne de caractères", argv[0]);
	}
}

#ifndef MAIN_H_
#define MAIN_H_

#include<time.h>

#include "compagnon_laby.h"
#include "linked_list.h"
#include "unionfind.h"

void rec_generator(laby_t laby, bool* visited, int i, int j);
void generate_laby(laby_t laby);
void generate_laby_non_rec(laby_t laby);

struct mur_s {
  int i, j;
  int i2, j2;
};

typedef struct mur_s mur_t;

mur_t* tab_murs_laby_plein(laby_t laby);
void melange_liste_mur(mur_t* murs, int n);
void uf_generate_laby(laby_t laby);
bool rec_solver(laby_t laby, bool* visited, int i, int j);
bool* solve_labyrinthe(laby_t laby);
void repare(laby_t laby);

#endif // MAIN_H_

#ifndef COMPAGNON_LABY_H_
#define COMPAGNON_LABY_H_

#include<stdlib.h>
#include<stdio.h>
#include<stdbool.h>

struct laby_s {
  int   width  ;                /* largeur du labyrinthe                                     */
  int   height ;                /* hauteur du labyrinthe                                     */
  char* cells  ;                /* tableau indiquant le contenu de chaque case du labyrinthe */
};

typedef struct laby_s laby_t;

/* Description des types de cases.
   ? indique une information présente dans une autre case,
   | ou - indique un mur (vertical ou horizontal)

   De type 0 :
   +?+
   ?
   + +

   De type 1 :
   +?+
   ? |
   + +

   De type 2 :
   +?+
   ?
   +-+

   De type 3 :
   +?+
   ? |
   +-+

   Ainsi entre deux cases a et b de coordonnées (i,j) et (i,j+1), ie l'une à côté de l'autre avec a à gauche,
   il y a un mur ssi la case a est de type 1 ou 3, soit de type impair.
   Entre deux cases a et b de coordonnées (i,j) et (i+1,j), ie l'une au dessus de l'autre avec a au dessus,
   il y a un mur ssi la case a est de type 2 ou 3, soit de type de quotient modulo 2 égal à 1.
*/

void draw_laby(laby_t laby);
void draw_laby_with_visited(laby_t laby, bool* visited);
bool is_laby_plein(laby_t laby);
int linearise(laby_t laby, int i, int j);
void delinearise(laby_t laby, int x, int* pi, int* pj);
bool is_in_laby(laby_t laby, int i, int j);
bool can_go_from(laby_t laby, int i1, int j1, int i2, int j2);
void casse_mur(laby_t laby, int i1, int j1, int i2, int j2);

#endif // COMPAGNON_LABY_H_

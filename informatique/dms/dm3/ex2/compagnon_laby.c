#include "compagnon_laby.h"

void draw_laby(laby_t laby) {
	//affiche le labyrinthe laby avec des caractères ASCII
  for (int j = 0 ; j < laby.width ; j ++) {
    printf("+---");
  }
  printf("+\n");
  for (int i = 0 ; i < laby.height-1 ; i ++) {
    printf("|   ");
    for (int j = 0 ; j < laby.width-1 ; j ++) {
      int c = laby.cells[i*laby.width+j];
      if (c % 2 == 1) {printf("|   ");}
      else {printf("    ");}
    }
    printf("|\n");
    printf("+");
    for (int j = 0 ; j < laby.width ; j ++) {
      int c = laby.cells[i*laby.width+j];
      if ((c/2) % 2 == 1) {printf("---+");}
      else {printf("   +");}
    }
    printf("\n");
  }
  printf("|   ");
  for (int j = 0 ; j < laby.width-1 ; j ++) {
    int c = laby.cells[(laby.height-1)*laby.width+j];
    if (c % 2 == 1) {printf("|   ");}
    else {printf("    ");}
  }
  printf("|\n");
  printf("+");
  for (int j = 0 ; j < laby.width ; j ++) {
    printf("---+");
  }
  printf("\n");
}





void draw_laby_with_visited(laby_t laby, bool* visited) {
	//affiche le labyrinthe laby et le chemin décrit par visited en ASCII
  for (int j = 0 ; j < laby.width ; j ++) {
    printf("+-");
  }
  printf("+\n");
  for (int i = 0 ; i < laby.height-1 ; i ++) {
    printf("|");
    for (int j = 0 ; j < laby.width-1 ; j ++) {
      if (visited[i*laby.width+j]) {printf(".");}
      else {printf(" ");}
      int c = laby.cells[i*laby.width+j];
      if (c % 2 == 1) {printf("|");}
      else {
        if (visited[i*laby.width+j] && visited[i*laby.width+j+1]) {printf(".");}
        else {printf(" ");}
      }
    }
    if (visited[i*laby.width+laby.width-1]) {printf(".");}
    else {printf(" ");}
    printf("|\n");
    printf("+");
    for (int j = 0 ; j < laby.width ; j ++) {
      int c = laby.cells[i*laby.width+j];
      if ((c/2) % 2 == 1) {printf("-+");}
      else {
        if (visited[i*laby.width+j] && visited[(i+1)*laby.width+j]) {printf(".+");}
        else {printf(" +");}
      }
    }
    printf("\n");
  }
  printf("|");
  for (int j = 0 ; j < laby.width-1 ; j ++) {
    if (visited[(laby.height-1)*laby.width+j]) {printf(".");}
    else {printf(" ");}
    int c = laby.cells[(laby.height-1)*laby.width+j];
    if (c % 2 == 1) {printf("|");}
    else {
      if (visited[(laby.height-1)*laby.width+j] && visited[(laby.height-1)*laby.width+j+1]) {printf(".");}
      else {printf(" ");}
    }
  }
  if (visited[(laby.height-1)*laby.width+laby.width-1]) {printf(".");}
  printf("|\n");
  printf("+");
  for (int j = 0 ; j < laby.width ; j ++) {
    printf("-+");
  }
  printf("\n");
}


bool is_laby_plein(laby_t laby) {
	//teste si le labyrinthe laby est plein
  for(int k=0; k < laby.height; k++){
  	if (laby.cells[k] != 3) return false;
  	//3 est le type des cases ac mur sud et mur est
  } 
  return true;
}


int linearise(laby_t laby, int i, int j) { 
  //indice ds le tableau unidimensionnel de la case de coordonnées (i,j)
	//dans le tableau bidimensionnel aux dimensions de laby
	return i * laby.width + j ; 
}

void delinearise(laby_t laby, int x, int* pi, int* pj) {
	//modifie les valeurs de *pi et *pj pour qu'ils enregistrent les
	//coordoonées dans le tableau bidimensionnel aux dimensions de laby *
	//de la case d'indice x dans le tableau  unidimensionnel correspondant
  *pi = x / laby.width;
  *pj = x % laby.width;
}


bool is_in_laby(laby_t laby, int i, int j) {
  return i >= 0 && j >= 0 && i < laby.height && j < laby.width;
  // teste si la case de coordonnées (i,j) est dans laby
}

bool can_go_from(laby_t laby, int i1, int j1, int i2, int j2) {	
	//hypothèse : is_in_laby(laby,i1,j1) && is_in_laby(laby,i2,j2)
	//teste l'abscence de mur entre les cases (i1,j1) et (i2,j2) ds laby 
  if      (i1 == i2     && j2 == j1 + 1 ) //1 à gauche, 2 à droite
  	{return laby.cells[linearise (laby, i1, j1 )] % 2 == 0;     }
  else if (i1 == i2     && j2 == j1 - 1 ) //2 à gauche, 1 à droite
  	{return laby.cells[linearise (laby, i2, j2 )] % 2 == 0;     }
  else if (i2 == i1 + 1 && j1 == j2     ) //1 au dessus, 2 en dessous
  	{return laby.cells[linearise (laby, i1, j1 )] / 2 % 2 == 0; }
  else if (i1 == i2 + 1 && j1 == j2     ) //2 au dessus, 1 en dessous
  	{return laby.cells[linearise (laby, i2, j2 )] / 2 % 2 == 0; }
  else {return false;}
}



void casse_mur(laby_t laby, int i1, int j1, int i2, int j2) {
	//hypothèse : is_in_laby(laby,i1,j1) && is_in_laby(laby,i2,j2)
	//casse le mur entre la case (i1,j1) et la case (i2,j2) dans laby
  if      (i1 == i2   && j2 == j1 + 1 ) 
  	{laby.cells[linearise (laby, i1, j1 )] = (laby.cells[linearise (laby, i1, j1 )] / 2 ) * 2;}
  else if (i1 == i2   && j2 == j1 - 1 ) 
  	{laby.cells[linearise (laby, i2, j2 )] = (laby.cells[linearise (laby, i2, j2 )] / 2 ) * 2;}
  else if (i2 == i1+1 && j1 == j2     ) 
  	{laby.cells[linearise (laby, i1, j1 )] =  laby.cells[linearise (laby, i1, j1 )] % 2;}
  else if (i1 == i2+1 && j1 == j2     ) 
  	{laby.cells[linearise (laby, i2, j2 )] =  laby.cells[linearise (laby, i2, j2 )] % 2;}
}


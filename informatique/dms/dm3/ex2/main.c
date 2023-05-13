#include "main.h"

void generate_laby(laby_t laby) {
  bool* visited = malloc(laby.width * laby.height * sizeof(bool));

  rec_generator(laby, visited, 0, 0);

  free(visited);
}

void rec_generator(laby_t laby, bool* visited, int i, int j) {
  visited[linearise(laby, i, j)] = true;

  bool available[] = {
    is_in_laby(laby, i + 1, j) && !visited[linearise(laby, i + 1, j)],
    is_in_laby(laby, i - 1, j) && !visited[linearise(laby, i - 1, j)],
    is_in_laby(laby, i, j + 1) && !visited[linearise(laby, i, j + 1)],
    is_in_laby(laby, i, j - 1) && !visited[linearise(laby, i, j - 1)],
  };

  int di[] = {1, -1, 0, 0};
  int dj[] = {0, 0, 1, -1};

  bool has_available = available[0] || available[1] || available[2] || available[3];

  while (has_available) {
    // Choix d'un mur aléatoire
    int random = rand() % 4;
    while(!available[random]) random = rand() % 4;

    available[random] = false;

    int i2 = i + di[random], j2 = j + dj[random];

    // Visite de la case voisine
    casse_mur(laby, i, j, i2, j2);
    rec_generator(laby, visited, i2, j2);

    // Recalcul des cases voisines disponibles
    for(int k = 0; k < 4; ++k)
      if(available[k])
        available[k] = is_in_laby(laby, i + di[k], j + dj[k])
          && !visited[linearise(laby, i + di[k], j + dj[k])];

    has_available = available[0] || available[1] || available[2] || available[3];
  }
}


// cette fonction ne marche pas...
// essayer de la compléter
void generate_laby_non_rec(laby_t laby) {
  bool* visited = malloc(laby.width * laby.height * sizeof(bool));
  linked_list_t stack = create_list();

  // la pile `stack` contient des entiers de la forme
  // i * max_index + j, où i et j sont des indices dans
  // dans le tableau des cases du labyrinthe.
  // i représente l'indice précédent, j représente l'indice
  // de la case suivante.

  int max_index = laby.width * laby.height;

  printf("OK ?\n\n\n");

  push_element(stack, -max_index);

  while(get_size(stack) > 0) {
    printf("OK ?\n\n\n");
    int index = pop_element(stack)->value;
    printf("OK ?\n\n\n");

    int index1 = index % max_index;
    int index2 = index / max_index;

    printf("Index %d -> (%d, %d)\n", index, index1, index2);

    int i, j;
    int i_prev, j_prev;

    delinearise(laby, index1, &i, &j);
    delinearise(laby, index2, &i_prev, &j_prev);

    if(!is_in_laby(laby, i, j) || visited[linearise(laby, i, j)]) {
      continue;
    }

    if(is_in_laby(laby, i_prev, j_prev))
      casse_mur(laby, i_prev, j_prev, i, j);

    visited[linearise(laby, i, j)] = true;

    bool available[] = {
      is_in_laby(laby, i + 1, j) && !visited[linearise(laby, i + 1, j)],
      is_in_laby(laby, i - 1, j) && !visited[linearise(laby, i - 1, j)],
      is_in_laby(laby, i, j + 1) && !visited[linearise(laby, i, j + 1)],
      is_in_laby(laby, i, j - 1) && !visited[linearise(laby, i, j - 1)],
    };

    // offsets pour accéder aux cases voisines
    int di[] = {1, -1, 0, 0};
    int dj[] = {0, 0, 1, -1};

    bool has_available = available[0] || available[1] || available[2] || available[3];

    int current_cell_index = linearise(laby, i, j) * max_index;

    while (has_available) {
      // Choix d'un mur aléatoire
      int random = rand() % 4;
      while(!available[random]) random = rand() % 4;

      available[random] = false;

      int i2 = i + di[random], j2 = j + dj[random];
      int next_index = linearise(laby, i2, j2) + current_cell_index;

      push_element(stack, next_index);
    }
  }

  free(visited);
}

mur_t* tab_murs_laby_plein(laby_t laby) {
  int w = laby.width, h = laby.height;
  int nb = 2 * w * h - w - h;
  mur_t* murs = malloc(sizeof(mur_t) * nb);

  int k = 0;

  // murs verticaux
  for(int i = 0; i < w - 1; ++i) {
    for(int j = 0; j < h; ++j) {
      murs[k].i = i;
      murs[k].j = j;
      murs[k].i2 = i + 1;
      murs[k].j2 = j;
      k++;
    }
  }

  // murs horizontaux
  for(int i = 0; i < w; ++i) {
    for(int j = 0; j < h - 1; ++j) {
      murs[k].i = i;
      murs[k].j = j;
      murs[k].i2 = i;
      murs[k].j2 = j + 1;
      k++;
    }
  }

  return murs;
}

// `n` est la longueur du tableau `murs`
void melange_liste_mur(mur_t* murs, int n) {
  for(int i = n - 1; i >= 1; i--) {
    int j = rand() % (i + 1);
    mur_t temp = murs[i];
    murs[i] = murs[j];
    murs[j] = temp;
  }
}

void uf_generate_laby(laby_t laby) {
  mur_t* murs = tab_murs_laby_plein(laby);
  int n = 2 * laby.width * laby.height - laby.width - laby.height; // nombre de murs
  melange_liste_mur(murs, n);

  // création de la structure UnionFind
  uf_partition_t uf_laby = uf_initialise(n);
  int casses = 0; // nb de murs cassés
  int k = 0; // index dans le tableau `murs`

  while (casses < laby.width * laby.height - 1) {
    mur_t mur = murs[k++];

    int index_a = linearise(laby, mur.i, mur.j);
    int index_b = linearise(laby, mur.i2, mur.j2);

    uf_elem_t a = uf_laby[index_a];
    uf_elem_t b = uf_laby[index_b];

    // même classe, pas de mur entre eux
    if (uf_find(a) == uf_find(b)) continue;

    uf_union(a,b);
    casses++;
    casse_mur(laby, mur.i, mur.j, mur.i2, mur.j2);
  }
}

bool* solve_labyrinthe(laby_t laby) {
  bool* visited = malloc(sizeof(bool) * laby.width * laby.height);
  assert(visited != NULL);

  rec_solver(laby, visited, 0, 0);
  return visited;
}

bool rec_solver(laby_t laby, bool* visited, int i, int j) {
  if(i < 0 || j < 0 || j >= laby.width || i >= laby.height)
    return false;

  int k = linearise(laby, i, j);

  if(j == laby.width - 1 && i == laby.height - 1) {
    visited[k] = true;
    return true;
  }

  if(visited[k]) return false;

  if(laby.cells[k] == 0 || laby.cells[k] == 1) {
    visited[k] = true;

    if(rec_solver(laby, visited, i + 1, j))
      return true;
  }

  if(laby.cells[k] == 0 || laby.cells[k] == 2) {
    visited[k] = true;

    if(rec_solver(laby, visited, i, j + 1))
      return true;
  }

  if(j > 0) {
    int k2 = linearise(laby, i, j - 1);

    if(laby.cells[k2] == 0 || laby.cells[k2] == 2) {
      visited[k] = true;

      if(rec_solver(laby, visited, i, j - 1))
        return true;
    }
  }

  if(i > 0) {
    int k2 = linearise(laby, i - 1, j);

    if(laby.cells[k2] == 0 || laby.cells[k2] == 1) {
      visited[k] = true;

      if(rec_solver(laby, visited, i - 1, j))
        return true;
    }
  }

  visited[k] = false;
  return false;
}

// Modification de la fonction `rec_solver`
bool rec_repare_ajout(laby_t laby, bool* visited, int i, int j, int iprev, int jprev) {
  if(i < 0 || j < 0 || j >= laby.width || i >= laby.height)
    return false;

  int k = linearise(laby, i, j);

  printf("(%d, %d) -> (%d, %d) with type %d\n", iprev, jprev, i, j, laby.cells[k]);

  if(visited[k] && !(i == iprev && j == jprev)) {
    // ajout du mur entre (i, j) et (iprev, jprev)

    if(abs(i - iprev) == 1) {
      int k = i - iprev < 0
        ? linearise(laby, iprev, jprev) // au dessus
        : linearise(laby, i, j); // en dessous

      if (laby.cells[k] == 0)
        laby.cells[k] = 2;
      if (laby.cells[k] == 1)
        laby.cells[k] = 3;
    } else if(abs(j - jprev) == 1) {
      int k = j - jprev < 0
        ? linearise(laby, iprev, jprev) // à droite
        : linearise(laby, i, j); // à gauche

      if (laby.cells[k] == 0)
        laby.cells[k] = 1;
      if (laby.cells[k] == 2)
        laby.cells[k] = 3;
    }

    visited[k] = false;

    return false;
  }

  visited[k] = true;

  if(laby.cells[k] == 0 || laby.cells[k] == 1) {
    if(rec_repare_ajout(laby, visited, i + 1, j, i, j))
      return true;
  }

  if(laby.cells[k] == 0 || laby.cells[k] == 2) {
    if(rec_repare_ajout(laby, visited, i, j + 1, i, j))
      return true;
  }

  if(j > 0) {
    int k2 = linearise(laby, i, j - 1);

    if(laby.cells[k2] == 0 || laby.cells[k2] == 2) {
      if(rec_repare_ajout(laby, visited, i, j - 1, i, j))
        return true;
    }
  }

  if(i > 0) {
    int k2 = linearise(laby, i - 1, j);

    if(laby.cells[k2] == 0 || laby.cells[k2] == 1) {
      if(rec_repare_ajout(laby, visited, i - 1, j, i, j))
        return true;
    }
  }

  return false;
}

void repare(laby_t laby) {
  // => Ajout des murs

  // Pour ajouter les murs, on réalise un parcours en profondeur.
  // Si on veut se rendre sur une case déjà visitée, on place un
  // mur entre la case courante, et la suivante.

  bool* visited = malloc(sizeof(bool) * laby.width * laby.height);
  assert(visited != NULL);

  for(int i = 0; i < laby.height; ++i)
    for(int j = 0; j < laby.width; ++j)
      rec_repare_ajout(laby, visited, i, j, i, j);

  draw_laby_with_visited(laby, visited);
  draw_laby(laby);

  // => Suppression des murs avec UnionFind
  
  // On crée une structure UnionFind contenant les indices des cases.
  // On compte le nombre N de composantes connexes, et on réalise N
  // unions entre deux classes différentes. On supprime les murs entre
  // les cases des représentants.
}

int main() {
  srand(time(NULL));

  char cells[10 * 10];
  for(int i = 0; i < 10 * 10; ++i) cells[i] = 3;

  laby_t laby = { 10, 10, cells };

  /* // Sous-section 1 du DM
   *
   * generate_laby(laby);
   * draw_laby(laby);
   */

  /* // Sous-section 2 du DM
   *
   * uf_generate_laby(laby);
   * draw_laby(laby);
   */

  /* // Sous-section 3 du DM
   * generate_laby(laby);
   * draw_laby(laby);
   * bool* visited = solve_labyrinthe(laby);
   * draw_laby_with_visited(laby, visited);
   */

  for(int i = 0; i < 10 * 10; ++i) cells[i] = rand() % 4;
  draw_laby(laby);
  repare(laby);
}

#ifndef LINKED_LIST_H_
#define LINKED_LIST_H_

#include<stdlib.h>
#include<assert.h>

struct linked_list_node_s {
  struct linked_list_node_s* parent;
  struct linked_list_node_s* next;
  int value;
};

typedef struct linked_list_node_s* linked_list_node_t;

struct linked_list_s {
  linked_list_node_t head;
  linked_list_node_t tail;
  int size;
};

typedef struct linked_list_s* linked_list_t;

linked_list_t create_list();
linked_list_node_t get_head(linked_list_t l);
linked_list_node_t get_tail(linked_list_t l);
int get_size(linked_list_t l);
linked_list_node_t get_element(linked_list_t l, int i);
linked_list_node_t insert_at(linked_list_t l, int i, int value);
linked_list_node_t push_element(linked_list_t l, int value);
linked_list_node_t pop_element(linked_list_t l);
linked_list_node_t remove_at(linked_list_t l, int i);
void free_list(linked_list_t l);

#endif // LINKED_LIST_H_

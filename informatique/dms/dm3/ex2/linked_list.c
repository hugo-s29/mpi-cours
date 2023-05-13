#include "linked_list.h"

linked_list_t create_list() {
  linked_list_t list = malloc(sizeof(struct linked_list_s));
  assert(list != NULL);

  list->head = NULL;
  list->tail = NULL;
  list->size = 0;
  return list;
}

linked_list_node_t get_head(linked_list_t l) { return l->head; }
linked_list_node_t get_tail(linked_list_t l) { return l->tail; }
int get_size(linked_list_t l) { return l->size; }

linked_list_node_t get_element(linked_list_t l, int i) {
  if(i > get_size(l)/2) { // start from tail
    linked_list_node_t node = get_tail(l);
    for (int k = 0; k < i; ++k) node = node->parent;
    return node;
  } else { // start from head
    linked_list_node_t node = get_head(l);
    for (int k = 0; k < i; ++k) node = node->next;
    return node;
  }
}

linked_list_node_t insert_at(linked_list_t l, int i, int value) {
  linked_list_node_t node = malloc(sizeof(struct linked_list_node_s));
  assert(node != NULL);

  node->value = value;

  if(i == 0) {
    linked_list_node_t prev_head = get_head(l);
    l->head = node;
    node->next = prev_head;
    node->parent = NULL;
    if(l->size == 0) l->tail = node;
    l->size++;
    return node;
  }

  if(i == get_size(l)) {
    linked_list_node_t tail = get_tail(l);
    node->next = NULL;
    node->parent = tail;
    tail->next = node;
    if(l->size == 0) l->head = node;
    l->size++;
    return node;
  }

  linked_list_node_t prev_node = get_element(l, i - 1);
  linked_list_node_t next_node = prev_node->next;
  prev_node->next = node;
  node->next = next_node;
  node->parent = prev_node;
  next_node->parent = node;
  l->size++;

  return node;
}

linked_list_node_t push_element(linked_list_t l, int value) {
  return insert_at(l, get_size(l), value);
}

linked_list_node_t pop_element(linked_list_t l) {
  linked_list_node_t tail = get_tail(l);
  linked_list_node_t prev_tail = tail->parent;
  if(prev_tail == NULL) {
    l->head = NULL;
    l->tail = NULL;
    l->size = 0;
    return tail;
  }
  l->tail = prev_tail;
  l->size--;
  prev_tail->next = NULL;
  return tail;
}

linked_list_node_t remove_at(linked_list_t l, int i) {
  linked_list_node_t node = get_element(l, i);
  linked_list_node_t next = node->next, prev = node->parent;
  prev->next = next;
  next->parent = prev;
  return node;
}

void free_list(linked_list_t l) {
  linked_list_node_t node = get_head(l);
  while(node != NULL) {
    linked_list_node_t next = node->next;
    free(node);
    node = next;
  }
  free(l);
}

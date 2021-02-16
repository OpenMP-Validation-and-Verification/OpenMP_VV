//===--- test_taskgroup_task_reduction_device.c -----------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// This test checks the taskgroup construct with the task_reduction clause.
// It uses a linked list data structure which is then traversed and reduced
// over by a taskgroup. This test is based on a OpenMP 5.0 official example.
// This test checks the above in a target context.
//
////===----------------------------------------------------------------------===//
#include <stdlib.h>
#include <stdio.h>
#include <omp.h>
#include "ompvv.h"

#define N 128

typedef struct node_tag {
  int val;
  struct node_tag *next;
} node_t;

#pragma omp declare target
int linked_list_sum(node_t *p)
{
  int result = 0;
#pragma omp taskgroup task_reduction(+: result)
  {
    node_t* temp = p;
    while(temp != 0) {
#pragma omp task in_reduction(+: result)
      result += temp->val;
      temp = temp->next;
    }
  }
  return result;
}
#pragma omp end declare target


int seq_linked_list_sum(node_t *p)
{
  int result = 0;
  node_t* temp = p;
  while(temp != 0) {
    result += temp->val;
    temp = temp->next;
  }
  return result;
}


int main(int argc, char *argv[]) {
  OMPVV_TEST_OFFLOADING;

  int errors = 0, result = -1;
  node_t* root = (node_t*) malloc(sizeof(node_t));
  root->val = 1;

  node_t* temp = root;

  for(int i = 2; i <= N; ++i) {
    temp->next = (node_t*) malloc(sizeof(node_t));
    temp = temp->next;
    temp->val = i;
  }

  temp->next = 0;

#pragma omp target parallel shared(result) num_threads(OMPVV_NUM_THREADS_DEVICE) defaultmap(tofrom)
#pragma omp single
  {
    result = linked_list_sum(root);
  }

  OMPVV_TEST_AND_SET_VERBOSE(errors, result != seq_linked_list_sum(root));

  OMPVV_REPORT_AND_RETURN(errors);
}

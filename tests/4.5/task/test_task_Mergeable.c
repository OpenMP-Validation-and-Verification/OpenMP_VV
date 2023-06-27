//===-- test_task_Mergeable.c -------------------------------------------===//
//
// OpenMP API Version 4.5 Nov 2015
//
// Description
// Test case description: This test case demonstrates that mergeable clause
// combined with if clause
//===----------------------------------------------------------------------===//

#include <stdio.h>
#include <stdbool.h>
#include <unistd.h>
#include "omp.h"
#include "ompvv.h"
#define THREADS 64
#define MERGE 0  //  0 is for merge & 1 is for No merge


struct Node {
  int NodPos;
  int MasterThrd;
  int ExcutnThrd;
  struct Node *Nxt;
};


int main() {
  int errors = 0;
  omp_set_num_threads(THREADS);

  // Creating Linked list
  struct Node *Head = NULL;
  for (int i = 0; i < 10; ++i) {
    struct Node *temp = malloc(sizeof(struct Node));
    temp->Nxt = NULL;
    if (Head == NULL) {
      Head = temp;
    } else {
      struct Node *tmp = Head;
      while (tmp != NULL) {
        if (tmp->Nxt == NULL) {
          tmp->Nxt = temp;
          break;
        }
        tmp = tmp->Nxt;
      }
    }
  }


#pragma omp parallel
  {
    struct Node *Tmp = Head;
    int Count = 1;
#pragma omp single
    {
      int OutThrdId = omp_get_thread_num();
      while (Tmp != NULL) {
        Count++;
#pragma omp task mergeable if (MERGE) firstprivate(Count)
        {
          int IntThrdId = omp_get_thread_num();
          Tmp->NodPos = Count;
          Tmp->MasterThrd = OutThrdId;
          Tmp->ExcutnThrd = IntThrdId;
        }
        Tmp = Tmp->Nxt;
      }
    }
  }

  // verifying results
  struct Node *Tmp = Head;

  int FailCount = 0;
  while (Tmp != NULL) {
    if (Tmp->MasterThrd != Tmp->ExcutnThrd) {
      FailCount++;
    }
    Tmp = Tmp->Nxt;
  }
  Tmp = Head;
  struct Node *temp = Tmp;
  while (Tmp != NULL) {
    temp = Tmp;
    Tmp = Tmp->Nxt;
    free(temp);
  }

  OMPVV_TEST_AND_SET_VERBOSE(errors, (FailCount++ != 0));
  OMPVV_REPORT_AND_RETURN(errors);
}

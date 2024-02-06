//===================-test_parallel_sections.c-=============================//
//
// OpenMP API Version 4.5 Nov 2015
//
// testing the combined construct 'parallel sections'
//
//===---------------------------------------------------------------------===//



#include <stdio.h>
#include <unistd.h>
#include <omp.h>
#include "ompvv.h"
#include <pthread.h>

pthread_mutex_t Mutex;
int Var = 0;

// The following function waits for Var value to become 1 and then increments
// it to Var = 1
void function1(int *Var) {
  while(1) {
    pthread_mutex_lock(&Mutex);
    if (*Var == 1) {
      *Var += 1;
      pthread_mutex_unlock(&Mutex);
      break;
    }
  pthread_mutex_unlock(&Mutex);
  }
  pthread_mutex_unlock(&Mutex);
}

// The following function check if Var == 0 and then increments to Var = 1
// and then immediately it loops until Var == 3 and then increments it to
// Var = 4
void function2(int *Var) {
  while(1) {
    pthread_mutex_lock(&Mutex);
    if (*Var == 0) {
      *Var += 1;
      pthread_mutex_unlock(&Mutex);
      break;
    }
  pthread_mutex_unlock(&Mutex);
  }
  pthread_mutex_unlock(&Mutex);

  while(1) {
    pthread_mutex_lock(&Mutex);
    if (*Var == 3) {
      *Var += 1;
      pthread_mutex_unlock(&Mutex);
      break;
    }
  pthread_mutex_unlock(&Mutex);
  }
  pthread_mutex_unlock(&Mutex);
}

// The following function checks if Var == 2 and then increments to Var = 3
void function3(int *Var) {
  while(1) {
    pthread_mutex_lock(&Mutex);
    if (*Var == 2) {
      *Var += 1;
      pthread_mutex_unlock(&Mutex);
      break;
    }
  pthread_mutex_unlock(&Mutex);
  }
  pthread_mutex_unlock(&Mutex);
}


int main() {

  pthread_mutex_init(&Mutex, NULL);
#pragma omp parallel sections
  {
#pragma omp section
    function1(&Var);

#pragma omp section
    function2(&Var);

#pragma omp section
    function3(&Var);
  }
  pthread_mutex_destroy(&Mutex);
  int errors = 0;
  // The final expected value of Var is '4'
  OMPVV_TEST_AND_SET_VERBOSE(errors, (Var != 4));
  OMPVV_REPORT_AND_RETURN(errors);
}

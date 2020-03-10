#include "omp.h"


int offload_test() {
  int a = 0;

#pragma omp target map(tofrom: a)
  {
    a = 1;
  }

  return a;
}

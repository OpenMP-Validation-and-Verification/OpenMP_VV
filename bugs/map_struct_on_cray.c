#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

void test_map_struct() {
  struct {
    int a;
    int b[5];
    int *p;
  } single, array[5];

  single.p = (int*)malloc(5*sizeof(int));

  for (int i = 0; i < 5; ++i) {
    array[i].p = (int*)malloc(5*sizeof(int));
  }

#pragma omp target map(tofrom: single) map(tofrom: array[0:5])
  { 
    single.a = 1;
    for (int i = 0; i < 5; ++i) {
      array[i].a = 1;
    }
  }
}

int main () {
  test_map_struct();

  return 0;
}

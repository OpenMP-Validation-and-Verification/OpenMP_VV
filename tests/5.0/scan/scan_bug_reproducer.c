#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int main() {
  int errors = 0;
  int x = 0;
  int expected_x = 0;
  int a[N];
  int b[N];

  for (int i = 0; i < N; i++) {
    a[i] = i;
    b[i] = 0;
  }

#pragma omp parallel for simd reduction(inscan, +: x)
  for (int i = 0; i < N; i++) {
    x += a[i];
#pragma omp scan inclusive(x)
    b[i] = x;
  }

  for (int i = 0; i < N; i++) {
    for (int j = 0; j <= i; j++) {
      expected_x += a[j];
    }
    if (b[i] != expected_x) {
      errors++;
    }
    expected_x = 0;
  }

  return errors;
}

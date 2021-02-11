#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

#define N 1024

int main() {
  int errors = 0;
  int y[N];
  int z[N];
  int sum = 0;
  int expected_sum = 0;

  for (int i = 0; i < N; i++) {
    y[i] = i + 1;
    z[i] = 2*(i + 1);
  }

#pragma omp target parallel reduction(task, +: sum) shared(y, z) map(tofrom: sum, y, z)
  {
#pragma omp master
    {
#pragma omp task in_reduction(+: sum)
      {
        for (int i = 0; i < N; i++) {
          sum += y[i]*z[i];
        }
      }
    }
  }

  for (int i = 0; i < N; i++) {
    expected_sum += y[i]*z[i];
  }

  if (sum != expected_sum) {
    printf("Did not obtain matching sums.\n");
  } else {
    printf("Test passed.\n");
  }

  return errors;
}

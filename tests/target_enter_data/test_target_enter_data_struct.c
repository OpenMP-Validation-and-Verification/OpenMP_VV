// RUN: %libomptarget-compile-run-and-check-aarch64-unknown-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64le-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-x86_64-pc-linux-gnu

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <omp.h>

#define N 1000
#define ARRAY_SIZE 5

#define DEBUG_MODE 1
#define __FILENAME__ (strrchr(__FILE__, '/') ? strrchr(__FILE__, '/') + 1 : __FILE__)
#define TEST_ERRORS(var, test, ...) {         \
  var |= test;                          \
if (DEBUG_MODE && test) {                  \
fprintf(stderr, "[%s] ERROR reported in line %i, test = " #test "\n", __FILENAME__, __LINE__);     \
__VA_ARGS__;                            \
}  \
}

int test_struct() {

  printf("test_struct\n");

  int errors = 0, isHost = -1;
  int* pointers[ARRAY_SIZE + 1];

  struct {
    int a; // firstprivate
    int b[N]; // tofrom:b[0:N]
    int *p; // tofrom:p[0:0]
  } single, array[ARRAY_SIZE], singleCopy, arrayCopy[ARRAY_SIZE];

  // single initialization on host. Using map(to)
  single.p = (int*) malloc(ARRAY_SIZE * sizeof(int));
  pointers[0] = single.p;
  single.a = 1;
  for (int i = 0; i < N; ++i)
    single.b[i] = i;

  // Array initialization on host. Using map(to)
  for (int i = 0; i < ARRAY_SIZE; ++i) {
    array[i].p = (int*) malloc(ARRAY_SIZE * sizeof(int));
    pointers[i + 1] = array[i].p;
    array[i].a = 1;
    for (int j = 0; j < N; ++j)
      array[i].b[j] = j;
  }

  // unstructured mapping
  {
#pragma omp target enter data map(to: single) map(to: array[0:ARRAY_SIZE])
    printf(""); // forcing the compiler to not moving out of the scope
  }
  // operation
#pragma omp target map(from: singleCopy) map(from: arrayCopy[0:ARRAY_SIZE]) map(tofrom: isHost)
  {
    isHost = omp_is_initial_device();

    singleCopy.a = single.a;
    singleCopy.p = single.p;
    for (int i = 0; i < N; ++i)
      singleCopy.b[i] = single.b[i];

    // Array initialization on host. Using map(to)
    for (int i = 0; i < ARRAY_SIZE; ++i) {
      arrayCopy[i].a = array[i].a;
      arrayCopy[i].p = array[i].p;
      for (int j = 0; j < N; ++j)
        arrayCopy[i].b[j] = array[i].b[j];
    }
  }

  // checking results
  TEST_ERRORS(errors, (singleCopy.a != single.a)); 
  for (int i = 0; i < N; ++i)
    TEST_ERRORS(errors, (singleCopy.b[i] != single.b[i]));
  TEST_ERRORS(errors, (pointers[0] != singleCopy.p));
  for (int i = 0; i < ARRAY_SIZE; ++i) {
    TEST_ERRORS(errors, (arrayCopy[i].a != array[i].a)); 
    for (int j = 0; j < N; ++j)
      TEST_ERRORS(errors, (arrayCopy[i].b[j] != array[i].b[j]));
    TEST_ERRORS(errors, (pointers[i + 1] != arrayCopy[i].p));
  }

  if (!errors)
    printf("Test passed on %s\n", (isHost ? "host" : "device"));
  else
    printf("Test failed on %s\n", (isHost ? "host" : "device"));

  return errors;
}

int test_typedef() {

  printf("test_typedef\n");

  int errors = 0, isHost = -1;
  int* pointers[ARRAY_SIZE + 1];

  typedef struct {
    int a;
    int b[N];
    int *p;
  } test_struct;

  test_struct single, array[ARRAY_SIZE], singleCopy, arrayCopy[ARRAY_SIZE];

  // single initialization on host. Using map(to)
  single.p = (int*) malloc(ARRAY_SIZE * sizeof(int));
  pointers[0] = single.p;
  single.a = 1;
  for (int i = 0; i < N; ++i)
    single.b[i] = i;

  for (int i = 0; i < ARRAY_SIZE; ++i) {
    array[i].p = (int*) malloc(ARRAY_SIZE * sizeof(int));
    pointers[i + 1] = array[i].p;
    array[i].a = 1;
    for (int j = 0; j < N; ++j)
      array[i].b[j] = j;
  }

  // unstructured mapping
  {
#pragma omp target enter data map(to: single) map(to: array[0:ARRAY_SIZE])
    printf(""); // forcing the compiler to not moving out of the scope
  }
  // operation
#pragma omp target map(from: singleCopy) map(from: arrayCopy[0:ARRAY_SIZE]) map(tofrom: isHost)
  {
    isHost = omp_is_initial_device();
    singleCopy.a = single.a;
    singleCopy.p = single.p;
    for (int i = 0; i < N; ++i)
      singleCopy.b[i] = single.b[i];

    for (int i = 0; i < ARRAY_SIZE; ++i) {
      arrayCopy[i].a = array[i].a;
      arrayCopy[i].p = array[i].p;

      for (int j = 0; j < N; ++j)
        arrayCopy[i].b[j] = array[i].b[j]; 
    }
  }

  // checking results
  TEST_ERRORS(errors, (singleCopy.a != single.a)); 
  for (int i = 0; i < N; ++i)
    TEST_ERRORS(errors, (singleCopy.b[i] != single.b[i]));
  TEST_ERRORS(errors, (pointers[0] != singleCopy.p));
  for (int i = 0; i < ARRAY_SIZE; ++i) {
    TEST_ERRORS(errors, (arrayCopy[i].a != array[i].a)); 
    for (int j = 0; j < N; ++j)
      TEST_ERRORS(errors, (arrayCopy[i].b[j] != array[i].b[j]));
    TEST_ERRORS(errors, (pointers[i + 1] != arrayCopy[i].p));
  }

  if (!errors)
    printf("Test passed on %s\n", (isHost ? "host" : "device"));
  else
    printf("Test failed on %s\n", (isHost ? "host" : "device"));

  return errors;
}

int main () {
  int errors = 0;
  errors += test_struct();
  errors += test_typedef();
  return errors;
}

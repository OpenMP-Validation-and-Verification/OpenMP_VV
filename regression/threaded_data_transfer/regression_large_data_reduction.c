//===--- regression_large_data_reduction.c ----------------------------------===//
//
// OpenMP API Version 4.5
//
// this test 8 differently sized array using 10000 threads to perform +=, -=,
// *=, |=, &=, and ^= reductions it peforms three sum reductions since i dont
// know how to use && and || reductions
//
// Clause being tested
// reduction
//
// Author: Aaron Liu <olympus@udel.edu> Oct 2023
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

#define N 32768
#define number_of_threads 10000

int main() {
  int error = 0;

  int array_size8 = N;
  int array_size7 = (N * .875);
  int array_size6 = (N * .750);
  int array_size5 = (N * .635);
  // any larger whould cause a overflow
  int array_size4 = 20;
  int array_size3 = (N * .375);
  int array_size2 = (N * .250);
  int array_size1 = (N * .125);
  int initialReductionValue = 1;

  int array8[array_size8];
  int hostReduction8 = initialReductionValue;
  int deviceReduction8 = initialReductionValue;

  int array7[array_size7];
  int hostReduction7 = initialReductionValue;
  int deviceReduction7 = initialReductionValue;

  int array6[array_size6];
  int hostReduction6 = initialReductionValue;
  int deviceReduction6 = initialReductionValue;

  int array5[array_size5];
  int hostReduction5 = initialReductionValue;
  int deviceReduction5 = initialReductionValue;

  int array4[array_size4];
  int hostReduction4 = initialReductionValue;
  int deviceReduction4 = initialReductionValue;

  int array3[array_size3];
  int hostReduction3 = initialReductionValue;
  int deviceReduction3 = initialReductionValue;

  int array2[array_size2];
  int hostReduction2 = initialReductionValue;
  int deviceReduction2 = initialReductionValue;

  int array1[array_size1];
  int hostReduction1 = initialReductionValue;
  int deviceReduction1 = initialReductionValue;

  for (int x = 0; x < array_size8; x++) {
    array8[x] = x;
    hostReduction8 += x;
    if (x < array_size7) {
      array7[x] = x;
      hostReduction7 += x;
    }
    if (x < array_size6) {
      array6[x] = x;
      hostReduction6 += x;
    }
    if (x < array_size5) {
      array5[x] = x;
      hostReduction5 -= x;
    }
    if (x != 0 && x < array_size4) {
      array4[x] = x;
      hostReduction4 *= x;
    }
    if (x < array_size3) {
      array3[x] = x;
      hostReduction3 &= x;
    }
    if (x < array_size2) {
      array2[x] = x;
      hostReduction2 |= x;
    }
    if (x < array_size1) {
      array1[x] = x;
      hostReduction1 ^= x;
    }
  }

#pragma omp target parallel for num_threads(number_of_threads)                 \
    map(to : array8[0 : array_size8], array7[0 : array_size7],                 \
            array6[0 : array_size6], array5[0 : array_size5],                  \
            array4[0 : array_size4], array3[0 : array_size3],                  \
            array2[0 : array_size2], array1[0 : array_size1])                  \
    reduction(+ : deviceReduction8) reduction(+ : deviceReduction7)            \
    reduction(+ : deviceReduction6) reduction(- : deviceReduction5)            \
    reduction(* : deviceReduction4) reduction(& : deviceReduction3)            \
    reduction(| : deviceReduction2) reduction(^ : deviceReduction1)            \
    map(tofrom : deviceReduction8, deviceReduction7, deviceReduction6,         \
            deviceReduction5, deviceReduction4, deviceReduction3,              \
            deviceReduction2, deviceReduction1)
  for (int x = 0; x < array_size8; x++) {
    deviceReduction8 += array8[x];

    if (x < array_size7) {
      deviceReduction7 += array7[x];
    }
    if (x < array_size6) {
      deviceReduction6 += array6[x];
    }

    if (x < array_size5) {
      deviceReduction5 -= array5[x];
    }

    if (x != 0 && x < array_size4) {
      deviceReduction4 *= array4[x];
    }
    if (x < array_size3) {
      deviceReduction3 &= array3[x];
    }
    if (x < array_size2) {
      deviceReduction2 |= array2[x];
    }
    if (x < array_size1) {
      deviceReduction1 ^= array1[x];
    }
  }

  if (deviceReduction8 != hostReduction8) {
    error += 1;
  }
  if (deviceReduction7 != hostReduction7) {
    error += 1;
  }
  if (deviceReduction6 != hostReduction6) {
    error += 1;
  }
  if (deviceReduction5 != hostReduction5) {
    error += 1;
  }
  if (deviceReduction4 != hostReduction4) {
    error += 1;
  }
  if (deviceReduction3 != hostReduction3) {
    error += 1;
  }
  if (deviceReduction2 != hostReduction2) {
    error += 1;
  }
  if (deviceReduction1 != hostReduction1) {
    error += 1;
  }

  printf("program created %d amount of errors\n", error);
  printf(" (^= Reduction) host = %d device = %d\n (|= Reduction) host = %d "
         "device = %d\n (&= Reduction) host = %d device = %d\n (*= Reduction) "
         "host = %d device = %d\n (-= Reduction) host = %d device = %d\n (+= "
         "Reduction) host = %d device = %d\n (+= Reduction) host = %d device = "
         "%d\n (+= Reduction) host = %d device = %d\n",
         hostReduction1, deviceReduction1, hostReduction2, deviceReduction2,
         hostReduction3, deviceReduction3, hostReduction4, deviceReduction4,
         hostReduction5, deviceReduction5, hostReduction6, deviceReduction6,
         hostReduction7, deviceReduction7, hostReduction8, deviceReduction8);
  return error;
}
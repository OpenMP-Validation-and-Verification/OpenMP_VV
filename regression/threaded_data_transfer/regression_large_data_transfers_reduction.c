//===--- regression_large_data_transfer_reduction.c --------------------------===//
//
// OpenMP API Version 4.5
//
// this test is meant to test the runtime for the reduction clause while also 
// performing data transfers between the accelerator and the host
//
// Clause being tested
// reduction (+=, -=, *=, |=, &=, ^=) 
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
  int array_size4 = (N * .500);
  int MultiplcationSize = 20;
  int array_size3 = (N * .375);
  int array_size2 = (N * .250);
  int array_size1 = (N * .125);
  int initialReductionValue = 1;

  int data8[array_size8];
  int empty8[array_size8];
  int hostReduction8 = initialReductionValue;
  int deviceReduction8 = initialReductionValue;

  int data7[array_size7];
  int empty7[array_size7];
  int hostReduction7 = initialReductionValue;
  int deviceReduction7 = initialReductionValue;

  int data6[array_size6];
  int empty6[array_size6];
  int hostReduction6 = initialReductionValue;
  int deviceReduction6 = initialReductionValue;

  int data5[array_size5];
  int empty5[array_size5];
  int hostReduction5 = initialReductionValue;
  int deviceReduction5 = initialReductionValue;

  int data4[array_size4];
  int empty4[array_size4];
  int hostReduction4 = initialReductionValue;
  int deviceReduction4 = initialReductionValue;

  int data3[array_size3];
  int empty3[array_size3];
  int hostReduction3 = initialReductionValue;
  int deviceReduction3 = initialReductionValue;

  int data2[array_size2];
  int empty2[array_size2];
  int hostReduction2 = initialReductionValue;
  int deviceReduction2 = initialReductionValue;

  int data1[array_size1];
  int empty1[array_size1];
  int hostReduction1 = initialReductionValue;
  int deviceReduction1 = initialReductionValue;

  for (int x = 0; x < array_size8; x++) {
    data8[x] = x;
    hostReduction8 += x;
    if (x < array_size7) {
      data7[x] = x;
      hostReduction7 += x;
    }
    if (x < array_size6) {
      data6[x] = x;
      hostReduction6 += x;
    }
    if (x < array_size5) {
      data5[x] = x;
      hostReduction5 -= x;
    }
    if (x < array_size4) {
      data4[x] = x;
      if (x != 0 && x < MultiplcationSize) {
        hostReduction4 *= x;
      }
    }
    if (x < array_size3) {
      data3[x] = x;
      hostReduction3 &= x;
    }
    if (x < array_size2) {
      data2[x] = x;
      hostReduction2 |= x;
    }
    if (x < array_size1) {
      data1[x] = x;
      hostReduction1 ^= x;
    }
  }

#pragma omp target parallel for num_threads(number_of_threads)                 \
    map(to : data8[0 : array_size8], data7[0 : array_size7],                   \
            data6[0 : array_size6], data5[0 : array_size5],                    \
            data4[0 : array_size4], data3[0 : array_size3],                    \
            data2[0 : array_size2], data1[0 : array_size1], MultiplcationSize) \
    reduction(+ : deviceReduction8) reduction(+ : deviceReduction7)            \
    reduction(+ : deviceReduction6) reduction(- : deviceReduction5)            \
    reduction(* : deviceReduction4) reduction(& : deviceReduction3)            \
    reduction(| : deviceReduction2) reduction(^ : deviceReduction1)            \
    map(from : empty8[0 : array_size8], empty7[0 : array_size7],               \
            empty6[0 : array_size6], empty5[0 : array_size5],                  \
            empty4[0 : array_size4], empty3[0 : array_size3],                  \
            empty2[0 : array_size2], empty1[0 : array_size1])                  \
    map(tofrom : deviceReduction8, deviceReduction7, deviceReduction6,         \
            deviceReduction5, deviceReduction4, deviceReduction3,              \
            deviceReduction2, deviceReduction1)
  for (int x = 0; x < array_size8; x++) {
    deviceReduction8 += data8[x];
    empty8[x] = data8[x];

    if (x < array_size7) {
      deviceReduction7 += data7[x];
      empty7[x] = data7[x];
    }
    if (x < array_size6) {
      deviceReduction6 += data6[x];
      empty6[x] = data6[x];
    }
    if (x < array_size5) {
      deviceReduction5 -= data5[x];
      empty5[x] = data5[x];
    }
    if (x < array_size4) {
      empty4[x] = data4[x];
      if (x != 0 && x < MultiplcationSize) {
        deviceReduction4 *= data4[x];
      }
    }
    if (x < array_size3) {
      deviceReduction3 &= data3[x];
      empty3[x] = data3[x];
    }
    if (x < array_size2) {
      deviceReduction2 |= data2[x];
      empty2[x] = data2[x];
    }
    if (x < array_size1) {
      deviceReduction1 ^= data1[x];
      empty1[x] = data1[x];
    }
  }

  for (int x = 0; x < array_size8; x++) {
    if (empty8[x] != data8[x]) {
      error += 1;
    }
    if (x < array_size7) {
      if (empty7[x] != data7[x]) {
        error += 1;
      }
    }
    if (x < array_size6) {
      if (empty6[x] != data6[x]) {
        error += 1;
      }
    }
    if (x < array_size5) {
      if (empty5[x] != data5[x]) {
        error += 1;
      }
    }
    if (x < array_size4) {
      if (empty4[x] != data4[x]) {
        error += 1;
      }
    }
    if (x < array_size3) {
      if (empty3[x] != data3[x]) {
        error += 1;
      }
    }
    if (x < array_size2) {
      if (empty2[x] != data2[x]) {
        error += 1;
      }
    }
    if (x < array_size1) {
      if (empty1[x] != data1[x]) {
        error += 1;
      }
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

  return error;
}
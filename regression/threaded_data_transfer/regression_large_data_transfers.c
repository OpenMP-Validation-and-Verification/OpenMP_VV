//===--- regression_large_data_transfers.c ---------------------------------===//
//
// OpenMP API Version 4.5
//
// this is meant to test stress the runtime for the data transfers using 8 
// differently sized arrays 
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
  int array_size3 = (N * .375);
  int array_size2 = (N * .250);
  int array_size1 = (N * .125);

  int data8[array_size8];
  int empty8[array_size8];

  int data7[array_size7];
  int empty7[array_size7];

  int data6[array_size6];
  int empty6[array_size6];

  int data5[array_size5];
  int empty5[array_size5];

  int data4[array_size4];
  int empty4[array_size4];

  int data3[array_size3];
  int empty3[array_size3];

  int data2[array_size2];
  int empty2[array_size2];

  int data1[array_size1];
  int empty1[array_size1];

  for (int x = 0; x < array_size8; x++) {
    data8[x] = x;
    if (x < array_size7) {
      data7[x] = x;
    }
    if (x < array_size6) {
      data6[x] = x;
    }
    if (x < array_size5) {
      data5[x] = x;
    }
    if (x < array_size4) {
      data4[x] = x;
    }
    if (x < array_size3) {
      data3[x] = x;
    }
    if (x < array_size2) {
      data2[x] = x;
    }
    if (x < array_size1) {
      data1[x] = x;
    }
  }

#pragma omp target parallel for num_threads(number_of_threads)                 \
    map(to : data8[0 : array_size8], data7[0 : array_size7],                   \
            data6[0 : array_size6], data5[0 : array_size5],                    \
            data4[0 : array_size4], data3[0 : array_size3],                    \
            data2[0 : array_size2], data1[0 : array_size1])                    \
    map(from : empty8[0 : array_size8], empty7[0 : array_size7],               \
            empty6[0 : array_size6], empty5[0 : array_size5],                  \
            empty4[0 : array_size4], empty3[0 : array_size3],                  \
            empty2[0 : array_size2], empty1[0 : array_size1])
  for (int x = 0; x < array_size8; x++) {
    empty8[x] = data8[x];
    if (x < array_size7) {
      empty7[x] = data7[x];
    }
    if (x < array_size6) {
      empty6[x] = data6[x];
    }
    if (x < array_size5) {
      empty5[x] = data5[x];
    }
    if (x < array_size4) {
      empty4[x] = data4[x];
    }
    if (x < array_size3) {
      empty3[x] = data3[x];
    }
    if (x < array_size2) {
      empty2[x] = data2[x];
    }
    if (x < array_size1) {
      empty1[x] = data1[x];
    }
  }

  for (int x = 0; x < array_size8; x++) {
    if (data8[x] != empty8[x]) {
      error += 1;
    }
    if (x < array_size7) {
      if (data7[x] != empty7[x]) {
        error += 1;
      }
    }
    if (x < array_size6) {
      if (data6[x] != empty6[x]) {
        error += 1;
      }
    }
    if (x < array_size5) {
      if (data5[x] != empty5[x]) {
        error += 1;
      }
    }
    if (x < array_size4) {
      if (data4[x] != empty4[x]) {
        error += 1;
      }
    }
    if (x < array_size3) {
      if (data3[x] != empty3[x]) {
        error += 1;
      }
    }
    if (x < array_size2) {
      if (data2[x] != empty2[x]) {
        error += 1;
      }
    }
    if (x < array_size1) {
      if (data1[x] != empty1[x]) {
        error += 1;
      }
    }
  }

  printf("program created %d amount of errors\n", error);
  return error;
}
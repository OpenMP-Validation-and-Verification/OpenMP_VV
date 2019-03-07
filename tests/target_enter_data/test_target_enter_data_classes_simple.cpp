//===---- test_target_enter_data_classes_simple.c - combined consutrct -===//
//
// OpenMP API Version 4.5 Nov 2015
//
//
//===----------------------------------------------------------------------------------===//
//
//

#include <iostream>
#include <omp.h>
#include <cassert>
#include "ompvv.h"
#include <cmath>

#define N 1000

class Simple {
private:
  int *d_array;
  int size;
  int sum;

public:
  Simple(int s) : size(s) { 
      sum = 0;
      d_array = new int[size];
      int* helper = d_array;
      int &hs = size;
      int &hsum = sum;
#pragma omp target enter data map(to: helper[0:size]) map(to: hs) map(to:hsum)
  }

  ~Simple() { 
      int* helper = d_array;
      int &hs = size;
      int &hsum = sum;
#pragma omp target exit data map(delete: helper[0:size]) map(delete: hs) map(delete: hsum)
      delete[] d_array; 
  }
  
  void modify() {
    int * helper = d_array;
    int &hsize = size;
    int *hsum = &sum;
#pragma omp target defaultmap(tofrom: scalar) 
    {
      *hsum = 0;
      for (int i = 0; i < hsize; ++i) {
        helper[i] = 1;
        *hsum += helper[i];
      }
    }
  }
  void getValues(int &h_sum, int* h_array) {
      int* helper = d_array;
      int &hsize = size;
      int *help_sum = &sum;
#pragma omp target map(from: h_sum) map(tofrom: h_array[0:size])
    {
      h_sum = *help_sum;
      for (int i = 0; i < hsize; i++) {
        h_array[i] = helper[i];
      }
    }
  }
  int* getArray() {
    return d_array;
  }
  
  int* getSum() {
    return &sum;
  }
};

int test_simple_class() {
  
  OMPVV_INFOMSG("Testing enter exit data with a simple class");
  int errors = 0, h_sum = 0, sum = 0;
  int* h_array = new int[N];

  // allocation on the device
  Simple *obj = new Simple(N);

  obj->modify();
  obj->modify();
  obj->modify();
  obj->getValues(h_sum, h_array);

  // checking results
  for (int i = 0; i < N; ++i)
    sum += h_array[i];

  delete obj;
  delete[] h_array;
  
  OMPVV_TEST_AND_SET_VERBOSE(errors, (N != sum));
  OMPVV_TEST_AND_SET_VERBOSE(errors, (N != h_sum));
  OMPVV_ERROR_IF(errors != 0, "N = %d, sum = %d, h_sum = %d", N, sum, h_sum);

  return errors;
}

int main() {
  OMPVV_TEST_OFFLOADING;
  int errors = 0;

  OMPVV_TEST_AND_SET_VERBOSE(errors, test_simple_class());
  
  OMPVV_REPORT_AND_RETURN(errors)
}


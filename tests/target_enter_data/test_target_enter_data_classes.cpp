//===---- test_target_enter_data_classes.c - combined consutrct -===//
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

#define N 1000

template<typename T>
class Mapper {
private:
  T* ptr;
  bool not_mapped;
public:
  Mapper (T* p) : ptr(p) {
    not_mapped = !omp_target_is_present(ptr, omp_get_default_device());
    T* solutionPtr = ptr;
    // maps if target is not present
#pragma omp target enter data map(to:solutionPtr[0:1]) if(not_mapped)
  }
  ~Mapper() {
    T* solutionPtr = ptr;
    // unmaps iff this mapper mapped the target
#pragma omp target exit data map(delete: solutionPtr[0:1]) if(not_mapped)
    ptr = NULL;
  }
};

class B : public Mapper<B> {
private:
  int n;
  double* x;

public:
  B(int n) : Mapper<B>(this), n(n) {
    x = new double[n];
    double* solutionX = x;
#pragma omp target update to(n)
#pragma omp target enter data map(to:solutionX[0:n])
  }

  void modifyB(double* obj_x_ptr) {
    double * cpy_x = x; 
#pragma omp target map(from: obj_x_ptr[0:1])
    {
      for (int i = 0; i < n; ++i)
        cpy_x[i] = 1.0;
      
      obj_x_ptr = x;
    } 
  }

  double* getX() {
    return x;
  }
};

class A : public Mapper<A>, public B {
private:
  int n;
  int* y;

public:
  A(int s) : Mapper<A>(this), B(s), n(s) { 
      y = new int[n];
  }

  void modifyA(double* obj_x_ptr, int* obj_y_ptr) {
    int * cpy_y = y;
#pragma omp target map(from: obj_x_ptr) map(from: obj_y_ptr)
    {
      modifyB(obj_x_ptr);

      for (int i = 0; i < n; ++i)
        cpy_y[i] = 1;
      
      obj_y_ptr = y;
    }
  }
  
  int* getY() {
    return y;
  }
  
  // TODO: Add virtual once supported
  //~A() { h_array = NULL; }
};

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
#pragma omp target enter data map(to: helper[0:size]) map(to: hs)
  }

  ~Simple() { 
      int* helper = d_array;
      int &hs = size;
#pragma omp target exit data map(delete: helper[0:size]) map(delete: hs)
      delete[] d_array; 
  }
  
  void modify() {
    int * helper = d_array;
    int &hsize = size;
    int &hsum = sum;
#pragma omp target defaultmap(tofrom: scalar)
    {
      hsum = 0;
      for (int i = 0; i < hsize; ++i) {
        helper[i] = 1;
        hsum += helper[i];
      }
    }
  }
  void getValues(int &h_sum, int* h_array) {
      int* helper = d_array;
      int &hsize = size;
      int &hsum = sum;
#pragma omp target map(from: h_sum) map(tofrom: h_array[0:size])
    {
      h_sum = hsum;
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

int test_complex_class() {
return 0;
//  cout << "test_complex_class" << endl;
//
//  int sumY = 0, isHost = 0, errors = 0;
//  double sumX = 0.0;
//  int *h_y = new int[N], *obj_y_ptr = NULL;
//  double *h_x = new double[N], *obj_x_ptr = NULL;
//  // allocation on the device
//  A *obj = new A(N);
//
//  obj->modifyA(&isHost, obj_x_ptr, obj_y_ptr);
//
//  assert(0 == omp_target_memcpy(h_y, obj_y_ptr, N*sizeof(int), 0, 0, omp_get_initial_device(), omp_get_default_device()));
//  assert(0 == omp_target_memcpy(h_x, obj_x_ptr, N*sizeof(double), 0, 0, omp_get_initial_device(), omp_get_default_device()));
//
//  // checking results
//  for (int i = 0; i < N; ++i) {
//    sumY += h_y[i];
//    sumX += h_x[i];
//  }
//
//  delete obj;
//  delete[] h_x;
//  delete[] h_y;
//    
//  int res_cpy_y = omp_target_memcpy(h_y, obj_y_ptr, N*sizeof(int), 0, 0, omp_get_initial_device(), omp_get_default_device());
//  int res_cpy_x = omp_target_memcpy(h_x, obj_x_ptr, N*sizeof(double), 0, 0, omp_get_initial_device(), omp_get_default_device());
//
//  errors = (N != sumY) || (N != sumX) || (0 == res_cpy_x) || (0 == res_cpy_y) ;
//  if (!errors)
//    cout << "Test passed on " << (isHost ? "host" : "device") << ": sumX=" << sumX << ", sumY=" << sumY << ", N=" << N << endl;
//  else
//    cout << "Test failed on " << (isHost ? "host" : "device") << ": sumX=" << sumX << ", sumY=" << sumY << ", N=" << N << endl;
//
//  return errors;
}

int main() {
  OMPVV_TEST_OFFLOADING;
  int errors = 0;

  OMPVV_TEST_AND_SET_VERBOSE(errors, test_simple_class());
//  errors += test_complex_class();

  OMPVV_REPORT_AND_RETURN(errors)
}


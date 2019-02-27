// RUN: %libomptarget-compile-run-and-check-aarch64-unknown-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64le-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-x86_64-pc-linux-gnu

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
#pragma omp target map(from: obj_x_ptr[0:1])
    {
      for (int i = 0; i < n; ++i)
        x[i] = 1.0;
      
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
#pragma omp target map(from: obj_x_ptr) map(from: obj_y_ptr)
    {
      modifyB(obj_x_ptr);

      for (int i = 0; i < n; ++i)
        y[i] = 1;
      
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
//#pragma omp target enter data map(to:this[0:1])
      int* helper = d_array;
      int &hs = size;
#pragma omp target enter data map(to: helper[0:size]) map(to: hs)
  }

  // TODO: Add virtual once supported
  ~Simple() { 
      int* helper = d_array;
      int &hs = size;
#pragma omp target exit data map(delete: helper[0:size]) map(delete: hs)
//#pragma omp target exit data map(delete:this[0:1])
      delete[] d_array; 
  }
  
  void modify() {
#pragma omp target defaultmap(tofrom: scalar) 
    {
      sum = 0;
      for (int i = 0; i < size; ++i) {
        d_array[i] = 1;
        sum += d_array[i];
      }
    }
  }
  void getValues(int &h_sum, int* h_array) {
      int* helper = d_array;
      int &hs = size;
      int &hs2 = sum;
#pragma omp target map(from: h_sum) 
    {
      h_sum = sum;
      for (int i = 0; i < size; i++) {
        h_array[i] = d_array[i];
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

int myInt = 5;
#pragma omp target enter data map(to: myInt)

#pragma omp target defaultmap(tofrom:scalar)
{
        myInt += 5;
}

#pragma omp target exit data map(from: myInt)
OMPVV_INFOMSG("The value of myInt is = %d", myInt);
  int errors = 0;

//  errors += test_simple_class();
//  errors += test_complex_class();

  return errors;
}


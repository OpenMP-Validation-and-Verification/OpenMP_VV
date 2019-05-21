//===--test_target_map_classes_default.c - test a class default mapping -----===//
// 
// OpenMP API Version 4.5 Nov 2015
// 
// This test focuses on the mapping of classes into the device. There are 2 
// definition of clases. A, and B. A contains an array and its size, while B
// has an static double and a virtual method. This test consist of 4 parts
// explicit default mapping of the array, implicit default mapping of the array,
// static variable mapping, virtual method mapping. The exmplicit test has a map
// clause that does not use a map-type-modifier for the array. It should be 
// mapped as tofrom. The implicit test does not use a map clause but writes to
// the array. This should do the mapping automatically, and use the tofrom
// modifier. The static variable mapping test if an static variable will be copied
// and map over to the device. Finally, the virtual method test should access
// the method from the device. (See TODO note)
//
// TODO: Add virtual once supported by compilers
// 
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include "ompvv.h"
#include <cmath>

#define N 1000

class A {

private:
  int h_array[N];
  int size;

public:
  A(const int s) : size(s) {
    for (int i = 0; i < N; i++) {
      h_array[i] = 0;
    }
  }

  void modifyExplicit() {
    int * theArray = this->h_array;
    int theSize = size;
    // It is not possible to do this-> since it is an
    // expression and it is not supported by 4.5
#pragma omp target map(theArray[0:N]) map(theSize) 
    {
      for (int i = 0; i < theSize; ++i)
          theArray[i] += 1;
    } // end target
  }

  void modifyImplicit() {
#pragma omp target // implicit map(tofrom: this->h_array) map(firstprivate: this->size)
    {
      for (int i = 0; i < size; ++i)
          h_array[i] += 1;
    } // end target 
  }

  int* getArray() {
    return &h_array[0];
  }
};

class B {
public:
  static double VAR;
  B() {}

  ~B() {}

#pragma omp declare target 
  static int staticMethod() {
      return 1;
  }
#pragma omp end declare target 
};

double B::VAR = 1.0;

int test_explicit() {

  OMPVV_INFOMSG("Explicit mapping test");
  int sum = 0, errors = 0;

  A *obj = new A(N);

  obj->modifyExplicit();

  // checking results
  int* h_array = obj->getArray();
  for (int i = 0; i < N; ++i)
    sum += h_array[i];

  OMPVV_TEST_AND_SET_VERBOSE(errors, N != sum);

  delete obj;

  return errors;
}

int test_implicit() {

  OMPVV_INFOMSG("Test implicit mapping");

  int sum = 0, errors = 0;

  A *obj = new A(N);

  obj->modifyImplicit();

  // checking results
  int* h_array = obj->getArray();
  for (int i = 0; i < N; ++i)
    sum += h_array[i];

  OMPVV_TEST_AND_SET_VERBOSE(errors, N != sum);

  delete obj;

  return errors;
}

int test_static () {

  OMPVV_INFOMSG("Testing accessing a static variable");

  int errors = 0;
  double res = 0.0;
  
#pragma omp target map(tofrom: res)
  {
    res = B::VAR;
  } // end target

  // checking results
  OMPVV_TEST_AND_SET_VERBOSE(errors, std::abs(res - 1.0) > 0.0001)

  return errors;
}

int test_static_method () {
  OMPVV_INFOMSG("Testing static methods on the device");

  int errors = 0;
  int value = 0;

#pragma omp target map(tofrom: value) 
  {
    value = B::staticMethod();
  } // end target

  OMPVV_TEST_AND_SET_VERBOSE(errors, std::abs(value - 1.0) > 0.0001);

  return errors;
}

int main() {
  OMPVV_TEST_OFFLOADING;

  int errors = 0;

  OMPVV_TEST_AND_SET_VERBOSE(errors, test_static_method() != 0);
  OMPVV_TEST_AND_SET_VERBOSE(errors,  test_static() != 0);
  OMPVV_TEST_AND_SET_VERBOSE(errors,  test_explicit() != 0);
  OMPVV_TEST_AND_SET_VERBOSE(errors,  test_implicit() != 0);

  OMPVV_REPORT_AND_RETURN(errors);
}

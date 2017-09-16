// RUN: %libomptarget-compile-run-and-check-aarch64-unknown-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64le-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-x86_64-pc-linux-gnu

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
#include <iostream>

using namespace std;

#define N 1000

class A {

private:
  int h_array[N];
  int size;

public:
  A(const int s) : size(s) {}

  void modifyExplicit(int* isHost) {
#pragma omp target map(this->h_array) map(this->size)           \
        map(tofrom: isHost) // by defaul arrays are tofrom
    {
      *isHost = omp_is_initial_device();
      for (int i = 0; i < size; ++i)
          h_array[i] = 1;
    } // end target
  }

  void modifyImplicit(int* isHost) {
#pragma omp target map(tofrom: isHost) // implicit map(tofrom: this->h_array) map(firstprivate: this->size)
    {
      *isHost = omp_is_initial_device();
      for (int i = 0; i < size; ++i)
          h_array[i] = 1;
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

  // TODO: Add virtual once supported
  ~B() {}
  
  virtual int vMethod() {
      return 1;
  }
};
double B::VAR = 1.0;

int test_explicit() {

  cout << "test_explicit" << endl;

  int sum = 0, isHost = 0, errors = 0;

  A *obj = new A(N);

  obj->modifyExplicit(&isHost);

  // checking results
  int* h_array = obj->getArray();
  for (int i = 0; i < N; ++i)
    sum += h_array[i];

  errors = N != sum;
  if (!errors)
    cout << "Test passed on " << (isHost ? "host" : "device") << ": sum=" << sum << ", N=" << N << endl;
  else
    cout << "Test failed on " << (isHost ? "host" : "device") << ": sum=" << sum << ", N=" << N << endl;

  delete obj;

  return errors;
}

int test_implicit() {

  cout << "test_implicit" << endl;

  int sum = 0, isHost = 0, errors = 0;

  A *obj = new A(N);

  obj->modifyImplicit(&isHost);

  // checking results
  int* h_array = obj->getArray();
  for (int i = 0; i < N; ++i)
    sum += h_array[i];

  errors = N != sum;
  if (!errors)
    cout << "Test passed on " << (isHost ? "host" : "device") << ": sum=" << sum << ", N=" << N << endl;
  else
    cout << "Test failed on " << (isHost ? "host" : "device") << ": sum=" << sum << ", N=" << N << endl;

  delete obj;

  return errors;
}

int test_static () {
  cout << "test_static" << endl;

  int isHost = 0, errors = 0;
  double exp = 1.0, res = 0.0;
  
  B *obj = new B();

#pragma omp target map(tofrom: obj[0:1]) map(tofrom: res) map(tofrom: isHost)
  {
    isHost = omp_is_initial_device();
    res = B::VAR;
  } // end target

  // checking results
  errors = res != exp;
  if (!errors)
    cout << "Test passed on " << (isHost ? "host" : "device") << ": exp=" << exp << ", res=" << res << endl;
  else
    cout << "Test failed on " << (isHost ? "host" : "device") << ": exp=" << exp << ", res=" << res << endl;

  delete obj;

  return errors;
}

int test_virtual () {
  cout << "test_virtual" << endl;

  int isHost = 0, errors = 0;
  int (B::*vMethodPointer) (); // function pointer

  B *obj = new B();

#pragma omp target map(tofrom: obj[0:1]) map(tofrom: vMethodPointer) \
        map(tofrom: isHost)
  {
      isHost = omp_is_initial_device();
      vMethodPointer = &B::vMethod;
  } // end target

  // checking results
  errors = &B::vMethod != vMethodPointer;
  if (!errors)
    cout << "Test passed on " << (isHost ? "host" : "device") << endl;
  else
    cout << "Test failed on " << (isHost ? "host" : "device") << endl;

  delete obj;

  return errors;
}

int main() {

  int errors = 0;

  errors += test_virtual();
  errors += test_static();
  errors += test_explicit();
  errors += test_implicit();

  return errors;
}

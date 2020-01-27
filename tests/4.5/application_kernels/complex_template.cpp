//===--- complex_template.c ------------------------------------------------===//
//
// OpenMP API Version 4.5 Nov 2015
//
// This is a stripped-down application code which sets up two template
// structs which use standard library functions. A templated struct
// containing the other struct is declared on the device. This test does
// not check any results as it is only a test of declaring a templated
// struct on the device.
//
////===----------------------------------------------------------------------===//

#include <type_traits>
#include "omp.h"
#include "ompvv.h"

template<int Dim> struct V {
  template<bool B = (Dim == 0),
           typename = typename std::enable_if<B>::type>
  V() {}

  template<typename TArg0,
           typename = typename std::enable_if<(std::is_same<unsigned long,
                                               typename std::decay<TArg0>::type>::value)>::type>
  V(TArg0 && arg0) {}
};

template<int Dim> struct S {
  V<Dim> v;
};

int main(int argc, char *argv[]) {
  OMPVV_TEST_OFFLOADING;

  OMPVV_TEST_SHARED_ENVIRONMENT;

  int errors = 0;

#pragma omp target
  {
    S<0> s;
  }

  OMPVV_REPORT_AND_RETURN(errors);
}

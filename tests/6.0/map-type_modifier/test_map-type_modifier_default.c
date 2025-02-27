//--------------test_map-type_modifier_default.c-----------------------------------//
// OpenMP API Version 6.0 November 2024
// Pg. 899, line 12
// ***********
// DIRECTIVE:target
// CLAUSE:map
// ***********
// The default properties behavior of the map-type modifier supplied to the map
// clause is being tested. Providing the present modifier and a from modifier
// ensures that more than one modifier can be supplied at once to map. This is
// in accordance with the modifier defaults column in Table 5.1 on page 159. If
// a tofrom behavior is observed at runtime, then the test passes.
//---------------------------------------------------------------------------------//
#include "ompvv.h"

#define N 2

int test_maptype_modifier_default() {
  int errors = 0, a = 1;

  #pragma omp target enter data map(to : a)

  #pragma omp target map(present, from : a)
  {
    a = a * N;
  }
  #pragma omp target update from(a)

  OMPVV_TEST_AND_SET_VERBOSE(errors, a != N)
  return errors;
}

int main() {
  int errors = 0;
  OMPVV_TEST_AND_SET(errors, test_maptype_modifier_default() != 0);
  OMPVV_REPORT_AND_RETURN(errors);
  return errors;
}

//-------------- test_assumes_noopenmpconstructs.c----------------------------------//
// OpenMP API Version 6.0 November 2024
// Pg. 900, line 22
// ***********
// DIRECTIVE:assumes
// CLAUSE:no_openmp_constructs 
// ***********
// The no_openmp_constructs clause is used to guarantee that the section of code
// which applies to the assumes directive does not contain any OpenMP constructs. 
//---------------------------------------------------------------------------------//
#include "ompvv.h"

#define N 4

int test_assumes_no_openmp_constructs() {
  int errors = 0;

  #pragma omp assume no_openmp_constructs(1)
  {
  
  }

  return errors;
}

int main() {
  int errors = 0;
  OMPVV_TEST_AND_SET(errors, test_assumes_no_openmp_constructs() != 0);
  OMPVV_REPORT_AND_RETURN(errors);
  return errors;
}

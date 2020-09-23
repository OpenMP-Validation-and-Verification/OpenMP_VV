!===--- test_declare_target_nested_functions.F90 -------------------------------------===//
!
! OpenMP API Version 5.0 Nov 2018
!
! Test of declare target on a host function that calls another host
! function. According to 5.0 specification, the inner function, while
! not explicitly declared on the target, should be treated as if it was.
! The declared functions increment the passed-in value and return it. 
! They are tested in a simple target region and the result is mapped out
! and checked.
!
!//===----------------------------------------------------------------------===//

#include "ompvv.F90"

#define N 1024

PROGRAM test_declare_target_nested_functions

    USE iso_fortran_env
    USE ompvv_lib
    USE omp_lib
    implicit none
    INTEGER :: errors
    errors = 0 

    OMPVV_TEST_OFFLOADING

    !$omp declare target to(outer_fn)

    OMPVV_TEST_VERBOSE(test_declared_functions() .ne. 0)

    OMPVV_REPORT_AND_RETURN()

CONTAINS  
    INTEGER FUNCTION inner_fn(a)
      INTEGER:: a
      inner_fn = 1 + a
    END FUNCTION inner_fn

    INTEGER FUNCTION outer_fn(a)
      INTEGER:: a
      outer_fn = 1 + inner_fn(a)
    END FUNCTION outer_fn
    
    INTEGER FUNCTION test_declared_functions() 
      INTEGER:: outcome, errors
      errors = 0
      outcome = 0

      !$omp target map (tofrom: outcome)
      outcome = outer_fn(outcome)
      !$omp end target
    
      OMPVV_TEST_AND_SET_VERBOSE(errors, outcome .ne. 2)

      test_declared_functions = errors
    END FUNCTION test_declared_functions

END PROGRAM test_declare_target_nested_functions 

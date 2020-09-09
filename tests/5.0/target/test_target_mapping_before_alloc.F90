!===---- test_target_mapping_before_alloc.F90 -------------------------------===//
!
! OpenMP API Version 5.0 Nov 2018
!
! The description of the map clause was modified to clarify the mapping
! order when multiple map-types are specified for a variable or structure 
! members of a variable on the same construct.
!
! For a given construct, the effect of a map clause with the to, from,
! or tofrom map-type is ordered before the effect of a map clause with 
! the alloc map-type.
!
!===-------------------------------------------------------------------------===//
#include "ompvv.F90"

#define N 1024

PROGRAM test_target_mapping_before_alloc
  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none
  INTEGER :: errors
  errors = 0
 
  OMPVV_TEST_OFFLOADING

  OMPVV_TEST_VERBOSE(to_before_alloc() .ne. 0)

  OMPVV_REPORT_AND_RETURN()


CONTAINS
  INTEGER FUNCTION to_before_alloc()
    INTEGER,DIMENSION(N):: a,
    INTEGER:: x, scalar
    errors = 0
    scalar = 80

    DO x = 1, N
      a(x) = x
    END DO

    !$omp target map (alloc: scalar, a) map (to: scalar a)
    IF (scalar.ne.80 .or. a(1).ne.2)
      errors = errors + 1
    END IF

    test_before_alloc = errors
  END FUNCTION to_before_alloc
END PROGRAM test_target_mapping_before_alloc 



! GO BACK AND ADD STRUCTURE ! 

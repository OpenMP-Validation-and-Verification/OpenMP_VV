!===--- test_target_defaultmap_present_aggregate.F90 -----------------------===//
!
! OpenMP API Version 5.1 Nov 2021
!
! This test checks behavior of the defaultmap clause when the specified 
! implicit-behavior is present. The variable-categories available for defaultmap
! are scalar, aggregate, and pointer. If implicit-behavior is present, each 
! variable referenced in the construct in the category specified by 
! variable-category is treated as if it had been listed in a map clause with the
! map-type of alloc and map-type-modifier of present.
!
!//===----------------------------------------------------------------------===//

#include "ompvv.F90"

#define N 1024

PROGRAM test_target_defaultmap_present_aggregate
  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none
  OMPVV_TEST_OFFLOADING

  OMPVV_TEST_VERBOSE(defaultmap_present_aggregate() .NE. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION defaultmap_present_aggregate()
    TYPE test_struct
       INTEGER :: s
       INTEGER,DIMENSION(2) :: SA
    END TYPE test_struct

    INTEGER :: errors, i
    INTEGER, TARGET, DIMENSION(N) :: a !aggregate

    TYPE(test_struct) :: new_struct !aggregate

    errors = 0

    !Initialize aggregate array
    DO i = 1, N
        a(i) = i
    END DO

    !Initialize struct and associated members
    new_struct%s = 10; new_struct%SA(1) = 100; new_struct%SA(2) = 1000

    !$omp target data map(tofrom: a, new_struct)
    !$omp target map(tofrom: errors) defaultmap(present:aggregate)
    DO i = 1, N
        IF (a(i) .NE. i) then
            errors = errors + 1
        END IF 
        a(i) = a(i) + 2
    END DO
    IF ((new_struct%s .NE. 10) .OR. (new_struct%SA(1) .NE. 100) .OR. (new_struct%SA(2) .NE. 1000)) then
        errors = errors + 1
    END IF 
    new_struct%s = 7; new_struct%SA(1) = 70; new_struct%SA(2) = 700
    !$omp end target
    !$omp end target data 

    OMPVV_ERROR_IF(errors .GT. 0, "Values were not mapped to the device properly")

    DO i = 1, N
        OMPVV_TEST_AND_SET(errors, a(i) .NE. 2+i)
    END DO

    OMPVV_TEST_AND_SET(errors, (new_struct%s .NE. 7) .OR.  (new_struct%SA(1) .NE. 70) .OR. (new_struct%SA(2) .NE. 700))

    defaultmap_present_aggregate = errors
  END FUNCTION defaultmap_present_aggregate
END PROGRAM test_target_defaultmap_present_aggregate

!===--- test_target_defaultmap_present_pointer.F90 ---------------------- -===//
!
! OpenMP API Version 5.1 Nov 2020
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

PROGRAM test_target_defaultmap_present_pointer
  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none
  OMPVV_TEST_OFFLOADING

  OMPVV_TEST_VERBOSE(defaultmap_present_pointer() .NE. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION defaultmap_present_pointer()
    INTEGER :: errors, i
    INTEGER, TARGET, DIMENSION(N) :: A !aggregate
    INTEGER, POINTER :: ptr(:) !pointer

    errors = 0
    ptr => A

    DO i = 1, N
        A(i) = i
    END DO

    !$omp target enter data map(to: ptr)
    !$omp target map(tofrom: errors) defaultmap(present:pointer)
    DO i = 1, N
        IF (ptr(i) .NE. i) then
            errors = errors + 1
        END IF 
        ptr(i) = i + 2
    END DO
    !$omp end target
    !$omp target exit data map(delete: ptr)

    OMPVV_ERROR_IF(errors .GT. 0, "Values were not mapped to the device properly")

    DO i = 1, N
        OMPVV_TEST_AND_SET(errors, A(i) .NE. 2+i)
    END DO

    defaultmap_present_pointer = errors
  END FUNCTION defaultmap_present_pointer
END PROGRAM test_target_defaultmap_present_pointer

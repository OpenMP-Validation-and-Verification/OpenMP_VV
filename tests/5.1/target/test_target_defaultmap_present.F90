!===--- test_target_defaultmap_present.F90 -------------------------------===//
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

PROGRAM test_target_defaultmap_present
  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none
  OMPVV_TEST_OFFLOADING

  OMPVV_TEST_VERBOSE(defaultmap_present() .NE. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION defaultmap_present()
    TYPE test_struct
       INTEGER :: s
       INTEGER,DIMENSION(N) :: SA
    END TYPE test_struct

    INTEGER :: errors, i
    INTEGER :: scalar_var !scalar
    INTEGER, TARGET, DIMENSION(N) :: A !aggregate
    INTEGER, POINTER :: ptr(:) !pointer

    TYPE(test_struct) :: new_struct !aggregate

    errors = 0
    scalar_var = 1
    A(1) = 0; A(51) = 50
    new_struct%s = 10; new_struct%SA(1) = 10; new_struct%SA(2) = 10
    ptr => A
    ptr(51) = 50; ptr(52) = 51

    !$omp target enter data map(to: scalar_var, A, new_struct, ptr)

    !$omp target map(tofrom: errors) defaultmap(present)
    IF (scalar_var .NE. 1) then
        errors = errors + 1
    END IF 
    IF (A(1) .NE. 0) then
        errors = errors + 1
    END IF 
    IF ((A(51) .NE. 50) .OR. (A(52) .NE. 51)) then
        errors = errors + 1
    END IF 
    IF (new_struct%s .NE. 10) then
        errors = errors + 1
    END IF 
    IF ((new_struct%SA(1) .NE. 10) .OR. (new_struct%SA(2) .NE. 10)) then
        errors = errors + 1
    END IF 
    scalar_var = 7
    A(1) = 70; A(51) = 150
    new_struct%s = 110; new_struct%SA(1) = 110; new_struct%SA(2) = 110
    ptr => A
    ptr(51) = 150; ptr(52) = 151
    !$omp end target

    OMPVV_TEST_AND_SET(errors, scalar_var .EQ. 7)
    OMPVV_TEST_AND_SET(errors, A(1) .EQ. 70 .OR. A(51) .EQ. 150 .OR. A(52) .EQ. 151) 
    OMPVV_TEST_AND_SET(errors, new_struct%s .EQ. 110 .OR. new_struct%SA(1) .EQ. 110 .OR. new_struct%SA(2) .EQ. 110)

    !$omp target exit data map(delete: scalar_var, A, new_struct, ptr)

    defaultmap_present = errors
  END FUNCTION defaultmap_present
END PROGRAM test_target_defaultmap_present

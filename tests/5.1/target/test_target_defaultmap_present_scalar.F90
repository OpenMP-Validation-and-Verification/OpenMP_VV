!===--- test_target_defaultmap_present_scalar.F90 ---------------------- -===//
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

PROGRAM test_target_defaultmap_present_scalar
  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none
  OMPVV_TEST_OFFLOADING

  OMPVV_TEST_VERBOSE(defaultmap_present_scalar() .NE. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION defaultmap_present_scalar()
    INTEGER :: errors, scalar_var
    REAL :: float_var
    DOUBLE PRECISION :: double_var

    errors = 0
    scalar_var = 1
    float_var = 10.7
    double_var = 1.222D1

    !$omp target enter data map(to: scalar_var, float_var, double_var)
    !$omp target map(tofrom: errors) defaultmap(present:scalar)
    IF (scalar_var .NE. 1) then
      errors = errors + 1
    END IF 
    IF (float_var .NE. 10.7) then
      errors = errors + 1
    END IF 
    IF (double_var .NE. 1.222D1) then
      errors = errors + 1
    END IF 

    scalar_var = 7
    float_var = 20.1
    double_var = 5.555D1
    !$omp end target
    !$omp target exit data map(delete: scalar_var, float_var, double_var)

    OMPVV_ERROR_IF(errors .GT. 0, "Values were not mapped to the device properly")

    OMPVV_TEST_AND_SET(errors, scalar_var .EQ. 7)
    OMPVV_TEST_AND_SET(errors, float_var .EQ. 20.1)
    OMPVV_TEST_AND_SET(errors, double_var .EQ. 5.555D1)

    defaultmap_present_scalar = errors
  END FUNCTION defaultmap_present_scalar
END PROGRAM test_target_defaultmap_present_scalar

!===--- test_target_is_accessible.F90 -------------------------------===//
!
! OpenMP API Version 5.1 Nov 2020
!
! This test checks that the omp_target_is_accessible device routine.
! In this test the output of the target_is_accessible call should return
! true because the storage indicated by the first and second arguements
! is accessible by the targeted device. This test is closely adapdted
! from the 5.1 OpenMP example sheet.
!
!//===---------------------------------------------------------------------===//

#include "ompvv.F90"

PROGRAM test_target_is_accessible
  USE iso_fortran_env
  USE, INTRINSIC :: iso_c_binding
  USE ompvv_lib
  USE omp_lib
  implicit none
  OMPVV_TEST_OFFLOADING

  OMPVV_TEST_VERBOSE(check_device() .NE. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION check_device()
    INTEGER :: errors, i, N
    INTEGER, POINTER :: fptr(:)
    TYPE (C_PTR) :: ptr
    INTEGER (C_SIZE_T) :: buf_size
    INTEGER (C_INT) :: dev, check_test

    errors = 0
    check_test = 2
    N = 100
    dev = omp_get_default_device()

    ALLOCATE(fptr(N))

    ptr = c_loc(fptr(1))
    buf_size = c_sizeof(fptr(1)) * N

    check_test = omp_target_is_accessible(ptr, buf_size, dev)

    IF( check_test .NE. 0 ) THEN
      !$omp target firstprivate(fptr)
      DO i=1, N
        fptr(i) = 5*i
      END DO
      !$omp end target
      DO i=1, N
        OMPVV_TEST_AND_SET(errors, fptr(i) .NE. 5*i)
      END DO
    END IF

    DEALLOCATE(fptr)
    OMPVV_INFOMSG_IF(check_test .EQ. 1, "omp_target_is_accessible returning true")
    OMPVV_INFOMSG_IF(check_test .EQ. 0, "omp_target_is_accessible returning false")
    OMPVV_ERROR_IF(check_test .EQ. 2, "omp_target_is_accessible did not return true or false")
    check_device = errors
  END FUNCTION check_device
END PROGRAM test_target_is_accessible


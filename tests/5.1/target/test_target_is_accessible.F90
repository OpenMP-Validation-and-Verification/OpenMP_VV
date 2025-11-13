!===--- test_target_is_accessible.F90 -------------------------------===//
!
! OpenMP API Version 5.1 Nov 2020
!
! This test checks that the omp_target_is_accessible device routine.
! In this test if the output of the target_is_accessible returns
! true then the pointer on the host should be a valid pointer in the 
! device environment.
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
    ELSE
      OMPVV_TEST_AND_SET(errors,check_test.EQ.0)
      OMPVV_WARNING_IF(check_test.EQ.0, "omp_target_is_accessible returned false. This test will be skipped.\n");
    END IF

    DEALLOCATE(fptr)
    OMPVV_INFOMSG_IF(check_test .NE. 0, "omp_target_is_accessible returned true")
    OMPVV_INFOMSG_IF(check_test .EQ. 0, "omp_target_is_accessible returned false")
    check_device = errors
  END FUNCTION check_device
END PROGRAM test_target_is_accessible


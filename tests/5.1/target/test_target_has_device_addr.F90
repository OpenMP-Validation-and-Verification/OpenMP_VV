!===--- test_target_has_device_addr.F90 ------------------------------------===//
!
! OpenMP API Version 5.1 Nov 2020
!
! This test verifies the 'has_device_addr' feature added to the target construct.
! We tested this by mapping a scalar & array to the device
! and ensuring that the address does not change when using
! has_device_addr on another target region.
!
!//===----------------------------------------------------------------------===//

#include "ompvv.F90"

#define N 1024

PROGRAM test_target_has_device_addr
  USE iso_fortran_env
  USE iso_c_binding
  USE ompvv_lib
  USE omp_lib
  implicit none
  OMPVV_TEST_OFFLOADING

  OMPVV_TEST_VERBOSE(target_has_device_addr() .NE. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION target_has_device_addr()
    INTEGER :: errors, i
    INTEGER, TARGET :: x
    INTEGER, TARGET, DIMENSION(N) :: arr
    INTEGER, POINTER :: first_scalar_device_addr
    INTEGER, POINTER :: first_arr_device_addr(:)
    INTEGER, POINTER :: second_scalar_device_addr
    INTEGER, POINTER :: second_arr_device_addr(:)
    INTEGER, POINTER :: third_scalar_device_addr
    INTEGER, POINTER :: third_arr_device_addr(:)
    type(c_ptr) :: cptr_scalar1, cptr_arr1
    type(c_ptr) :: cptr_scalar2, cptr_arr2
    type(c_ptr) :: cptr_scalar3, cptr_arr3

    nullify (first_scalar_device_addr, first_arr_device_addr)
    nullify (second_scalar_device_addr, second_arr_device_addr)
    nullify (third_scalar_device_addr, third_arr_device_addr)

    errors = 0
    x = 10
    DO i=1, N
      arr(i) = i
    END DO

    OMPVV_INFOMSG("test_target_has_device_addr")

    !$omp target enter data map(to: x, arr)
    !$omp target data use_device_addr(x, arr)
      cptr_scalar1 = c_loc(x)
      cptr_arr1 = c_loc(arr)
    !$omp end target data
    call c_f_pointer (cptr_scalar1, first_scalar_device_addr)
    call c_f_pointer (cptr_arr1, first_arr_device_addr, shape(arr))

    !$omp target map(from: cptr_scalar2, cptr_arr2) map(to: x, arr) &
    !$omp&       has_device_addr(first_scalar_device_addr, first_arr_device_addr) &
    !$omp&       map(errors)
      if (.not. associated(first_scalar_device_addr, x)) errors = errors + 1
      if (.not. associated(first_arr_device_addr, arr)) errors = errors + 1
      cptr_scalar2 = c_loc(x)
      cptr_arr2 = c_loc(arr)
    !$omp end target
    OMPVV_TEST_AND_SET_VERBOSE(errors, .not. c_associated(cptr_scalar1, cptr_scalar2))
    OMPVV_TEST_AND_SET_VERBOSE(errors, .not. c_associated(cptr_arr1, cptr_arr2))

    call c_f_pointer (cptr_scalar2, second_scalar_device_addr)
    call c_f_pointer (cptr_arr2, second_arr_device_addr, shape(arr))

    ! check addresses are same on device region
    !$omp target data use_device_addr(x, arr)
    !$omp target map(from: cptr_scalar3, cptr_arr3) has_device_addr(x, arr) &
    !$omp&       has_device_addr(first_scalar_device_addr, first_arr_device_addr) &
    !$omp&       has_device_addr(second_scalar_device_addr, second_arr_device_addr) &
    !$omp&       map(errors)
      if (.not. associated(first_scalar_device_addr, x)) errors = errors + 1
      if (.not. associated(first_arr_device_addr, arr)) errors = errors + 1
      if (.not. associated(second_scalar_device_addr, x)) errors = errors + 1
      if (.not. associated(second_arr_device_addr, arr)) errors = errors + 1
      cptr_scalar3 = c_loc(x)
      cptr_arr3 = c_loc(arr)
    !$omp end target
    !$omp end target data
    !$omp target exit data map(release: x, arr)

    OMPVV_TEST_AND_SET_VERBOSE(errors, .not. c_associated(cptr_scalar3, cptr_scalar2))
    OMPVV_TEST_AND_SET_VERBOSE(errors, .not. c_associated(cptr_arr3, cptr_arr2))

    call c_f_pointer (cptr_scalar3, third_scalar_device_addr)
    call c_f_pointer (cptr_arr3, third_arr_device_addr, shape(arr))

    OMPVV_TEST_AND_SET_VERBOSE(errors, .not. associated(first_scalar_device_addr, target=second_scalar_device_addr))
    OMPVV_TEST_AND_SET_VERBOSE(errors, .not. associated(first_arr_device_addr, target=second_arr_device_addr))
    OMPVV_TEST_AND_SET_VERBOSE(errors, .not. associated(third_scalar_device_addr, target=second_scalar_device_addr))
    OMPVV_TEST_AND_SET_VERBOSE(errors, .not. associated(third_arr_device_addr, target=second_arr_device_addr))

    target_has_device_addr = errors
  END FUNCTION target_has_device_addr
END PROGRAM test_target_has_device_addr

!===--- test_target_map_iterator.F90 ----------------------------------===//
!
! OpenMP API Version 5.1 Nov 2020
!
! This test is designed to test the iterator map-type-modifier for the map
! clause. The test should create a list of size N and then pass to the 
! target region the length of the array 1:N and modify the values.
! The test then checks to see if the appropriate range was modified.
!
!//===-----------------------------------------------------------------===//

#include "ompvv.F90"

#define N 1024

PROGRAM test_target_map_iterator
  USE iso_fortran_env
  USE, INTRINSIC :: iso_c_binding
  USE ompvv_lib
  USE omp_lib
  implicit none
  OMPVV_TEST_OFFLOADING

  OMPVV_TEST_VERBOSE(test_map_iterator() .NE. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION test_map_iterator()
    INTEGER :: errors, i, listsum
    TYPE t
      INTEGER, POINTER :: ptr
    END TYPE t
    TYPE(t), DIMENSION(N) :: test_lst

    errors = 0
    listsum = 0

    DO i=1, N
      allocate(test_lst(i)%ptr)
      test_lst(i)%ptr = 1
    END DO

    !$omp target map(iterator(it = 1:N), tofrom: test_lst(it)%ptr) map(test_lst)
    DO i=1, N
      test_lst(i)%ptr = 2
    END DO
    !$omp end target

    DO i=1, N
      listsum = listsum + test_lst(i)%ptr
      deallocate(test_lst(i)%ptr)
    END DO

    OMPVV_TEST_AND_SET(errors, listsum .NE. 2*N)

    test_map_iterator = errors
  END FUNCTION test_map_iterator
END PROGRAM test_target_map_iterator


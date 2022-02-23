!/===--- test_target_task_depend_mutexinoutset.F90 ---------------------------===//
!
! OpenMP API Version 5.0 Nov 2018
!
! This test verifies the working of the mutexinout on the depend clause.
! Here task T5 will be scheduled after tasks T1 and T3 are completed. Due to 
! the mutexinoutset dependence type on c, T4 and T5 may be scheduled
! in any order with respect to each other, but not at the same time. Tasks 
! T6 will be scheduled after both T4 and T5 are completed.
!
! Adapted from OpenMP Examples 5.0.
!/===----------------------------------------------------------------------===//

#include "ompvv.F90"

#define N 1024

PROGRAM test_target_task_depend_mutexinoutset

  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none

  OMPVV_TEST_OFFLOADING

  OMPVV_TEST_VERBOSE(target_task_depend_mutexinoutset() .ne. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION target_task_depend_mutexinoutset()
    INTEGER:: errors
    INTEGER:: a, b, c, d

    OMPVV_INFOMSG("test_task_mutexinoutset")
    errors = 0

    !$omp target map(from: d)
    !$omp parallel
    !$omp single
      !$omp task depend(out: c)
        c = 1 ! Task T1
      !$omp end task
      !$omp task depend(out: a)
        a = 2 ! Task T2
      !$omp end task
      !$omp task depend(out: b)
        b = 3 ! Task T3
      !$omp end task
      !$omp task depend(in: a) depend(mutexinoutset: c)
        c = c + a ! Task T4
      !$omp end task
      !$omp task depend(in: b) depend(mutexinoutset: c)
        c = c + b ! Task T5
      !$omp end task
      !$omp task depend(out: c)
        d = c ! Task T6
      !$omp end task
    !$omp end single
    !$omp end parallel
    !$omp end target

    OMPVV_TEST_AND_SET_VERBOSE(errors, d .ne. 6)

    target_task_depend_mutexinoutset = errors
  END FUNCTION target_task_depend_mutexinoutset
END PROGRAM test_target_task_depend_mutexinoutset

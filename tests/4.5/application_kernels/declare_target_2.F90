!===--- declare_target_2.F90 -----------------------------------------------===//
!
! OpenMP API Version 4.5 Nov 2015
!
! This test uses a declare target construct to within a subroutine, called
! on the device. It takes in an array argument and checks that the array
! is summed correctly. This test was provided by the LLNL FGPU repository,
! derived from the OpenMP 4.5 examples document.
!
!===------------------------------------------------------------------------===//


#include "ompvv.F90"

#define N 1024

PROGRAM declare_target_2
  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none
  INTEGER:: sum
  sum = 0

  OMPVV_TEST_OFFLOADING

  OMPVV_TEST_SHARED_ENVIRONMENT

  CALL main(sum)

  OMPVV_TEST_VERBOSE(sum .ne. N)

  OMPVV_REPORT_AND_RETURN()
END PROGRAM declare_target_2

SUBROUTINE main(sum)
  INTEGER,POINTER:: a(:)
  INTEGER,INTENT(inout):: sum
  INTEGER:: x

  INTERFACE
     SUBROUTINE test_declare(a, sum)
       INTEGER,POINTER,INTENT(in) :: a(:)
       INTEGER,INTENT(inout):: sum
     END SUBROUTINE test_declare
  END INTERFACE
  
  !$omp declare target(test_declare)

  ALLOCATE(a(N))
  DO x = 1, N
     a(x) = 1
  END DO

  !$omp target map(tofrom: sum) map(to: a)
  CALL test_declare(a, sum)
  !$omp end target
END SUBROUTINE main

SUBROUTINE test_declare(a, sum)
  INTEGER,POINTER,INTENT(in) :: a(:)
  INTEGER,INTENT(inout):: sum
  INTEGER:: x
  !$omp declare target
  DO x = 1, N
     sum = sum + a(x)
  END DO
END SUBROUTINE test_declare

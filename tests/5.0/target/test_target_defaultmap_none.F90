!===--- test_target_defaultmap_none.F90 -------------------------------===//
!
! OpenMP API Version 5.0 Nov 2018
!
!   This test checks behavior of the defaultmap clause when the specified implicit-behavior  
!   is none. The variable-categories avaiable for defaultmap are scalar, aggregate, allocatable,
!   and pointer. When defaultmap(none) is used alone without a variable-category, then none is 
!   the implicit-behavior for all variables referenced in the construct. Since none is used, 
!   the data mapping attributes must be specified somewhere else. In this case, they are 
!   specified using the map clause.
!
!//===----------------------------------------------------------------------===//
#include "ompvv.F90"

#define N 1024

PROGRAM test_target_defaultmap_none
  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none
  OMPVV_TEST_OFFLOADING

  OMPVV_TEST_VERBOSE(defaultmap_none() .ne. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION defaultmap_none()
    TYPE structure
       INTEGER :: s
       INTEGER,DIMENSION(N) :: SA
    END TYPE structure

    INTEGER :: errors, i
    INTEGER :: scalar !scalar
    INTEGER, TARGET, DIMENSION(N) :: A !aggregate
    INTEGER, ALLOCATABLE :: B(:) !allocatable
    INTEGER, POINTER :: ptr(:) !pointer


    TYPE(structure) :: new_struct !aggregate
    ALLOCATE(B(N))

    ptr => null()
    scalar = 1
    new_struct%s = 1

    DO i = 1, N
       A(i) = 0
       B(i) = 0
       new_struct%SA(i) = 0
    END DO

    errors = 0

    !$omp target defaultmap(none) map(tofrom: scalar, A, B, new_struct, ptr)
    scalar = 17 !scalar variable, default is firstprivate
    A(1) = 5; A(2) = 5 ! aggregate array, default is tofrom
    B(1) = 5; B(2) = 5 ! allocatable array, default is tofrom
    ! aggregate structure, default is tofrom
    new_struct%s = 10; new_struct%SA(1) = 10; new_struct%SA(2) = 10
    ptr => A
    ptr(51) = 50; ptr(52) = 51
    ptr => null()
    !$omp end target

    OMPVV_TEST_AND_SET_VERBOSE(errors, scalar /= 17)
    OMPVV_TEST_AND_SET_VERBOSE(errors, A(1) /= 5 .OR. A(2) /= 5) 
    OMPVV_TEST_AND_SET_VERBOSE(errors, A(51) /= 50 .OR. A(52) /= 51)
    OMPVV_TEST_AND_SET_VERBOSE(errors, B(1) /= 5 .OR. B(2) /= 5) 
    OMPVV_TEST_AND_SET_VERBOSE(errors, new_struct%s /= 10)
    OMPVV_TEST_AND_SET_VERBOSE(errors, new_struct%SA(1) /= 10)
    OMPVV_TEST_AND_SET_VERBOSE(errors, new_struct%SA(2) /= 10)

    !$omp target defaultmap(none) map(to: scalar, A, B, new_struct, ptr)
    scalar = scalar + 10 ! scalar variable, default is firstprivate
    A(1) = A(1) + 10; A(2) = A(2) + 10 ! aggregate array, default is tofrom
    B(1) = B(1) + 10; B(2) = B(2) + 10 ! allocatable array, default is tofrom
    ! aggregate structure, default is tofrom
    new_struct%s = new_struct%s + 10; new_struct%SA(1) = new_struct%SA(1) + 10
    new_struct%SA(2) = new_struct%SA(2) + 10
    ptr => A
    ptr(51) = ptr(51) + 10; ptr(52) = ptr(52) + 10
    ptr => null()
    !$omp end target

    OMPVV_TEST_AND_SET_VERBOSE(errors, scalar /= 17)
    OMPVV_TEST_AND_SET_VERBOSE(errors, A(1) /= 5 .OR. A(2) /= 5) 
    OMPVV_TEST_AND_SET_VERBOSE(errors, A(51) /= 50 .OR. A(52) /= 51)
    OMPVV_TEST_AND_SET_VERBOSE(errors, B(1) /= 5 .OR. B(2) /= 5) 
    OMPVV_TEST_AND_SET_VERBOSE(errors, new_struct%s /= 10)
    OMPVV_TEST_AND_SET_VERBOSE(errors, new_struct%SA(1) /= 10)
    OMPVV_TEST_AND_SET_VERBOSE(errors, new_struct%SA(2) /= 10)

    defaultmap_none = errors
  END FUNCTION defaultmap_none
END PROGRAM test_target_defaultmap_none

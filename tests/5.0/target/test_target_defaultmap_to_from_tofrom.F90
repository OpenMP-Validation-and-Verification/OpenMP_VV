!===--- test_target_defaultmap_to_from_tofrom.F90 -------------------------------===//
!
! OpenMP API Version 5.0 Nov 2018
!
! This test checks behavior of the defaultmap clause when the specified 
! implicit-behavior is to, from, and tofrom. The variable-categories 
! available for defaultmap are scalar, aggregate, allocatable, 
! and pointer.
!
!//===----------------------------------------------------------------------===//
#include "ompvv.F90"

#define N 1024

PROGRAM test_target_defaultmap_to_from_tofrom
  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none
  OMPVV_TEST_OFFLOADING

  OMPVV_TEST_VERBOSE(defaultmap_with_to() .ne. 0)
  OMPVV_TEST_VERBOSE(defaultmap_with_from() .ne. 0)
  OMPVV_TEST_VERBOSE(defaultmap_with_tofrom() .ne. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION defaultmap_with_to()
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

    scalar = 1
    new_struct%s = 1

    DO i = 1, N
       A(i) = 0
       B(i) = 0
       new_struct%SA(i) = 0
    END DO

    errors = 0

    !$omp target defaultmap(to)
    scalar = 17 !scalar firstprivate, value not returned
    A(1) = 5; A(2) = 5 ! aggregate array, default is tofrom
    B(1) = 5; B(2) = 5 ! allocatable array, default is tofrom
    ! aggregate structure, default is tofrom
    new_struct%s = 10; new_struct%SA(1) = 10; new_struct%SA(2) = 10
    ptr => A
    ptr(51) = 70; ptr(52) = 71
    !$omp end target

    OMPVV_TEST_AND_SET_VERBOSE(errors, scalar /= 1)
    OMPVV_TEST_AND_SET_VERBOSE(errors, A(1) /= 0 .OR. A(2) /= 0) 
    OMPVV_TEST_AND_SET_VERBOSE(errors, A(51) /= 0 .OR. A(52) /= 0)
    OMPVV_TEST_AND_SET_VERBOSE(errors, B(1) /= 0 .OR. B(2) /= 0) 
    OMPVV_TEST_AND_SET_VERBOSE(errors, new_struct%s /= 0)
    OMPVV_TEST_AND_SET_VERBOSE(errors, new_struct%SA(1) /= 0)
    OMPVV_TEST_AND_SET_VERBOSE(errors, new_struct%SA(2) /= 0)

    defaultmap_with_to = errors
  END FUNCTION defaultmap_with_to

  INTEGER FUNCTION defaultmap_with_from()
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

    scalar = 1
    new_struct%s = 1

    DO i = 1, N
       A(i) = 0
       B(i) = 0
       new_struct%SA(i) = 0
    END DO

    errors = 0

    !$omp target defaultmap(from)
    scalar = 17 !scalar firstprivate, value not returned
    A(1) = 5; A(2) = 5 ! aggregate array, default is tofrom
    B(1) = 5; B(2) = 5 ! allocatable array, default is tofrom
    ! aggregate structure, default is tofrom
    new_struct%s = 10; new_struct%SA(1) = 10; new_struct%SA(2) = 10
    ptr => A
    ptr(51) = 50; ptr(52) = 51
    !$omp end target

    OMPVV_TEST_AND_SET_VERBOSE(errors, scalar /= 17)
    OMPVV_TEST_AND_SET_VERBOSE(errors, A(1) /= 5 .OR. A(2) /= 5) 
    OMPVV_TEST_AND_SET_VERBOSE(errors, A(51) /= 50 .OR. A(52) /= 51)
    OMPVV_TEST_AND_SET_VERBOSE(errors, B(1) /= 5 .OR. B(2) /= 5) 
    OMPVV_TEST_AND_SET_VERBOSE(errors, new_struct%s /= 10)
    OMPVV_TEST_AND_SET_VERBOSE(errors, new_struct%SA(1) /= 10)
    OMPVV_TEST_AND_SET_VERBOSE(errors, new_struct%SA(2) /= 10)

    defaultmap_with_from = errors
  END FUNCTION defaultmap_with_from

  INTEGER FUNCTION defaultmap_with_tofrom()
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

    scalar = 1
    new_struct%s = 1

    DO i = 1, N
       A(i) = 0
       B(i) = 0
       new_struct%SA(i) = 0
    END DO

    errors = 0

    !$omp target defaultmap(tofrom)
    scalar = 17 !scalar firstprivate, value not returned
    A(1) = 5; A(2) = 5 ! aggregate array, default is tofrom
    B(1) = 5; B(2) = 5 ! allocatable array, default is tofrom
    ! aggregate structure, default is tofrom
    new_struct%s = 10; new_struct%SA(1) = 10; new_struct%SA(2) = 10
    ptr => A
    ptr(51) = 50; ptr(52) = 51
    !$omp end target

    OMPVV_TEST_AND_SET_VERBOSE(errors, scalar /= 17)
    OMPVV_TEST_AND_SET_VERBOSE(errors, A(1) /= 5 .OR. A(2) /= 5) 
    OMPVV_TEST_AND_SET_VERBOSE(errors, A(51) /= 50 .OR. A(52) /= 51)
    OMPVV_TEST_AND_SET_VERBOSE(errors, B(1) /= 5 .OR. B(2) /= 5) 
    OMPVV_TEST_AND_SET_VERBOSE(errors, new_struct%s /= 10)
    OMPVV_TEST_AND_SET_VERBOSE(errors, new_struct%SA(1) /= 10)
    OMPVV_TEST_AND_SET_VERBOSE(errors, new_struct%SA(2) /= 10)

    defaultmap_with_tofrom = errors
  END FUNCTION defaultmap_with_tofrom
END PROGRAM test_target_defaultmap_to_from_tofrom

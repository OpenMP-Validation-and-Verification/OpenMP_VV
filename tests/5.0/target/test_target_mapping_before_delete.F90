!===---- test_target_mapping_before_alloc.F90 -------------------------------===//
!
! OpenMP API Version 5.0 Nov 2018
!
! The description of the map clause was modified to clarify the mapping
! order when multiple map-types are specified for a variable or
! structure members of a variable on the same construct.
!
! For a given construct, the effect of a map clause with the to, from, or 
! tofrom map-type is ordered before the effect of a map clause with the delete
! map-type.
!
!===-------------------------------------------------------------------------===//
#include "ompvv.F90"

#define N 1024

PROGRAM test_target_mapping_before_alloc
   USE iso_fortran_env
   USE ompvv_lib
   USE omp_lib
   implicit none
   INTEGER :: errors
   errors = 0

   OMPVV_TEST_OFFLOADING

   OMPVV_TEST_VERBOSE(to_before_delete() .ne. 0)

   OMPVV_TEST_VERBOSE(to_from_before_delete() .ne. 0)


CONTAINS
   INTEGER FUNCTION to_before_delete()
      INTEGER,DIMENSION(N):: a
      INTEGER:: x, y, z, i, scalar, summation
      errors = 0
      scalar = 70

      DO i = 1, N
         a(i) = i 
         summation = summation + 1
      END DO

      !$omp target map(to: scalar, a) map(from: x, y) map(delete: &
      !$omp& scalar, a)
      x = scalar

      DO i = 1, N
         y = y + a(i)
      END DO

      OMPVV_TEST_AND_SET_VERBOSE(errors, x .ne. 70)
      OMPVV_TEST_AND_SET_VERBOSE(errors, y .ne. summation)

      to_before_delete = errors

   END FUNCTION to_before_delete

   INTEGER FUNCTION to_from_before_delete()
      INTEGER, DIMENSION(N):: c
      INTEGER:: x, y, z, i, scalar, summation
      errors = 0
      scalar = 30

      DO i = 1, N
         c(i) = i
         summation = summation + 1
      END DO

      !$omp target map(tofrom: scalar, c) map(from: x, y, z) &
      !$omp& map(delete: scalar, c)

      x = scalar

      DO i = 1, N
         y = c(i)
      END DO
      
      !$omp end target

      OMPVV_TEST_AND_SET_VERBOSE(errors, x .ne. 30)
      OMPVV_TEST_AND_SET_VERBOSE(errors, y .ne. summation)

      to_from_before_delete = errors

   END FUNCTION to_from_before_delete
END PROGRAM test_target_mapping_before_alloc      

! GO BACK AND ADD STRUCTURE !

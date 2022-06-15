!===--- test_requires_unified_address.F90 ----------------------------------===//
!
! OpenMP API Version 5.0 Nov 2018
!
! This test checks for support of unified_address clause on the requires
! directive.
!
!===------------------------------------------------------------------------------===//

#define OMPVV_MODULE_REQUIRES_LINE !$omp requires unified_address
#include "ompvv.F90"

#define N 1024

PROGRAM test_requires_unified_address
   USE iso_fortran_env
   USE ompvv_lib
   USE omp_lib
   implicit none

!$omp requires unified_address

   OMPVV_TEST_OFFLOADING

   OMPVV_TEST_VERBOSE(unified_address() .NE. 0)

   OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION unified_address()
    INTEGER:: errors, i
    INTEGER, ALLOCATABLE:: mem_ptr(:)

    errors = 0

    ALLOCATE(mem_ptr(N))

    OMPVV_ERROR_IF(.NOT. ALLOCATED(mem_ptr), "Memory was not properly allocated")

    !$omp target map(to: mem_ptr)
    DO i = 1, N
      mem_ptr(i) = i + 1;
    END DO
    !$omp end target

    DO i = 1, N
      IF (mem_ptr(i) .NE. (i + 1)) THEN
        errors = errors + 1
      END IF
    END DO
    
    unified_address = errors
  END FUNCTION unified_address
END PROGRAM test_requires_unified_address
      
   

!===--- test_target_update_mapper_from_discontiguous.F90 ----------------------===//
!
! OpenMP API Version 5.0 Nov 2018
!
!  This test seeks to ensure that target update with motion-clause "to"
!  can properly map data to the device by specifying a user-defined
!  mapper. Additionally, the test checks a new addition to target update
!  in OpenMP 5.0 that states "List items in the to or from clauses may
!  include array sections with stride expressions."
!
!  Adopted from OpenMP 5.0 Example target_mapper.1.F90
!===-----------------------------------------------------------------------===//

#include "ompvv.F90"

#define N 1024

module my_struct
  type newvec
    integer                     :: len
    double precision, pointer   :: data(:)
  end type
end module

PROGRAM test_target_update_mapper_from_discontiguous

    USE my_struct
    USE iso_fortran_env
    USE ompvv_lib
    USE omp_lib
    implicit none
    INTEGER :: errors
    errors = 0 

    OMPVV_TEST_OFFLOADING

    OMPVV_TEST_VERBOSE(test_target_update_mapper_from() .ne. 0)
 
    OMPVV_REPORT_AND_RETURN()

CONTAINS
    INTEGER FUNCTION test_target_update_mapper_from()
        INTEGER :: i 
        !$omp declare mapper(newvec :: v)&
        !$omp& map(v, v%data(1:v%len))

        type(newvec) :: s
        allocate(s%data(N))
        s%len = N
        s%data(1:N) = 0.0d0

        !$omp target 
        DO i=1, s%len, 2
           s%data(i) = i
        END DO
        !$omp end target

        !$omp target update from(s)

        DO i=1, s%len, 2
           OMPVV_TEST_AND_SET(errors, s%data(i) /= i)
        END DO

        test_target_update_mapper_from = errors

    END FUNCTION test_target_update_mapper_from
END PROGRAM test_target_update_mapper_from_discontiguous


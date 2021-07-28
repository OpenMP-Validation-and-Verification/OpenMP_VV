!===--- test_target_parallel.F90 --------------------------------------------===//
!
! OpenMP API Version 4.5 Nov 2015
! 
! This test checks for the combined construct target and parallel. It
! creates a parallel region inside of the target devices.
! 
!===-------------------------------------------------------------------------===//

#include "ompvv.F90"

#define N 1024

PROGRAM test_target_parallel
   USE iso_fortran_env
   USE ompvv_lib
   USE omp_lib
   implicit none

   OMPVV_TEST_OFFLOADING

   OMPVV_TEST_VERBOSE(target_parallel() .ne. 0)

   OMPVV_REPORT_AND_RETURN()

   CONTAINS
      INTEGER FUNCTION target_parallel()
         INTEGER :: errors, thread_id, i
         INTEGER, DIMENSION(OMPVV_NUM_THREADS_DEVICE) :: num_threads
         CHARACTER(len=400) :: threadMsg
         errors = 0
         thread_id = 0

         !$omp target parallel num_threads(OMPVV_NUM_THREADS_DEVICE) map(from: num_threads(0:OMPVV_NUM_THREADS_DEVICE))
            thread_id = omp_get_thread_num()
            num_threads(thread_id) = omp_get_num_threads()
         !$omp end target parallel

         OMPVV_WARNING_IF(num_threads(0) .eq. 1, "The number of threads in the parallel region was 1. &
                 &This is not a specifications error but we could not confirm the parallel region.")

         DO i = 2, num_threads(0)
            OMPVV_TEST_AND_SET(errors, num_threads(i) .ne. num_threads(0))
            WRITE (threadMsg, *) "The number of threads recorded by thread", i, &
                     &" was", num_threads(i), ". Expected was", num_threads(0) 
            OMPVV_INFOMSG(threadMsg)
         END DO

      target_parallel = errors
      END FUNCTION target_parallel
END PROGRAM test_target_parallel


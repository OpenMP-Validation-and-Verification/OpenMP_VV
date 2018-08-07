!===---- test_target_if.F90 -  ------------------------------------------===//
! 
! OpenMP API Version 4.5 Nov 2015
! 
! The if clause determines if the section should be executed in the host or 
! the device. There are three things to test here: 
! (a) with offloading when 'if' clause evaluates to true then code
! be executed on the device 
! (b) with offloading when 'if' clause evaluates to false then code should
! be executed on the host
! (c) without offloading all the code should be executed on the device
! The if clause is evaluated on runtime which means that variables could
! determine this behavior. We use a SIZE_THRESHOLD variable to check if we 
! should execute on the device or the host. Before starting the test we 
! sample offloading to see if it was enabled or not. If the code is executed
! in the device, the result should be c(i) = a(i) + b(i) = i + 1. 
! If the code is executed on the host the result should be c(i) = -1
!
!===----------------------------------------------------------------------===//

#include "ompvv.F90" 

#define SIZE_ARRAY 1024
#define THRESHOLD 512

      PROGRAM test_target_if
        USE iso_fortran_env
        USE ompvv_lib
        USE omp_lib
        implicit none
       
         LOGICAL :: isOffloading, isHost
         INTEGER :: a(1:SIZE_ARRAY)
         INTEGER :: b(1:SIZE_ARRAY)
         INTEGER :: c(1:SIZE_ARRAY)
         INTEGER :: alpha, errors(2), i, j, s
         CHARACTER(len=300) :: infoMessage
         
         OMPVV_TEST_AND_SET_OFFLOADING(isOffloading)

         ! warning if offloading is off
         IF (.NOT. isOffloading) THEN 
           WRITE(infoMessage, *) "Offloading is off. Not possible&
           & to test if clause"
           OMPVV_WARNING(infoMessage)
         END IF

         ! a and b array initialization
         a(:) = 1
         b(:) = (/ (i - 1, i = 1, SIZE_ARRAY) /) 
       
         ! check multiple sizes. 
         DO s = 256, 1024, 256
           ! init C
             c(:) = -1
             !$omp target if(s > THRESHOLD) &
             !$omp map(to: s, a(1:s), b(1:s)) &
             !$omp map(tofrom: c(1:s), isHost)
               isHost = omp_is_initial_device()
               alpha = 1;
               IF (isHost) alpha = 0
               ! c(j) is zero if executed in the host
               ! c(j) is 1+j if executed on the device
               c(1:s) = (/ (alpha*(a(j) + b(j)), j = 1, s) /)
             !$omp end target 
       
           ! checking results 
           DO i = 1, s
             IF (isOffloading .AND. s > THRESHOLD) THEN
               ! Should have executed on the device
               ! if offloading was used
               ! c(i) is zero if it was executed in the host
               ! error when executed on the device
               OMPVV_TEST_AND_SET(errors(1), (c(i) /= i)) 
             ELSE
               ! Should have executed in the host
               ! with or without offloading
               OMPVV_TEST_AND_SET(errors(2), (c(i) /= 0))
             END IF 
           END DO ! i
         END DO ! s
      
         infoMessage = MERGE("enabled ", "disabled", isOffloading)
           IF (errors(1) == 0 .AND. errors(2) == 0) THEN 
             infoMessage = "Test passed with offloading "//infoMessage
             OMPVV_INFOMSG(infoMessage)
           ELSE IF (errors(1) == 0 .AND. errors(2) /= 0) THEN
             infoMessage = "Test failed on the host &
             & with offloading "//infoMessage
             OMPVV_ERROR(infoMessage)
           ELSE IF (errors(1) /= 0 .AND. errors(2) == 0) THEN
             infoMessage = "Test failed on the device &
             & with offloading "//infoMessage
             OMPVV_ERROR(infoMessage)
           ELSE IF (errors(1) /= 0 .AND. errors(2) /=0) THEN
             infoMessage = "Test failed on both host and device &
             & with offloading "//infoMessage
             OMPVV_ERROR(infoMessage)
           END IF
           
         OMPVV_REPORT_AND_RETURN()
      END PROGRAM test_target_if

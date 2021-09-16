!===--- test_target_teams_distribute_parallel_for_devices.F90 ---------------===//
!
! OpenMP API Version 4.5 Nov 2015
! 
! Testing for multiple devices checking if it is possible to send work and data 
! to different devices with the device clause used with omp target teams distribute 
! parallel for
! 
!===-------------------------------------------------------------------------===//

#include "ompvv.F90"

#define N 1024

PROGRAM test_target_teams_distribute_parallel_for_devices
   USE iso_fortran_env
   USE ompvv_lib
   USE omp_lib
   implicit none

   OMPVV_TEST_OFFLOADING

   OMPVV_TEST_VERBOSE(target_teams_distribute_parallel_for_devices() .ne. 0)

   OMPVV_REPORT_AND_RETURN()

CONTAINS
   INTEGER FUNCTION target_teams_distribute_parallel_for_devices()
      INTEGER :: num_dev, errors, i, dev
      INTEGER, DIMENSION(N) :: a
      LOGICAL, DIMENSION(N) :: isHost
      CHARACTER(len=400) :: numDeviceMsg, hostOrDevMsg

      errors = 0

      num_dev = omp_get_num_devices()
      OMPVV_WARNING_IF(num_dev .le. 1, "Testing devices clause without&
                                        & multiple devices")

      WRITE(numDeviceMsg, *) "Number of devices =", num_dev
      OMPVV_INFOMSG(numDeviceMsg)

      DO i = 1, N
         a(i) = 1
      END DO

      DO dev = 1, num_dev
         !$omp target enter data map(to: a) device(dev)
      END DO

      DO dev = 1, num_dev
      !$omp target teams distribute parallel do device(dev) map(tofrom: isHost)
            DO i = 1, N
               IF ((omp_get_team_num() .eq. 0) .and. (omp_get_thread_num() .eq. 0)) THEN
                  isHost(dev) = omp_is_initial_device()
               END IF
               a(i) = a(i) + dev
            END DO
      END DO

      DO dev = 1, num_dev
         !$omp target exit data map(from: a) device(dev)
         IF (isHost(dev) .eqv. .true.) THEN 
            WRITE(hostOrDevMsg, *) "Device", dev, "ran on the host"
         END IF
         IF (isHost(dev) .eqv. .false.) THEN
            WRITE(hostOrDevMsg, *) "Device", dev, "ran on the device"
         END IF
         OMPVV_INFOMSG(hostOrDevMsg)
         DO i = 1, N
            OMPVV_TEST_AND_SET(errors, a(i) .ne. (1 + dev))
         END DO
      END DO
       

      target_teams_distribute_parallel_for_devices = errors
   END FUNCTION target_teams_distribute_parallel_for_devices
END PROGRAM test_target_teams_distribute_parallel_for_devices


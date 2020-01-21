!===--- array_reduce_v1.F90 ------------------------------------------------===//
!
! OpenMP API Version 4.5 Nov 2015
!
! then separately tests the proper initialization of them separately
!
!//===----------------------------------------------------------------------===//

#include "ompvv.F90"
#define N 768

PROGRAM array_reduce_v1
  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none

  OMPVV_TEST_OFFLOADING
  OMPVV_TEST_SHARED_ENVIRONMENT

  OMPVV_TEST_VERBOSE(test_array_reduce() .ne. 0)

  OMPVV_REPORT_AND_RETURN()
CONTAINS
  INTEGER FUNCTION test_array_reduce()

    ! xl : mpif90 -O3 -qstrict -qsmp=omp -qoffload array_reduce_v1.F90 -o array_reduce.x

    REAL,ALLOCATABLE:: vx(:,:,:)
    REAL,ALLOCATABLE:: rrs(:,:), rrs_host(:,:)
    INTEGER:: nx, nz, nxh, my, errors
    INTEGER:: norder, incr, ir, x, x1, z, yp
    REAL:: term1, term2, sqterm

    errors = 0

    nx = N
    nz = N
    my = 32
    incr = 2

    nxh = N / 2
    norder = 2
    ALLOCATE(vx(nx, nz, my))
    ALLOCATE(rrs(norder, nxh))
    ALLOCATE(rrs_host(norder, nxh))

    rrs(:,:) = 0.
    rrs_host(:,:) = 0.
    DO x = 1, nx
       DO z = 1, nz
          DO yp = 1, my
             vx(x, z, yp) = yp
          END DO
       END DO
    END DO

    !$omp target data map(to:vx) map(from:rrs)
    !$omp target
    rrs(:,:) = 0.
    !$omp end target

    !$omp target teams distribute parallel do collapse(4) default(none) &
    !$omp private(yp, z, x, x1, ir, term1, term2, sqterm) shared(my, &
    !$omp nz, nx, nxh, incr, norder, vx) &
    !$omp reduction(+:rrs)
    DO yp = 1, my, incr
       DO z = 1, nz, incr
          DO ir = 1, nxh, 16
             DO x = 1, nx, incr
                x1 = mod(x + ir, nx)
                IF (x1 .eq. 0) THEN
                   x1 = nx
                END IF
                term1 = vx(x1, z, yp) - vx(x, z, yp)
                term2 = term1*term1
                rrs(1, ir) = rrs(1, ir) + term1
                rrs(2, ir) = rrs(2, ir) + term2
             END DO
          END DO
       END DO
    END DO
    !$omp end target teams distribute parallel do
    !$omp end target data

    DO yp = 1, my, incr
       DO z = 1, nz, incr
          DO ir = 1, nxh, 16
             DO x = 1, nx, incr
                x1 = mod(x + ir, nx)
                IF (x1 .eq. 0) THEN
                   x1 = nx
                END IF
                term1 = vx(x1, z, yp) - vx(x, z, yp)
                term2 = term1*term1
                rrs_host(1, ir) = rrs_host(1, ir) + term1
                rrs_host(2, ir) = rrs_host(2, ir) + term2
             END DO
          END DO
       END DO
    END DO

    DO ir = 1, nxh
       OMPVV_TEST_AND_SET_VERBOSE(errors, ABS(rrs_host(1, ir) - rrs(1, ir)) .gt. .00001)
       OMPVV_TEST_AND_SET_VERBOSE(errors, ABS(rrs_host(2, ir) - rrs(2, ir)) .gt. .00001)
    END DO

    DEALLOCATE(rrs)
    DEALLOCATE(rrs_host)
    DEALLOCATE(vx)

    test_array_reduce = errors
  END FUNCTION test_array_reduce
END PROGRAM array_reduce_v1

!===--- array_reduce_v2.F90 ------------------------------------------------===//
!
! OpenMP API Version 4.5 Nov 2015
!
! This test conducts an array reduction on both the host and device and
! compares the result. The reduction operator is a custom declared add
! operator over reals. The reduction occurs within a four-level collapsed
! loop.
!
!//===----------------------------------------------------------------------===//

#include "ompvv.F90"
#define N 768

MODULE arrayadd_module

  TYPE MyReal
     REAL:: x
  END TYPE MyReal

  INTERFACE operator (+)
     MODULE PROCEDURE:: ArrayAdd
  END INTERFACE operator (+)

  !$omp declare reduction(+ : MyReal : omp_out = omp_out + omp_in) &
  !$omp& initializer (omp_priv = 0.0)

CONTAINS
  FUNCTION ArrayAdd(a1,a2)
    type(MyReal),DIMENSION(:,:),INTENT(IN):: a1,a2
    type(MyReal),DIMENSION(size(a1,1),size(a1,2)):: ArrayAdd
    INTEGER:: i, j, n, m
    !$omp declare target

    n = size(a1,1)
    m = size(a1,2)

    ArrayAdd(:,:)%x = 0.0

    DO j = 1, n
       DO i = 1, m
          ArrayAdd(i, j)%x = a1(i, j)%x + a2(i, j)%x
       END DO
    END DO

  END FUNCTION ArrayAdd
END MODULE arrayadd_module

PROGRAM array_reduce_v2
  USE arrayadd_module
  USE iso_fortran_env
  USE omp_lib
  USE ompvv_lib
  implicit none

  OMPVV_TEST_OFFLOADING
  OMPVV_TEST_SHARED_ENVIRONMENT

  OMPVV_TEST_VERBOSE(test_array_reduce() .ne. 0)

  OMPVV_REPORT_AND_RETURN()
CONTAINS
  INTEGER FUNCTION test_array_reduce()
    REAL,ALLOCATABLE:: vx(:,:,:)
    type(MyReal),ALLOCATABLE:: rrs(:,:), rrs_host(:,:)
    INTEGER:: nx, nz, nxh, my, errors
    INTEGER:: norder, inc, ir, x, x1, z, yp
    REAL:: term1, term2, sqterm

    errors = 0

    nx = N
    nz = N
    my = 32
    inc = 2

    nxh = N / 2
    norder = 2
    ALLOCATE(vx(nx, nz, my))
    ALLOCATE(rrs(norder, nxh))
    ALLOCATE(rrs_host(norder, nxh))

    rrs(:,:)%x = 0.
    rrs_host(:,:)%x = 0.
    DO x = 1, nx
       DO z = 1, nz
          DO yp = 1, my
             vx(x, z, yp) = yp
          END DO
       END DO
    END DO


    !$omp target data map(to:vx) map(tofrom:rrs)
    !$omp target teams distribute parallel do collapse(4) default(none) &
    !$omp private(yp, z, x, x1, ir, term1, term2, sqterm) shared(my, &
    !$omp nz, nx, nxh, inc, norder, vx) &
    !$omp reduction(+:rrs)
    DO yp = 1, my, inc
       DO z = 1, nz, inc
          DO ir = 1, nxh, 16
             DO x = 1, nx, inc
                x1 = mod(x + ir, nx)
                IF (x1 .eq. 0) THEN
                   x1 = nx
                END IF
                term1 = vx(x1, z, yp) - vx(x, z, yp)
                term2 = term1*term1
                sqterm = term2
                rrs(1, ir)%x = rrs(1, ir)%x + term1
                rrs(2, ir)%x = rrs(2, ir)%x + term2
             END DO
          END DO
       END DO
    END DO
    !$omp end target teams distribute parallel do
    !$omp end target data

    DO yp = 1, my, inc
       DO z = 1, nz, inc
          DO ir = 1, nxh, 16
             DO x = 1, nx, inc
                x1 = mod(x + ir, nx)
                IF (x1 .eq. 0) THEN
                   x1 = nx
                END IF
                term1 = vx(x1, z, yp) - vx(x, z, yp)
                term2 = term1*term1
                sqterm = term2
                rrs_host(1, ir)%x = rrs_host(1, ir)%x + term1
                rrs_host(2, ir)%x = rrs_host(2, ir)%x + term2
             END DO
          END DO
       END DO
    END DO

    DO ir = 1, nxh
       OMPVV_TEST_AND_SET_VERBOSE(errors, ABS(rrs_host(1, ir)%x - rrs(1, ir)%x) .gt. .00001)
       OMPVV_TEST_AND_SET_VERBOSE(errors, ABS(rrs_host(2, ir)%x - rrs(2, ir)%x) .gt. .00001)
    END DO

    DEALLOCATE(rrs)
    DEALLOCATE(rrs_host)
    DEALLOCATE(vx)

    test_array_reduce = errors
  END FUNCTION test_array_reduce
END PROGRAM array_reduce_v2

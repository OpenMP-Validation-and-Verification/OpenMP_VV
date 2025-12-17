!===----test_workdistribute_directive.F90-----------------------------------===//
!
! OpenMP API Version 6.0 Nov 2024
! Pg. 901
! This test checks if the workdistribute construct works with target teams in
! various cases which involve array manipulations. The workdistribute Construct 
! example in the 6.0.1 examples document was referenced.
!===------------------------------------------------------------------------===//

#include "ompvv.F90"

module axpy_mod
    implicit none
contains
    subroutine axpy_workdistribute(a, x, y, n)
        implicit none
        integer :: n
        real :: a
        real, dimension(n) :: x
        real, dimension(n) :: y
        !!$omp target teams workdistribute map(to:x) map(tofrom:y)
        !$omp target teams map(to:x) map(tofrom:y)
            y = a * x + y
        !!$omp end target teams workdistribute
        !$omp end target teams 
    end subroutine axpy_workdistribute
end module axpy_mod

module workdistribute_2
  implicit none
contains
  subroutine array_ops(aa, bb, cc, dd, ee, ff, n)
    implicit none

    integer :: n
    real, dimension(n, n) :: aa, bb, cc
    real, dimension(n, n) :: dd, ee, ff
    !!$omp target teams workdistribute map(to:bb,dd,ee) &
    !$omp target teams map(to:bb,dd,ee) &
    !$omp map(tofrom:cc) map(from:aa,ff)
      aa = bb + cc
      cc = dd + ee
      ff = aa + cc
    !!$omp end target teams workdistribute
    !$omp end target teams 
  end subroutine array_ops
end module workdistribute_2

module workdistribute_3
  implicit none
contains
  subroutine array_transform(aa, bb, cc, dd, ee, n)
    implicit none

    integer :: n
    real, dimension(n, n) :: aa, bb, cc, ee
    real, dimension(n) :: dd
    real :: f

    !!$omp target teams workdistribute map(to:bb,cc) &
    !$omp target teams map(to:bb,cc) &
    !$omp map(from:aa,dd,f,ee)
      aa = bb + cc
      dd = sum(aa, 1)
      f = minval(dd)
      ee = aa ** f
    !!$omp end target teams workdistribute
    !$omp end target teams 
  end subroutine array_transform
end module workdistribute_3

program test_omp_workdistribute
    use iso_fortran_env
    use ompvv_lib
    use omp_lib
    use axpy_mod, only: axpy_workdistribute
    use workdistribute_2, only: array_ops
    use workdistribute_3, only: array_transform
    implicit none


    integer :: errors = 0
    integer, parameter :: N = 1024 * 1024

    real :: a
    real :: x0
    real :: y0
    real, dimension(N) :: x
    real, dimension(N) :: y

    !call array_ops(aa1, bb1, cc1, dd1, ee1, ff1, N)
    real, dimension(N, N) :: aa1, bb1, cc1, dd1, ee1, ff1, prior_cc1
    !call array_transform(aa2, bb2, cc2, dd2, ee2, N)
    real, dimension(N, N) :: aa2, bb2, cc2, ee2
    real, dimension(N) :: dd2
    real :: f

    OMPVV_TEST_OFFLOADING

    a = 2.0
    x = 2.0  ! initialize arrays
    y = 1.0
    x0 = 2.0 ! initialize scalars for validation
    y0 = 1.0

    aa1 = 2.0
    bb1 = 2.0
    cc1 = 2.0
    dd1 = 2.0
    ee1 = 2.0
    ff1 = 0.0
    prior_cc1 = cc1

    aa2 = 2.0
    bb2 = 2.0
    cc2 = 2.0
    dd2 = 2.0
    ee2 = 2.0

    call axpy_workdistribute(a, x, y, N)
    IF (sum(y) / N .NE. a * x0 + y0) THEN
      errors = errors + 1
    END IF 

    call array_ops(aa1, bb1, cc1, dd1, ee1, ff1, N)
    IF (sum(bb1 + prior_cc1 + dd1 + ee1) .NE. sum(ff1)) THEN
      errors = errors + 1
    END IF

    call array_transform(aa2, bb2, cc2, dd2, ee2, N)
    IF ( sum((bb2 + cc2) ** minval(sum(bb2, 1) + sum(cc2, 1))) .NE. sum(ee2)) THEN
      errors = errors + 1
    END IF

    OMPVV_ERROR_IF(errors /= 0, "The workdistribute directive did not perform as expected.")
    OMPVV_REPORT_AND_RETURN()
end program test_omp_workdistribute

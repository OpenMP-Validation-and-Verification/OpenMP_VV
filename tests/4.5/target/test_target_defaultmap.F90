!===---- test_target_defaultmap.F90 -  -----------------------------------===//
! 
! OpenMP API Version 4.5 Nov 2015
!
! This tests checks for the default map behavior of scalar varlues, as well
! as the use of hte defaultmap clause to change this behavior.
!
!===----------------------------------------------------------------------===//
#include "ompvv.F90" 

      PROGRAM test_target_defaultmap
        USE iso_fortran_env
        USE ompvv_lib
        USE omp_lib
        implicit none
        
        OMPVV_TEST_OFFLOADING
        OMPVV_TEST_VERBOSE(test_defaultmap_on() .ne. 0)
        OMPVV_TEST_VERBOSE(test_defaultmap_off() .ne. 0)

        OMPVV_REPORT_AND_RETURN()


        CONTAINS 
          LOGICAL FUNCTION same_val_real32 (x, y) RESULT(res)
            REAL(kind = real32) :: x, y
            res = abs(x-y) < min(abs(x) + abs(y), huge(x)) * epsilon(x)
          END FUNCTION
          LOGICAL FUNCTION same_val_real64 (x, y) RESULT(res)
            REAL(kind = real64) :: x, y
            res = abs(x-y) < min(abs(x) + abs(y), huge(x)) * epsilon(x)
          END FUNCTION
          LOGICAL FUNCTION same_val_cmplx (x, y) RESULT(res)
            COMPLEX :: x, y
            REAL :: rx, ry
            REAL :: ix, iy
            rx = real(x)
            ry = real(y)
            ix = aimag(x)
            iy = aimag(y)
            res = (abs(rx-ry) < min(abs(rx) + abs(ry), huge(rx)) * epsilon(rx) &
                   .and. abs(ix-iy) < min(abs(ix) + abs(iy), huge(ix)) * epsilon(ix))
          END FUNCTION
          INTEGER FUNCTION test_defaultmap_on()
            INTEGER :: errors_bf, errors_af
          
            ! we try with all the scalars
            INTEGER(kind = int8) :: scalar_byte
            INTEGER(kind = int16) :: scalar_short
            INTEGER(kind = int32) :: scalar_int
            INTEGER(kind = int64) :: scalar_long_int
            REAL(kind = real32) :: scalar_float
            REAL(kind = real64) :: scalar_double
            LOGICAL :: scalar_logical
            LOGICAL(kind = 4) :: scalar_logical_kind4
            LOGICAL(kind = 8) :: scalar_logical_kind8
            COMPLEX :: scalar_complex
            
            OMPVV_INFOMSG("test_defaultmap_on")
            OMPVV_GET_ERRORS(errors_bf)

            scalar_byte = 8
            scalar_short = 23
            scalar_int = 56
            scalar_long_int = 90000
            scalar_float = 4.5e+3_real32
            scalar_double = 4.9e+10_real64
            scalar_logical = .true.
            scalar_logical_kind4 = .true.
            scalar_logical_kind8 = .true.
            scalar_complex = (1, 1)
          
            ! Map the same array to multiple devices. initialize with device number
            !$omp target defaultmap(tofrom: scalar)
              scalar_byte = 5
              scalar_short = 83
              scalar_int = 49
              scalar_long_int = 12345
              scalar_float = 3.0e+5_real32
              scalar_double = 9.5e+9_real64
              scalar_logical = .false.
              scalar_logical_kind4 = .false.
              scalar_logical_kind8 = .false.
              scalar_complex = (5, 5)
            !$omp end target 
          
            OMPVV_TEST_VERBOSE(scalar_byte /= 5)
            OMPVV_TEST_VERBOSE(scalar_short /= 83)
            OMPVV_TEST_VERBOSE(scalar_int /= 49)
            OMPVV_TEST_VERBOSE(scalar_long_int /= 12345)
            OMPVV_TEST_VERBOSE(.NOT. same_val_real32(scalar_float, 3.0e+5_real32))
            OMPVV_TEST_VERBOSE(.NOT. same_val_real64(scalar_double, 9.5e+9_real64))
            OMPVV_TEST_VERBOSE(scalar_logical .NEQV. .false.)
            OMPVV_TEST_VERBOSE(scalar_logical_kind4 .NEQV. .false.)
            OMPVV_TEST_VERBOSE(LOGICAL(scalar_logical_kind8 .NEQV. .false., kind = 4))
            OMPVV_TEST_VERBOSE(.NOT. same_val_cmplx(scalar_complex, (5, 5)))

            OMPVV_GET_ERRORS(errors_af)
            test_defaultmap_on = errors_bf - errors_af
          END FUNCTION test_defaultmap_on

          INTEGER FUNCTION test_defaultmap_off()
            INTEGER :: errors_bf, errors_af, i, tmp1 = 0, tmp2 = 0
            CHARACTER(len=400) :: longMessage
            CHARACTER(len=100) :: shortMessage
            LOGICAL:: firstprivateCheck(10)
            
            ! we try with all the scalars
            INTEGER(kind = int8) :: scalar_byte
            INTEGER(kind = int16) :: scalar_short
            INTEGER(kind = int32) :: scalar_int
            INTEGER(kind = int64) :: scalar_long_int
            REAL(kind = real32) :: scalar_float
            REAL(kind = real64) :: scalar_double
            LOGICAL :: scalar_logical
            LOGICAL(kind = 4) :: scalar_logical_kind4
            LOGICAL(kind = 8) :: scalar_logical_kind8
            COMPLEX :: scalar_complex
            
            OMPVV_INFOMSG("test_defaultmap_off")
            OMPVV_GET_ERRORS(errors_bf)
            
            scalar_byte = 8
            scalar_short = 23
            scalar_int = 56
            scalar_long_int = 90000
            scalar_float = 4.5e+3_real32
            scalar_double = 4.9e+10_real64
            scalar_logical = .true.
            scalar_logical_kind4 = .true.
            scalar_logical_kind8 = .true.
            scalar_complex = (1, 1)
            
            ! Map the scalar variables without defaultmap
            !$omp target map(tofrom: firstprivateCheck)
              ! checking for firstprivate copy
              firstprivateCheck(1)  = scalar_byte == 8
              firstprivateCheck(2)  = scalar_short == 23
              firstprivateCheck(3)  = scalar_int == 56
              firstprivateCheck(4)  = scalar_long_int == 90000
              firstprivateCheck(5)  = same_val_real32(scalar_float, 4.5e+3_real32)
              firstprivateCheck(6)  = same_val_real64(scalar_double, 4.9e+10_real64)
              firstprivateCheck(7)  = scalar_logical .EQV. .true.
              firstprivateCheck(8)  = scalar_logical_kind4 .EQV. .true.
              firstprivateCheck(9)  = scalar_logical_kind8 .EQV. .true.
              firstprivateCheck(10) = same_val_cmplx (scalar_complex, (1, 1))

              ! Attempting value change
              scalar_byte = 80
              scalar_short = 83
              scalar_int = 49
              scalar_long_int = 12345
              scalar_float  = 3.0e+5_real32
              scalar_double  = 9.5e+9_real64
              scalar_logical = .false.
              scalar_logical_kind4 = .false.
              scalar_logical_kind8 = .false.
              scalar_complex = (5, 5)
            !$omp end target

            ! Check for the firstprivate values
            DO i = 1,10
              OMPVV_GET_ERRORS(tmp1)
              OMPVV_TEST_AND_SET_VERBOSE(tmp2, .not. firstprivateCheck(i))
              write(shortMessage, '(A,I0)') "Error in firstprivate&
              & for i = ", i
              OMPVV_ERROR_IF(tmp1 /= tmp2, shortMessage)
            END DO 
              
            OMPVV_TEST_VERBOSE(scalar_byte /= 8)
            OMPVV_TEST_VERBOSE(scalar_short /= 23)
            OMPVV_TEST_VERBOSE(scalar_int /= 56)
            OMPVV_TEST_VERBOSE(scalar_long_int /= 90000)
            OMPVV_TEST_VERBOSE(.NOT. same_val_real32(scalar_float, 4.5e+3_real32))
            OMPVV_TEST_VERBOSE(.NOT. same_val_real64(scalar_double, 4.9e+10_real64))
            OMPVV_TEST_VERBOSE(scalar_logical .NEQV. .true.)
            OMPVV_TEST_VERBOSE(scalar_logical_kind4 .NEQV. .true.)
            OMPVV_TEST_VERBOSE(LOGICAL(scalar_logical_kind8 .NEQV. .true., kind = 4))
            OMPVV_TEST_VERBOSE(.NOT. same_val_cmplx (scalar_complex, (1, 1)))

            OMPVV_GET_ERRORS(errors_af)
            test_defaultmap_off = errors_bf - errors_af
          END FUNCTION test_defaultmap_off
      END PROGRAM test_target_defaultmap

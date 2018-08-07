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
          INTEGER FUNCTION test_defaultmap_on()
            INTEGER :: errors_bf, errors_af
          
            ! we try with all the scalars
            CHARACTER :: scalar_char
            BYTE :: scalar_byte
            INTEGER(kind = 2) :: scalar_short
            INTEGER(kind = 4) :: scalar_int
            INTEGER(kind = 8) :: scalar_long_int
            REAL(kind = 4) :: scalar_float 
            REAL(kind = 8) :: scalar_double 
            LOGICAL :: scalar_logical
            LOGICAL(kind = 4) :: scalar_logical_kind4
            LOGICAL(kind = 8) :: scalar_logical_kind8
            COMPLEX :: scalar_complex
            COMPLEX(kind = 16) :: scalar_double_complex
            
            OMPVV_INFOMSG("test_defaultmap_on")
            OMPVV_GET_ERRORS(errors_bf)

            scalar_char = 'a'
            scalar_byte = 8
            scalar_short = 23
            scalar_int = 56
            scalar_long_int = 90000
            scalar_float  = 4.5e+3
            scalar_double  = 4.9e+10
            scalar_logical = .true.
            scalar_logical_kind4 = .true.
            scalar_logical_kind8 = .true.
            scalar_complex = (1, 1)
            scalar_double_complex = (1e+10, 1e+10)
          
            ! Map the same array to multiple devices. initialize with device number
            !$omp target defaultmap(tofrom: scalar)
              scalar_char = 'b'
              scalar_byte = 5
              scalar_short = 83
              scalar_int = 49
              scalar_long_int = 12345
              scalar_float  = 3.0e+5
              scalar_double  = 9.5e+9
              scalar_logical = .false.
              scalar_logical_kind4 = .false.
              scalar_logical_kind8 = .false.
              scalar_complex = (5, 5)
              scalar_double_complex = (5e+9, 5e+9)
            !$omp end target 
          
            OMPVV_TEST_VERBOSE(scalar_char /= 'b')
            OMPVV_TEST_VERBOSE(scalar_byte /= 5)
            OMPVV_TEST_VERBOSE(scalar_short /= 83)
            OMPVV_TEST_VERBOSE(scalar_int /= 49)
            OMPVV_TEST_VERBOSE(scalar_long_int /= 12345)
            OMPVV_TEST_VERBOSE(scalar_float  /= 3.0e+5)
            OMPVV_TEST_VERBOSE(scalar_double  /= 9.5e+9)
            OMPVV_TEST_VERBOSE(scalar_logical .NEQV. .false.)
            OMPVV_TEST_VERBOSE(scalar_logical_kind4 .NEQV. .false.)
            OMPVV_TEST_VERBOSE(LOGICAL(scalar_logical_kind8 .NEQV. .false., kind = 4))
            OMPVV_TEST_VERBOSE(scalar_complex /= (5, 5))
            OMPVV_TEST_VERBOSE(scalar_double_complex /= (5e+9, 5e+9))

            OMPVV_GET_ERRORS(errors_af)
            test_defaultmap_on = errors_bf - errors_af
          END FUNCTION test_defaultmap_on

          INTEGER FUNCTION test_defaultmap_off()
            INTEGER :: errors_bf, errors_af, i, tmp1, tmp2
            LOGICAL :: isHost, sharedEnvWarning
            CHARACTER(len=400) :: longMessage
            CHARACTER(len=100) :: shortMessage
            LOGICAL:: firstprivateCheck(12)
            
            ! we try with all the scalars
            CHARACTER :: scalar_char
            BYTE :: scalar_byte
            INTEGER(kind = 2) :: scalar_short
            INTEGER(kind = 4) :: scalar_int
            INTEGER(kind = 8) :: scalar_long_int
            REAL(kind = 4) :: scalar_float 
            REAL(kind = 8) :: scalar_double 
            LOGICAL :: scalar_logical
            LOGICAL(kind = 4) :: scalar_logical_kind4
            LOGICAL(kind = 8) :: scalar_logical_kind8
            COMPLEX :: scalar_complex
            COMPLEX(kind = 16) :: scalar_double_complex
            
            OMPVV_INFOMSG("test_defaultmap_off")
            OMPVV_GET_ERRORS(errors_bf)
            
            OMPVV_TEST_AND_SET_SHARED_ENVIRONMENT(sharedEnvWarning)
           
            longMessage = "Shared memory environment. Scalars&
              &are not copied over but modified.&
              &This part of the tests is inconclusive"
            OMPVV_WARNING_IF(sharedEnvWarning, longMessage)

            scalar_char = 'a'
            scalar_byte = 8
            scalar_short = 23
            scalar_int = 56
            scalar_long_int = 90000
            scalar_float  = 4.5e+3
            scalar_double  = 4.9e+10
            scalar_logical = .true.
            scalar_logical_kind4 = .true.
            scalar_logical_kind8 = .true.
            scalar_complex = (1, 1)
            scalar_double_complex = (1e+10, 1e+10)
            
            isHost = .false.
            ! Map the scalar variables without defaultmap
            !$omp target map(from: isHost) map(tofrom: firstprivateCheck)
              ! make sure it offloaded
              isHost = omp_is_initial_device()

              ! checking for firstprivate copy
              firstprivateCheck(1)  = scalar_char == 'a'
              firstprivateCheck(2)  = scalar_byte == 8
              firstprivateCheck(3)  = scalar_short == 23
              firstprivateCheck(4)  = scalar_int == 56
              firstprivateCheck(5)  = scalar_long_int == 90000
              firstprivateCheck(6)  = scalar_float  == 4.5e+3
              firstprivateCheck(7)  = scalar_double  == 4.9e+10
              firstprivateCheck(8)  = scalar_logical .EQV. .true.
              firstprivateCheck(9)  = scalar_logical_kind4 .EQV. .true.
              firstprivateCheck(10) = scalar_logical_kind8 .EQV. .true.
              firstprivateCheck(11) = scalar_complex == (1, 1)
              firstprivateCheck(12) = scalar_double_complex == (1e+10, 1e+10)

              ! Attempting value change
              scalar_char = 'b'
              scalar_byte = 80
              scalar_short = 83
              scalar_int = 49
              scalar_long_int = 12345
              scalar_float  = 3.0e+5
              scalar_double  = 9.5e+9
              scalar_logical = .false.
              scalar_logical_kind4 = .false.
              scalar_logical_kind8 = .false.
              scalar_complex = (5, 5)
              scalar_double_complex = (5e+9, 5e+9)
            !$omp end target

            ! Check for the firstprivate values
            DO i = 1,12
              OMPVV_GET_ERRORS(tmp1)
              OMPVV_TEST_AND_SET_VERBOSE(tmp2, .not. firstprivateCheck(i))
              write(shortMessage, '(A,I0)') "Error in firstprivate&
              & for i = ", i
              OMPVV_ERROR_IF(tmp1 /= tmp2, shortMessage)
            END DO 
              
            ! If it is initial device then we will modify the original memory region
            IF (isHost .OR. sharedEnvWarning) THEN
              OMPVV_TEST_VERBOSE(scalar_char /= 'b')
              OMPVV_TEST_VERBOSE(scalar_byte /= 80)
              OMPVV_TEST_VERBOSE(scalar_short /= 83)
              OMPVV_TEST_VERBOSE(scalar_int /= 49)
              OMPVV_TEST_VERBOSE(scalar_long_int /= 12345)
              OMPVV_TEST_VERBOSE(scalar_float  /= 3.0e+5)
              OMPVV_TEST_VERBOSE(scalar_double  /= 9.5e+9)
              OMPVV_TEST_VERBOSE(scalar_logical .NEQV. .false.)
              OMPVV_TEST_VERBOSE(scalar_logical_kind4 .NEQV. .false.)
              OMPVV_TEST_VERBOSE(LOGICAL(scalar_logical_kind8 .NEQV. .false., kind = 4))
              OMPVV_TEST_VERBOSE(scalar_complex /= (5, 5))
              OMPVV_TEST_VERBOSE(scalar_double_complex /= (5e+9, 5e+9))
            else 
              OMPVV_TEST_VERBOSE(scalar_char /= 'a')
              OMPVV_TEST_VERBOSE(scalar_byte /= 8)
              OMPVV_TEST_VERBOSE(scalar_short /= 23)
              OMPVV_TEST_VERBOSE(scalar_int /= 56)
              OMPVV_TEST_VERBOSE(scalar_long_int /= 90000)
              OMPVV_TEST_VERBOSE(scalar_float  /= 4.5e+3)
              OMPVV_TEST_VERBOSE(scalar_double  /= 4.9e+10)
              OMPVV_TEST_VERBOSE(scalar_logical .NEQV. .true.)
              OMPVV_TEST_VERBOSE(scalar_logical_kind4 .NEQV. .true.)
              OMPVV_TEST_VERBOSE(LOGICAL(scalar_logical_kind8 .NEQV. .true., kind = 4))
              OMPVV_TEST_VERBOSE(scalar_complex /= (1, 1))
              OMPVV_TEST_VERBOSE(scalar_double_complex /= (1e+10, 1e+10))
            END IF

            OMPVV_GET_ERRORS(errors_af)
            test_defaultmap_off = errors_bf - errors_af
          END FUNCTION test_defaultmap_off
      END PROGRAM test_target_defaultmap

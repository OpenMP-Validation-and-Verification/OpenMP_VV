!===--test_target_map_components_default.F90 - derived data type map default-===!
! 
! OpenMP API Version 4.5 Nov 2015
!
! This test checks the default derived data types (components) mapping behavior. 
! Without specifying. if no map type is used, the default is tofrom. When
! mapping a struct/derived data type the default behavior of the components
! should be the default behavior of their data type. No deep copy is supported
! 
!!===----------------------------------------------------------------------===!
#include "ompvv.F90"

#define N 100

      PROGRAM test_target_map_components
        USE iso_fortran_env
        USE ompvv_lib
        USE omp_lib
        implicit none

        TYPE :: testingType 
          INTEGER, POINTER :: myPtr
          INTEGER :: myInt
          CHARACTER(len=100) :: myStr
          REAL:: myReal
          INTEGER, DIMENSION(N) :: myArr
        END TYPE testingType
        
        OMPVV_TEST_OFFLOADING
        OMPVV_TEST_VERBOSE(test_map_derived_type_default() .ne. 0)
        OMPVV_TEST_VERBOSE(test_map_derived_type_default_array() .ne. 0)

        OMPVV_REPORT_AND_RETURN()

        CONTAINS 
          INTEGER FUNCTION test_map_derived_type_default()
            TYPE(testingType) :: myStruct
            TYPE(testingType) :: cpyStruct
            INTEGER, TARGET :: justATarget
            INTEGER :: err_bf, err_af

            OMPVV_GET_ERRORS(err_bf)

            myStruct%myInt = 5
            myStruct%myStr = "there you go"
            myStruct%myReal = 4.4
            myStruct%myArr(:) = 10
            myStruct%myPtr => justATarget

            cpyStruct%myInt = 0
            cpyStruct%myStr = ""
            cpyStruct%myReal = 0.0
            cpyStruct%myArr(:) = 0
            cpyStruct%myPtr => justATarget

            !$omp target map(myStruct, cpyStruct)
              cpyStruct%myInt = myStruct%myInt
              cpyStruct%myStr = myStruct%myStr
              cpyStruct%myReal = myStruct%myReal
              cpyStruct%myArr(:) = myStruct%myArr(:)
            !$omp end target

            OMPVV_TEST_VERBOSE(cpyStruct%myInt /= 5)
            OMPVV_TEST_VERBOSE(cpyStruct%myStr .NE. 'there you go')
            OMPVV_TEST_VERBOSE(ABS(cpyStruct%myReal - 4.4) .GT. 0.0001)
            OMPVV_TEST_VERBOSE(ANY(cpyStruct%myArr /= 10))
            OMPVV_TEST_VERBOSE(.NOT. ASSOCIATED(myStruct%myPtr, justATarget))
            OMPVV_TEST_VERBOSE(.NOT. ASSOCIATED(cpyStruct%myPtr, justATarget))

            OMPVV_GET_ERRORS(err_af)
            test_map_derived_type_default = err_af - err_bf

          END FUNCTION test_map_derived_type_default

          INTEGER FUNCTION test_map_derived_type_default_array()
            TYPE(testingType) :: myStruct(10)
            TYPE(testingType) :: cpyStruct(10)
            INTEGER, TARGET :: justATarget
            INTEGER :: i, err_bf, err_af

            OMPVV_GET_ERRORS(err_bf)
            DO i = 1, 10 
              myStruct(i)%myInt = 5
              myStruct(i)%myStr = "there you go"
              myStruct(i)%myReal = 4.4
              myStruct(i)%myArr(:) = 10
              myStruct(i)%myPtr => justATarget

              cpyStruct(i)%myInt = 0
              cpyStruct(i)%myStr = ""
              cpyStruct(i)%myReal = 0.0
              cpyStruct(i)%myArr(:) = 0
              cpyStruct(i)%myPtr => justATarget
            END DO

            !$omp target map(myStruct, cpyStruct)
              DO i = 1, 10 
                cpyStruct(i)%myInt = myStruct(i)%myInt
                cpyStruct(i)%myStr = myStruct(i)%myStr
                cpyStruct(i)%myReal = myStruct(i)%myReal
                cpyStruct(i)%myArr(1:N) = myStruct(i)%myArr(1:N)
              END DO
            !$omp end target

            DO i = 1, 10 
              OMPVV_TEST_VERBOSE(cpyStruct(i)%myInt /= 5)
              OMPVV_TEST_VERBOSE(cpyStruct(i)%myStr .NE. 'there you go')
              OMPVV_TEST_VERBOSE(ABS(cpyStruct(i)%myReal - 4.4) .GT. 0.0001)
              OMPVV_TEST_VERBOSE(ANY(cpyStruct(i)%myArr /= 10))
              OMPVV_TEST_VERBOSE(.NOT. ASSOCIATED(myStruct(i)%myPtr, justATarget))
              OMPVV_TEST_VERBOSE(.NOT. ASSOCIATED(cpyStruct(i)%myPtr, justATarget))
            END DO

            OMPVV_GET_ERRORS(err_af)
            test_map_derived_type_default_array = err_af - err_bf
          END FUNCTION test_map_derived_type_default_array

      END PROGRAM 


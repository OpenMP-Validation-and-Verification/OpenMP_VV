C===-test_ompx.F90-===/
C
C
C OpenMP API Version 5.2 July 2022
C
C Testing 'omx' sentinel
C
C 'omx' sentinel is reserved for
C implementation-defined extensions to
C fixed source form OpenMP directives.
C===-------------------------------===//
#        include "ompvv.F90"

#        define N 1024
        PROGRAM test_omx
                use iso_fortran_env
                use ompvv_lib
                use omp_lib
                implicit none

                OMPVV_TEST_OFFLOADING

                OMPVV_TEST_VERBOSE(test_fixed_omx() .ne. 2)

                OMPVV_REPORT_AND_RETURN()
        CONTAINS
                INTEGER FUNCTION test_fixed_omx() 
                        INTEGER :: i
                        INTEGER :: n
                        INTEGER :: errors
                        INTEGER, DIMENSION(2) :: ARR_ERR
                        errors = 0
                        !$omp parallel shared(ARR_ERR) private(i)  num_threads(2)
                        !$omx test_nonexistant_omx
                                i = omp_get_thread_num()
                                i = i + 1
                                ARR_ERR(i) = 1
                        !$omp end parallel
                        do n = 1,2
                                errors = errors + ARR_ERR(n)
                        end do
                        test_fixed_omx = errors
                END FUNCTION test_fixed_omx
        END PROGRAM test_omx

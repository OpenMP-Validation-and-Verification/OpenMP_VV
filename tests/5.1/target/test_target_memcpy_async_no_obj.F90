!===--- test_target_memcpy_async_no_obj.F90 -------------------------------===//
!
! OpenMP API Version 5.1 Nov 2020
!
!  Inspired from OpenMP 5.1 Examples Doc, 5.16.4 & 8.9
!  This test utilizes the omp_target_memcpy_async construct to
!  allocate memory on the device asynchronously. The construct
!  uses 0 for 'depobj_count', so that the clause is not dependent
!  and memory is therefore copied synchronously.
!
!//===---------------------------------------------------------------------===//

#include "ompvv.F90"

#define N 1024

PROGRAM test_target_memcpy_async_no_obj
  USE iso_fortran_env
  USE, INTRINSIC :: iso_c_binding
  USE ompvv_lib
  USE omp_lib
  implicit none
  OMPVV_TEST_OFFLOADING

  OMPVV_TEST_VERBOSE(test_memcpy_async_no_obj() .NE. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION test_memcpy_async_no_obj()
    INTEGER :: errors, i
    DOUBLE PRECISION, TARGET, ALLOCATABLE :: arr(:)
    TYPE (C_PTR) :: mem, mem_dev_cpy
    INTEGER (C_SIZE_T) :: csize, dst_offset, src_offset
    INTEGER (C_INT) :: h, t, depobj_count

    errors = 0
    h = omp_get_initial_device()
    t = omp_get_default_device()
    dst_offset = 0
    src_offset = 0
    depobj_count = 0

    OMPVV_TEST_AND_SET_VERBOSE(errors, omp_get_num_devices() .LT. 1 .OR. t .LT. 0)

    ALLOCATE(arr(N))
    mem = c_loc(arr(1))
    csize = c_sizeof(arr(1)) * N
    mem_dev_cpy = omp_target_alloc(csize, t)

    OMPVV_TEST_AND_SET_VERBOSE(errors, .NOT. c_associated(mem))
    IF(.NOT. c_associated(mem)) THEN
      test_memcpy_async_no_obj = errors
      RETURN
    END IF

    OMPVV_TEST_AND_SET_VERBOSE(errors, .NOT. c_associated(mem_dev_cpy))
    IF(.NOT. c_associated(mem_dev_cpy)) THEN
      test_memcpy_async_no_obj = errors
      RETURN  
    END IF

    DO i=1, N
      arr(i) = i
    END DO

    ! copy to target
    errors = omp_target_memcpy_async(mem_dev_cpy, mem, csize, dst_offset, src_offset, t, h, depobj_count)

    IF(errors /= 0) THEN
      OMPVV_ERROR("omp_target_memcpy_async returns not 0");
      DEALLOCATE(arr)
      CALL omp_target_free(mem_dev_cpy, t)
      test_memcpy_async_no_obj = errors
      RETURN
    END IF

    !$omp taskwait
    !$omp target is_device_ptr(mem_dev_cpy) device(t)
    !$omp teams distribute parallel do
    DO i=1, N
      BLOCK
      DOUBLE PRECISION, POINTER :: fptr(:)
      CALL c_f_pointer(mem_dev_cpy, fptr, [N])
      fptr(i) = fptr(i) * 2 ! initialize data on device
      END BLOCK
    END DO
    !$omp end teams distribute parallel do
    !$omp end target

    ! copy to host memory
    errors = omp_target_memcpy_async(mem, mem_dev_cpy, csize, dst_offset, src_offset, h, t, depobj_count)

    !$omp taskwait
    DO i=1, N
      OMPVV_TEST_AND_SET(errors, arr(i) .NE. i*2)
    END DO

    ! free resources
    DEALLOCATE(arr)
    CALL omp_target_free(mem_dev_cpy, t)

    test_memcpy_async_no_obj = errors
  END FUNCTION test_memcpy_async_no_obj
END PROGRAM test_target_memcpy_async_no_obj


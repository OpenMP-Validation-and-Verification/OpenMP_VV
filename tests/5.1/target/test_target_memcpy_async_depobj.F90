!===--- test_target_memcpy_async_depobj.F90 ---------------------------------------------===//
!
! OpenMP API Version 5.1 Nov 2020
!
!  Inspired from OpenMP 5.1 Examples Doc, 5.16.4 & 8.9
!  This test utilizes the omp_target_memcpy_async construct to
!  allocate memory on the device asynchronously. The construct
!  uses 'obj' for dependency, so that memory is only copied once
!  the variable listed in the depend clause is changed.
!
!//===-----------------------------------------------------------------------------------------===//

#include "ompvv.F90"

#define N 1024

PROGRAM test_target_memcpy_async_depobj
  USE iso_fortran_env
  USE, INTRINSIC :: iso_c_binding
  USE ompvv_lib
  USE omp_lib
  implicit none
  OMPVV_TEST_OFFLOADING

  OMPVV_TEST_VERBOSE(test_memcpy_async_depobj() .NE. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION test_memcpy_async_depobj()
    INTEGER :: errors, i
    DOUBLE PRECISION, TARGET, ALLOCATABLE :: arr(:)
    DOUBLE PRECISION, POINTER :: fptr
    TYPE (C_PTR) :: mem, mem_dev_cpy
    INTEGER (C_SIZE_T) :: csize, dst_offset, src_offset
    INTEGER (C_INT) :: h, t, depobj_count
    INTEGER (omp_depend_kind) :: obj, obj_arr(1)

    errors = 0
    h = omp_get_initial_device()
    t = omp_get_default_device()
    dst_offset = 0
    src_offset = 0
    depobj_count = 1

    ALLOCATE(arr(N))
    mem = c_loc(arr(1))
    csize = c_sizeof(arr(1)) * N
    mem_dev_cpy = omp_target_alloc(csize, t)

    OMPVV_TEST_AND_SET_VERBOSE(errors, .NOT. c_associated(mem_dev_cpy))

    DO i=1, N
      arr(i) = i
    END DO

    !$omp depobj(obj) depend(inout: mem_dev_cpy)
    obj_arr(1) = obj

    ! copy to device memory
    omp_target_memcpy_async(mem_dev_cpy, mem, csize, dst_offset, src_offset, t, h, depobj_count, obj_arr)

    !$omp taskwait depend(depobj: obj)
    !$omp target is_device_ptr(mem_dev_cpy) device(t) depend(depobj: obj)
    CALL c_f_pointer(mem_dev_cpy, fptr)
    DO i=1, N
      fptr(i) = fptr(i) * 2 ! initialize data
    END DO
    !$omp end target

    ! copy to host memory
    omp_target_memcpy_async(mem, mem_dev_cpy, csize, dst_offset, src_offset, h, t, depobj_count, obj_arr)

    !$omp taskwait depend(depobj: obj)
    DO i=1, N
      OMPVV_TEST_AND_SET(errors, arr(i) .NE. i*2)
    END DO

    ! free resources
    DEALLOCATE(arr)
    omp_target_free(mem_dev_cpy, t)
    !$omp depobj(obj) destroy

    test_memcpy_async_depobj = errors
  END FUNCTION test_memcpy_async_depobj
END PROGRAM test_target_memcpy_async_depobj


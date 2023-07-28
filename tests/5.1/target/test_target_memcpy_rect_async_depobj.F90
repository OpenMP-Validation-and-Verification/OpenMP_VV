!===--- test_target_memcpy_rect_async_depobj.F90 -------------------------------===//
!
! OpenMP API Version 5.1 Nov 2020
!
! Inspired from OpenMP 5.1 Examples Doc, 5.16.4 & 8.9
! This test utilizes the omp_target_memcpy_rect_async construct to
! allocate 2D memory on the device asynchronously. The construct
! uses 'obj' for dependency, so that memory is only copied once
! the variable listed in the depend clause is changed.
!
!//===---------------------------------------------------------------------===//

#include "ompvv.F90"

#define N 5
#define M 10

PROGRAM test_target_memcpy_rect_async_depobj
  USE iso_fortran_env
  USE, INTRINSIC :: iso_c_binding
  USE ompvv_lib
  USE omp_lib
  implicit none
  OMPVV_TEST_OFFLOADING

  OMPVV_TEST_VERBOSE(test_memcpy_rect_async_depobj() .NE. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION test_memcpy_rect_async_depobj()
    INTEGER :: errors, i, j
    DOUBLE PRECISION, POINTER :: fptr(:)
    TYPE (C_PTR) :: mem, devRect
    INTEGER (C_SIZE_T) :: csize
    INTEGER (C_SIZE_T) :: offsets(2), volume(2), dimensions(2)
    INTEGER (C_INT) :: h, t, depobj_count, num_dims
    DOUBLE PRECISION, TARGET :: hostRect(M,N)
    INTEGER (omp_depend_kind) :: obj, obj_arr(1)

    errors = 0
    fptr => null()
    h = omp_get_initial_device()
    t = omp_get_default_device()
    volume = (/ M, N /)
    offsets = (/ 0, 0 /)
    dimensions = (/ M, N /)
    depobj_count = 1
    num_dims = 2

    OMPVV_TEST_AND_SET_VERBOSE(errors, omp_get_num_devices() .LT. 1 .OR. t .LT. 0)

    mem = c_loc(hostRect(1,1))
    csize = c_sizeof(hostRect(1,1)) * M * N
    devRect = omp_target_alloc(csize, t)

    OMPVV_ERROR_IF(.NOT. c_associated(devRect), "Error: omp_target_alloc() failed")
    IF(.NOT. c_associated(devRect)) THEN
      errors = errors + 1
      test_memcpy_rect_async_depobj = errors
      RETURN
    END IF

    DO i=1, N
      DO j=1, M
      hostRect(j,i) = i + j
      END DO
    END DO

    !$omp depobj(obj) depend(inout: devRect)
    obj_arr(1) = obj

    csize = c_sizeof(hostRect(1,1))
    ! copy to device memory
    errors = omp_target_memcpy_rect_async(devRect, mem, csize, num_dims, volume, offsets, offsets, dimensions, dimensions, t, h, depobj_count, obj_arr)

    !$omp taskwait depend(depobj: obj)
    !$omp target is_device_ptr(devRect) device(t) depend(depobj: obj)
    CALL c_f_pointer(devRect, fptr, [M*N])
    DO i=1, N
      DO j=1, M
        fptr((i-1)*M+j) = fptr((i-1)*M+j) * 2 ! initialize data
      END DO
    END DO
    fptr => null()
    !$omp end target

    ! copy to host memory
    errors = omp_target_memcpy_rect_async(mem, devRect, csize, num_dims, volume, offsets, offsets, dimensions, dimensions, h, t, depobj_count, obj_arr)

    !$omp taskwait depend(depobj: obj)
    DO i=1, N
      DO j=1, N
        OMPVV_TEST_AND_SET(errors, hostRect(j,i) .NE. (i+j)*2)
      END DO
    END DO

    ! free resources
    CALL omp_target_free(devRect, t)
    !$omp depobj(obj) destroy

    test_memcpy_rect_async_depobj = errors
  END FUNCTION test_memcpy_rect_async_depobj
END PROGRAM test_target_memcpy_rect_async_depobj


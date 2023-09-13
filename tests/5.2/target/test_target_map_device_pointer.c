//--------------- test_target_map_device_pointer.c ---------------------------//
// OpenMP API Version 5.2 Nov 2021
// Pg. 627, line 24
// This test ensures that--paraphrase of Section 5.8.6--a pointer that is
// implicitly mapped or explicitely mapped to the device, predetermined to be
// firstprivate in the device data environment, will retain its original value
// (attained prior to the target region) and follow the semantics of the
// firstprivate clause. This is true if a matching mapped list item, as defined
// in Section 5.8.6, is not found for the pointer. In the test, the pointer p1
// is initialized with the base array address of arr2. When p1 is implicitly
// mapped to the device, it should retain the same base array address, as the
// array itself is not mapped (a matching mapped list item is not found). This
// check is made in the target region with a mapped integer variable holding the
// base address of arr2. The pointer p2 is initialized to NULL, and is checked
// to remain NULL after being initialized on a device. Following the checks, p1
// is changed to NULL on the device and checked outside of the target region if
// it was not changed from its initial value of the base address of arr2 (per
// firstprivate semnatics). p2 is given an arbitrary value for the purpose of
// ensuring that it too retains its original value after the target region. A
// defaultmap clause is present on the target construct to explicitely ensure
// that the default semantics of mapping apply to all mapped (implicit/explicit)
// pointer types. The zero length array, arr1, is mapped to the device to ensure
// that a runtime check occurs in accordance with the semantics described in the
// target construct on page 285 of the specifications.
//----------------------------------------------------------------------------//
#include "ompvv.h"
#include <omp.h>
#include <stdint.h> //includes intptr_t

#define N 16

int test_target_pointer() {
  int errors = 0;
  int arr1[0], arr2[N];
  int *p1 = &arr2[0];
  int *p2 = NULL;
  intptr_t hold_arr2 = (intptr_t)&arr2[0];

  #pragma omp target map(tofrom:errors) map(to:hold_arr2,arr1[:0]) defaultmap(default:pointer)
  {
    OMPVV_TEST_AND_SET(errors, ((intptr_t) p1 != hold_arr2) || (p2 != NULL));
    
    p1 = NULL;
    p2 = (int *) 0x12345;
  }

  OMPVV_TEST_OFFLOADING_PROBE;
  OMPVV_TEST_SHARED_ENVIRONMENT_PROBE;

  if (_ompvv_isOffloadingOn && !_ompvv_isSharedEnv){
    OMPVV_ERROR_IF(errors != 0,
                   "At least one pointer did not retain its original value");
    OMPVV_ERROR_IF(p1 != &arr2[0],
                   "The value of p1 was updated, p1 was not firstprivate");
    OMPVV_ERROR_IF(p2 != NULL,
                   "The value of p2 was updated, p2 was not firstprivate");
    OMPVV_TEST_AND_SET(errors, (p1 != &arr2[0]) || (p2 != NULL))
  }else{
    OMPVV_WARNING("_ompvv_isOffloadingOn: %i, _ompvv_isSharedEnv: %i, test is invalid",
        _ompvv_isOffloadingOn, _ompvv_isSharedEnv)
  }
  return errors;
}

int main() {
  int errors = 0;
  int on_device = 0;
  OMPVV_TEST_AND_SET(errors, test_target_pointer() != 0)
  OMPVV_REPORT_AND_RETURN(errors);
  return errors;
}

//--------------- test_target_map_type_modifier_self.c -----------------------//
// OpenMP API Version 6.0 November 2024
// Pg. 899, line 14
// ***********
// DIRECTIVE:target
// CLAUSE:map
// ***********
// This test checks that the map type-modifer self correctly refers to the
// original list-item when using the corresponding list-item on the device (the
// mapped scalar value). Thus, the test will pass if the memory address of the
// object on the device is the same as the host address. It is also required
// that the scalar_value variable is updated accordingly for the test to pass.
//----------------------------------------------------------------------------//
#include "ompvv.h"
#include <omp.h>

int test_map_type_modifier_self() {
  int errors = 0;
  int scalar_value = 0;
  int *host_address = &scalar_value;
  int *device_address = NULL;

  #pragma omp target map(self : scalar_value) map(tofrom : device_address)
  {
    scalar_value += 1;
    device_address = &scalar_value;
  }

  OMPVV_ERROR_IF(scalar_value != 1, "Expected %i, received %i", 1,
                 scalar_value);
  OMPVV_ERROR_IF(device_address != host_address, "Expected %p, received %p",
                 host_address, device_address);
  OMPVV_TEST_AND_SET(errors,
                     (scalar_value != 1) || (device_address != host_address));
  return errors;
}

int main() {
  int errors = 0;
  OMPVV_TEST_OFFLOADING;
  OMPVV_TEST_AND_SET(errors, test_map_type_modifier_self() != 0);
  OMPVV_REPORT_AND_RETURN(errors);
  return errors;
}

//===------ test_cpp_decl_attribute2.cpp -------------------------------------===//
//
// OpenMP API Version 6.0 November 2024
//
// ***********
// DIRECTIVE:target
// CLAUSE:map
// ***********
//
// This test checks the usage of the decl attribute for a directive-specification 
// with target as the directive and map as the optional clause. The test will pass
// when offloading succeeds or when num_devices == 0.
//
//===-------------------------------------------------------------------------===//
#include <omp.h>
#include "ompvv.h"

int main(void) {
  int errors = 0;
  int on_host = 1;
  int num_devices = omp_get_num_devices();

  [[omp :: decl( target map(from: on_host) )]]
  { on_host = omp_is_initial_device(); }

  if (num_devices > 0) {
    OMPVV_ERROR_IF(
        on_host == 1,
        "A device was available, but the target directive was not executed");
    OMPVV_TEST_AND_SET_VERBOSE(errors, on_host == 1);
  } else {
    OMPVV_WARNING(
        "NO DEVICES ARE AVAILABLE, DECL ATTRIBUTE EXECUTED ON HOST");
  }

  // CHECK: Target region executed on the device
  printf("Target region executed on the %s\n", on_host ? "host" : "device");

  return errors;
}

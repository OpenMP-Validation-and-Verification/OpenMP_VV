//===------ test_c_decl_attribute2.c -----------------------------------------===//
//
// OpenMP API Version 6.0 November 2024
//
// ***********
// DIRECTIVE:declare target
// CLAUSE:enter
// ***********
//
// This test checks the usage of the decl attribute for a directive-specification 
// with declare target as the directive and enter as the optional clause. The test
// will pass when offloading succeeds or when num_devices == 0.
//
//===-------------------------------------------------------------------------===//
#include <omp.h>
#include "ompvv.h"

[[omp :: decl(declare target, enter)]] int on_host = 1;

[[omp :: decl(declare target)]]
void update() { on_host = omp_is_initial_device(); }

int main(void) {
  int errors = 0;
  int num_devices = omp_get_num_devices();

#pragma omp target map(always, from: on_host)
  { update(); }

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

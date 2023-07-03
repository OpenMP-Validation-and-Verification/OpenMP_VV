//--------------- test_metadirective_otherwise.c ----------------------------//
// OpenMP API Version 5.2 Nov 2021
// *************************
// DIRECTIVE: metadirective
// CLAUSES: when, otherwise
// *************************
// The otherwise clause of the metadirective construct is being tested. The
// metadirective construct contains a when clause, which itself contains a
// context-selector (device = {kind(host)}) and a directive-variant (nothing).
// The metadirective construct also contains an otherwise clause, which itself
// contains a directive-variant (parallel for).
// The when clause of the metadirective construct is
// evaluated for a valid context-selector pertaining to a device related
// characteristic of a certain kind (host), defined in the OpenMP additional
// definitions. If no target construct were specified, the evaluation would
// be true, and the directive-variant nothing would be executed as if
// #pragma omp nothing were present. Target is present, so the directive-variant
// of the otherwise clause will be executed. This would not occur if
// device = {kind(nohost)} were specified. If the given for loop executes in
// a nested parallel region, the test will pass.
//----------------------------------------------------------------------------//

#include "ompvv.h"
#include <omp.h>

#define N 16

int test_otherwise() {
  int errors = 0;
  int total = 0;
  int num_devices = omp_get_num_devices();

  #pragma omp target map(tofrom : total)
  {
  #pragma omp metadirective \
    when(device = {kind(host)}: nothing) \
    otherwise(parallel for)
    {
      for (int i = 0; i < N; ++i) {
        if (omp_get_level()) {
          #pragma omp atomic update
          ++total;
        }
      }
    }
  }
  if (num_devices > 0) {
    OMPVV_ERROR_IF(total != N, "Value of total is %i, not %i", total, N);
    OMPVV_TEST_AND_SET_VERBOSE(errors, total != N);
  } else {
    OMPVV_WARNING(
        "NO DEVICES ARE AVAILABLE, OTHERWISE CLAUSE WAS NOT ACCESSED");
  }

  return errors;
}

int main() {
  int errors = 0;
  OMPVV_INFOMSG("IMPLEMENTATION DEFINED OPENMP VERSION: %d", _OPENMP);
  OMPVV_TEST_OFFLOADING;
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_otherwise() != 0);
  OMPVV_REPORT_AND_RETURN(errors);
}

//--------------- test_directive_name_modifier.c -----------------------------//
// OpenMP API Version 6.0 November 2024
// Pg. 898, line 3
// CLAUSE:firstprivate
// All modifiers to clauses were extended to accept directive-name-modifiers.
// In this case, the firstprivate clause is being used with the target
// directive-name-modifier. If the threads correctly firstprivatize
// shared_value, then the test will pass.
//----------------------------------------------------------------------------//

#include "ompvv.h"
#include <omp.h>

#define N OMPVV_NUM_THREADS_DEVICE

int test_directive_name_modifier() {
  int errors = 0;

  int shared_value = 0;

  #pragma omp target teams distribute firstprivate(target : shared_value) \
    map(from : final_shared)
  for (int i = 0; i < N; ++i) {
    #pragma omp atomic update
    shared_value++;
    final_shared = shared_value;
  }
  shared_value = 5;
  int shared_val_arr[OMPVV_NUM_TEAMS_TARGET];
   #pragma omp target teams distribute firstprivate(teams : shared_value) \
    map(from : shared_val_arr [])
    shared_value += omp_get_team_num();
    shared_val_arr[omp_get_team_num()] = shared_value;
    
  }

  if (shared_value != N)
    errors++;

  return errors;
}

int main() {
  int errors = 0;
  OMPVV_TEST_OFFLOADING;
  OMPVV_TEST_AND_SET(errors, test_directive_name_modifier() != 0);
  OMPVV_REPORT_AND_RETURN(errors);
  return errors;
}

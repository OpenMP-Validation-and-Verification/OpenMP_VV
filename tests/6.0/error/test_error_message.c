//--------------- test_directive_clause.c ------------------------------------//
// OpenMP API Version 6.0 August 2024
// Pg. 318, line 22
// ***********
// DIRECTIVE:error
// CLAUSE:message
// ***********
// The message clause for the error directive is being tested. If the clause
// executes correctly, than the user will see the given message. Otherwise, the
// message will be different, and the user will notice the difference.
//----------------------------------------------------------------------------//
#include "ompvv.h"

int test_message() {
  #pragma omp error at(compilation) severity(warning) \
      message("Message clause was executed.")

  return 0;
}

int main() {
  int errors = 0;
  OMPVV_TEST_AND_SET(errors, test_message() != 0);
  OMPVV_REPORT_AND_RETURN(errors);
  return errors;
}

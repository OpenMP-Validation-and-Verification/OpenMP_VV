// RUN: %libomptarget-compile-run-and-check-aarch64-unknown-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64le-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-x86_64-pc-linux-gnu

//===------ FILE_NAME.c ------------------ Test title ---------------------===//
// 
// OpenMP API Version 4.5 Nov 2015
// 
// 
//===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

#define VERBOSE_MODE 1

#define __FILENAME__ (strrchr(__FILE__, '/') ? strrchr(__FILE__, '/') + 1 : __FILE__)

#ifdef VERBOSE_MODE
#define OMPVV_WARNING(message, ...) { \
  printf("[OMPVV_WARNING: %s:%i] " #message "\n", __FILENAME__, __LINE__, __VA_ARGS__); \
}

#define OMPVV_ERROR(message, ...) { \
  fprintf(stderr, "[OMPVV_ERROR: %s:%i] " #message "\n", __FILENAME__, __LINE__, __VA_ARGS__); \
}
#else
#define OMPVV_WARNING(message, ...)
#define OMPVV_ERROR(message, ...)
#endif // END VERBOSE_MODE
#define HASH_SIGN(text) text
#define OMPVV_TEST_OFFLOADING { \
  int isOffloadingOn = 0;  \
_Pragma("omp target data map (from: isOffloadingOn)") \
  {  isOffloadingOn = !omp_is_initial_device();  } \
  printf("[OMPVV_INFO] Test is running on %s.\n",(isOffloadingOn)? "device" : "host"); \
}

#define OMPVV_REPORT(err) { \
  printf("[OMPVV_RESULT] Test %s.\n", (err == 0)? "passed":"failed"); \
}

#define OMPVV_RETURN(err) { \
  return (err == 0) ? EXIT_SUCCESS : EXIT_FAILURE; \
}

#define OMPVV_REPORT_AND_RETURN(err) {\
  OMPVV_REPORT(err); \
  OMPVV_RETURN(err); \
}

int test_function() {
  int errors = 0;
  //
  // WRITE YOUR TEST HERE
  //

  return errors;
}

int main() {
  OMPVV_TEST_OFFLOADING;
  int errors = 0;
  errors = test_function();
  // CALL OTHER FUNCTIONS HERE

  OMPVV_REPORT(errors);
  OMPVV_RETURN(errors);

}

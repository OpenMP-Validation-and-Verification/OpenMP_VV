//===------ ompvv.h ------------------ OMPVV HEADER FILE ------------------===//
// 
// Header file for OMP Validation and verification test suite
// 
//===----------------------------------------------------------------------===//

#include <stdio.h>
#include <string.h>

#define __FILENAME__ (strrchr(__FILE__, '/') ? strrchr(__FILE__, '/') + 1 : __FILE__)

// Macro for output of information, warning and error messages
#ifdef VERBOSE_MODE
  #define OMPVV_WARNING(message, ...) { \
    printf("[OMPVV_WARNING: %s:%i] " message "\n", __FILENAME__, __LINE__, ##__VA_ARGS__); \
  }
  
  #define OMPVV_ERROR(message, ...) { \
    fprintf(stderr, "[OMPVV_ERROR: %s:%i] " message "\n", __FILENAME__, __LINE__, ##__VA_ARGS__); \
  }
  
  #define OMPVV_INFOMSG(message, ...) { \
    printf("[OMPVV_INFO: %s:%i] " message "\n", __FILENAME__, __LINE__, ##__VA_ARGS__); \
  }
#else
  #define OMPVV_WARNING(message, ...) {}
  #define OMPVV_ERROR(message, ...) {}
  #define OMPVV_INFOMSG(message, ...) {}
#endif // END IF VERBOSE_MODE

#define OMPVV_TEST_OFFLOADING_PROBE \
  int _ompvv_isOffloadingOn = 0; \
_Pragma("omp target map (from: _ompvv_isOffloadingOn)") \
  {  _ompvv_isOffloadingOn = !omp_is_initial_device();  }

// Macro for checking if offloading is enabled or not
#define OMPVV_TEST_OFFLOADING { \
  OMPVV_TEST_OFFLOADING_PROBE \
  OMPVV_INFOMSG("Test is running on %s.",(_ompvv_isOffloadingOn)? "device" : "host"); \
}

// Macro for setting errors on condition
#define OMPVV_TEST_AND_SET(err, condition) { \
  int _ompvv_conditionEval = condition; \
  err += (_ompvv_conditionEval); \
  if ((_ompvv_conditionEval)) { \
    OMPVV_ERROR(" Condition " #condition " failed "); \
  } \
}

// Macro for reporting results
#define OMPVV_REPORT(err) { \
  OMPVV_TEST_OFFLOADING_PROBE \
  printf("[OMPVV_RESULT: %s] Test %s on the %s.\n", __FILENAME__, (err == 0)? "passed":"failed", (_ompvv_isOffloadingOn)? "device" : "host"); \
  OMPVV_INFOMSG("The value of " #err " is %d.", err); \
}

// Macro for correct exit code
#define OMPVV_RETURN(err) { \
  return (err == 0) ? EXIT_SUCCESS : EXIT_FAILURE; \
}

// Macro for report and exit
#define OMPVV_REPORT_AND_RETURN(err) {\
  OMPVV_REPORT(err); \
  OMPVV_RETURN(err); \
}

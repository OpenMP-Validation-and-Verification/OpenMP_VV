//===--- test_omp_target_offload_mandatory.c ----------------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// This test checks for offloading behavior when OMP_TARGET_OFFLOAD environment variable
// is set to MANDATORY. When set to Mandatory, the program will terminate execution when a
// target construct is encountered and a target device is not available or supported by the
// implementation. 
// 
////===------------------------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "ompvv.h"

typedef enum offload_policy
{MANDATORY, DISABLED, DEFAULT, UNKNOWN, NOTSET} offload_policy_t;

offload_policy_t get_offload_policy()
{
   char *env, *end;
   size_t n;

   env = getenv("OMP_TARGET_OFFLOAD");

   if(env == NULL) {
      return NOTSET;
   }
   end = env + strlen(env);

   while (*env && isspace(*(env  )) ) env++;

      while (end != env && isspace(*(end-1)) ) end--;
      n = (int)(end - env);

      if      (n == 9 && !strncasecmp(env, "MANDATORY",n)) return MANDATORY;
      else if (n == 8 && !strncasecmp(env, "DISABLED" ,n)) return DISABLED ;
      else if (n == 7 && !strncasecmp(env, "DEFAULT"  ,n)) return DEFAULT  ;
      else                                                 return UNKNOWN  ;
}

int main()
{
   int errors, device_num, on_init_dev;
   errors = 0;
   on_init_dev = 1;
 
   OMPVV_TEST_OFFLOADING;

   setenv("OMP_TARGET_OFFLOAD", "MANDATORY", 1);

   offload_policy_t policy = get_offload_policy();

   OMPVV_ERROR_IF(_OPENMP < 201811, "Warning: OMP_TARGET_OFFLOAD NOT supported by VER. %d\n",_OPENMP );
   OMPVV_TEST_AND_SET(errors, _OPENMP< 201811);

   device_num = omp_get_num_devices() + 1;
   #pragma omp target device(device_num) map(tofrom: on_init_dev)
      on_init_dev=omp_is_initial_device();

   OMPVV_ERROR_IF(policy == MANDATORY && _OPENMP >= 201811, "ERROR: OpenMP 5.0 implementation ignored MANDATORY policy");
   OMPVV_TEST_AND_SET(errors, policy == MANDATORY && _OPENMP >= 201811);

   OMPVV_REPORT_AND_RETURN(errors);
}


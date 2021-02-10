//===--- test_set_and_get_affinity_format.c ------------------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// This test checks the modification and retreival of the affinity-format-var through
// the use of the omp_set_affinity_format() and omp_get_affinity_format() routines.
// Additionally, omp_capture_affinity() is used within a parallel region to determine 
// affinity of each thread.
//
////===-------------------------------------------------------------------------------------===//


#include <stdlib.h>
#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include <omp.h>
#include "ompvv.h"

#define FORMAT_STORE 80 
#define BUFFER_STORE 80


int main(void) {

   int i, errors, n, thrd_num, max_req_store;
   size_t nchars;

   char default_format[FORMAT_STORE];
   char my_format[] = "host=%20H thrd_num=%0.4n binds_to=%A";
   char **buffer;

// Display Default Affinity Format

   omp_display_affinity(NULL);

   nchars = omp_get_affinity_format(default_format,(size_t)FORMAT_STORE);

   OMPVV_INFOMSG("Default Affinity Format is : %s\n", default_format);

   OMPVV_TEST_AND_SET_VERBOSE(errors, nchars >= FORMAT_STORE);
   OMPVV_ERROR_IF(nchars >= FORMAT_STORE, "Caution, Reported Format is truncated, increase format_store size");

   // Set Affinity Format
   omp_set_affinity_format(my_format);

   omp_display_affinity(NULL);

   // Capture Affinity
   n = omp_get_num_procs();
   buffer = (char **)malloc( sizeof(char *) * n );
   for (i = 0; i < n ;i++){ buffer[i]=(char *)malloc( sizeof(char) * BUFFER_STORE); }

   max_req_store = 0;

   #pragma omp parallel private(thrd_num,nchars) reduction(max:max_req_store)
   {   
      if(omp_get_num_threads()>n) exit(1); //safety: dont exceed # of buffers

      thrd_num=omp_get_thread_num();
      nchars=omp_capture_affinity(buffer[thrd_num],(size_t)BUFFER_STORE,NULL);

      if(nchars > max_req_store) {
         max_req_store=nchars;
      }
   }

   for (i = 0;i < n; i++) {
      OMPVV_INFOMSG("thrd_num= %d, affinity: %s\n", i,buffer[i]);
   }

   OMPVV_TEST_AND_SET_VERBOSE(errors, max_req_store >= BUFFER_STORE);
   OMPVV_ERROR_IF(max_req_store >= BUFFER_STORE, "Caution: Affinity string truncated, increase buffer size");

   for(i=0;i<n;i++) free(buffer[i]);
   free (buffer);

   OMPVV_REPORT_AND_RETURN(errors);

}


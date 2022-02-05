//===---------------- test_target_teams_thread_limit.c -----------------------===//
//
// OpenMP API Version 5.1 Nov 2020
//
// This test uses the thread_limit clause on the teams construct. Specifically
// testing if a thread_limit from a above target construct properly carries
// down to the nested teams construct, as if it were directly on the construct
// as defined in the spec. The test validates that only the specified 
// threads are created by summing a shared variable across all threads 
// (and teams). If the threads are correctedly limited this should produce the 
// expected value. Additional warnings are sent if specific issues occur.
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int main() {
	int errors = 0;
	int shared = 0;
	int num_teams = 0;

	#pragma omp target thread_limit(4)
	{
		#pragma omp teams num_teams(OMPVV_NUM_TEAMS_DEVICE)
		{
			num_teams = omp_get_num_teams();
			for (int i = 0; i < omp_get_num_threads(); i++) {
				shared += 1;
			}
		}
	}

	OMPVV_WARNING_IF(num_teams != 8, "The number of teams was unexpected, the test results are likely inconcuslive")
	OMPVV_WARNING_IF(shared > 32, "The sum was higher than expected. This likely means thread_limit isn't capping the maximum threads created.");
	OMPVV_TEST_AND_SET(errors, (shared != 32));

	OMPVV_REPORT_AND_RETURN(errors);
}

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
	int errors = [OMPVV_NUM_THREADS_DEVICE/OMPVV_NUM_TEAMS_DEVICE];
	int shared = 0;
	int num_teams = 0;

	int testing_thread_limit = OMPVV_NUM_THREADS_DEVICE/OMPVV_NUM_TEAMS_DEVICE;

	OMPVV_TEST_OFFLOADING;

	#pragma omp target map(tofrom:num_teams,errors) thread_limit(testing_thread_limit)
	{
		#pragma omp teams num_teams(OMPVV_NUM_TEAMS_DEVICE)
		{

			if (omp_get_team_num() == 0 && omp_get_thread_num() == 0) {
				num_teams = omp_get_num_teams();
			}

			if (omp_get_thread_num() == 0) {
			
			     if (omp_get_num_threads() > testing_thread_limit) {
					errors[omp_get_team_num()] += 1;				
			     }		
			     
			}

		}

	}
	

	OMPVV_WARNING_IF(num_teams != OMPVV_NUM_TEAMS_DEVICE, "The number of teams was unexpected, the test results are likely inconcuslive")
	OMPVV_WARNING_IF(shared > (num_teams * testing_thread_limit), "The sum was higher than expected. This likely means thread_limit isn't capping the maximum threads created");
	OMPVV_WARNING_IF(testing_thread_limit == 1, "Only one thread was allocated to each team, the test results are likely inconclusive");
	

	OMPVV_REPORT_AND_RETURN(errors);
}

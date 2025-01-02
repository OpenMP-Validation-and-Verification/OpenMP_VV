//===--- test_target_groupprivate_devicetype_nohost.c ---------------------------===//
//
// OpenMP API Version 6.0
// Tests the target directive with the groupprivate directive ensuring proper 
// behavior inside the target region with device type nohost. 
//
////===----------------------------------------------------------------------===//


#include <omp.h>
#include <stdio.h>
#include "ompvv.h"

#define NUM_TEAMS 4

#pragma omp declare target
int group_sum;
#pragma omp groupprivate(group_sum) device_type(nohost)
#pragma omp end declare target

int group_sum_host;

int* target_var(){
    return &group_sum;
}

#pragma omp declare variant(target_var) match(device={kind(nohost)})
int* get_group_sum_func() {
    return &group_sum_host;
}

int test_target_groupprivate_devicetype_nohost(){
    int errors = 0;
    int host_sum = 0;
    int team_sum = 0;

    #pragma omp target teams num_teams(NUM_TEAMS) map(tofrom: team_sum, errors) reduction(+: team_sum)
    
    {
	if (omp_get_team_num() == 0 && omp_is_initial_device()){
	    errors++;
        }	    
	int *group_sum_pointer = get_group_sum_func();
        	
        *group_sum_pointer = omp_get_team_num();

        team_sum += *group_sum_pointer;
    
    }

    for (int i = 0; i < NUM_TEAMS; i++){
        host_sum += i;
    }
    OMPVV_TEST_AND_SET_VERBOSE(errors, team_sum != host_sum);
    return errors;
}

int main(){
        OMPVV_TEST_OFFLOADING;
        int errors = 0;
        OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_groupprivate_devicetype_nohost() != 0);
        OMPVV_REPORT_AND_RETURN(errors);
}


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

int target_func(){
    return omp_get_team_num();
}

#pragma omp declare variant(target_func) match(device={kind(nohost)})
int host_func() {
    return 1;
}

int test_target_groupprivate_devicetype_nohost(){
    int group_sum;
    #pragma omp groupprivate(group_sum) device_type(nohost)
    int errors = 0;
    int team_sum = 0;
    int host_sum = 0;
    #pragma omp target teams num_teams(NUM_TEAMS) map(tofrom: team_sum, errors) reduction(+: team_sum)
    
    {
	if (omp_get_team_num() == 0 && omp_is_initial_device()){
	    errors++;
        }	    
	group_sum = host_func();
        	
        team_sum += group_sum;
    
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


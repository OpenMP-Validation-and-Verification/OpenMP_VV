//===--------------------- test_target_get_max_teams.c ----------------------===//
//
// OpenMP API Version 5.1 Nov 2020
//
// This test uses the omp_get_max_teams routine to check what the max teams 
// capacity is for this device. It should return the total amount of teams
// that could be allocated to a specific teams region
//
//===------------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int main() {

        OMPVV_TEST_OFFLOADING;

        int errors = 0;
        int MAX_TEAMS = omp_get_max_teams(); //on host
        int num_teams = MAX_TEAMS + 1; //a value that is not possible
        printf("max on host:%d\n",MAX_TEAMS);
        #pragma omp teams
        {
                if (omp_get_team_num() == 0) {
                        num_teams = omp_get_num_teams();
                }
        }       

        OMPVV_ERROR_IF(MAX_TEAMS > 0 && num_teams > MAX_TEAMS, "Number of teams reported on host exceeded max number of teams (max no. > 0)");
        OMPVV_ERROR_IF(num_teams <= 0, "Number of teams reported on host is 0 or negative");
        OMPVV_TEST_AND_SET(errors, num_teams > MAX_TEAMS);
        OMPVV_TEST_AND_SET(errors, num_teams <= 0);
        
        #pragma omp target map(tofrom:num_teams)
        {
           MAX_TEAMS = omp_get_max_teams(); //first-private
          #pragma omp teams 
          {
             if (omp_get_team_num() == 0) {
               printf("max on target:%d\n",MAX_TEAMS);
               num_teams = MAX_TEAMS + 1; //a value that is not possible
               num_teams = omp_get_num_teams();
             }  
          }
        }
        OMPVV_ERROR_IF(MAX_TEAMS > 0 && num_teams > MAX_TEAMS, "Number of teams reported on device exceeded max number of teams (max no. > 0)");
        OMPVV_ERROR_IF(num_teams <= 0, "Number of teams reported on device is 0 or negative");
        OMPVV_TEST_AND_SET(errors, num_teams > MAX_TEAMS);
        OMPVV_TEST_AND_SET(errors, num_teams <= 0);

        OMPVV_REPORT_AND_RETURN(errors);
}

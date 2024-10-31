//===--- test_target_groupprivate.c -----------------------------------------===//
//
// OpenMP API Version 6.0
// Tests the target directive with the groupprivate directive ensuring proper behavior
//
////===----------------------------------------------------------------------===//


#include <omp.h>
#include <stdio.h>
#include "ompvv.h"

#define N 10
#define NUM_TEAMS 4

int group_sum;

int test_target_groupprivate(){
    int errors = 0;
    int host_sum = 0;
    int team_sum[NUM_TEAMS];
    for (int i = 0; i < NUM_TEAMS; i++){
        team_sum[i] = 0;
    }
    for (int i = 0; i < N; i++){
            host_sum += i;
    }

    #pragma omp target teams num_teams(NUM_TEAMS) map(tofrom: team_sum[:NUM_TEAMS]) groupprivate(group_sum)
    {
        group_sum = 0;
        for (int i = 0; i < N; i++){
            group_sum += i;
        }

        team_sum[omp_get_team_num()] = group_sum;
    }

    for (int i = 0; i < NUM_TEAMS; i++){
        OMPVV_TEST_AND_SET_VERBOSE(errors, team_sum[i] != host_sum);
    }
    return errors;
}

int main(){
        OMPVV_TEST_OFFLOADING;
        int errors = 0;
        OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_groupprivate() != 0);
        OMPVV_REPORT_AND_RETURN(errors);
}


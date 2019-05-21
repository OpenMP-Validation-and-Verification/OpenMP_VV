//===--- test_target_teams_distribute_reduction.c----------------------------===//
//
// OpenMP API Version 4.5 Nov 2015
//
// This test uses the reduction clause on a target teams distribute directive,
// testing, for each operator, that the variable in the reduction clause is
// properly reduced.
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

#define SIZE_THRESHOLD 512


int test_add(){
    int a[1024];
    int b[1024];
    int total = 0;
    int host_total = 0;
    int errors = 0;
    int num_teams = 0;

    for (int x = 0; x < 1024; ++x){
        a[x] = 1;
        b[x] = x;
    }

    #pragma omp target data map(tofrom: num_teams) map(to: a[0:1024], b[0:1024])
    {
        #pragma omp target teams distribute reduction(+:total) map(alloc: a[0:1024], b[0:1024], num_teams)
        for (int x = 0; x < 1024; ++x){
            num_teams = omp_get_num_teams();
            total += a[x] + b[x];
        }
    }

    for (int x = 0; x < 1024; ++x){
        host_total += a[x] + b[x];
    }

    OMPVV_TEST_AND_SET_VERBOSE(errors, host_total != total);

    if (num_teams == 1){
        OMPVV_WARNING("Test operated with one team.  Testing of reduction clause cannot be done.");
    }
    return errors;
}

int test_and(){
    char a[1024];
    char result;
    char host_result;
    double false_margin = pow(exp(1), log(.5)/1024);
    int errors = 0;
    int num_teams = 0;

    for (int itr_count = 0; itr_count < 16; ++itr_count){
        for (int x = 0; x < 1024; ++x){
            if (rand() / (double)(RAND_MAX) < false_margin){
                a[x] = 1;
            }
            else{
                a[x] = 0;
            }
        }

        result = 1;
        host_result = 1;

        #pragma omp target data map(to: a[0:1024]) map(tofrom: num_teams)
        {
            #pragma omp target teams distribute reduction(&&:result) map(alloc: a[0:1024], num_teams)
            for (int x = 0; x < 1024; ++x){
                num_teams = omp_get_num_teams();
                result = result && a[x];
            }
        }

        for (int x = 0; x < 1024; ++x){
            host_result = host_result && a[x];
        }

        OMPVV_TEST_AND_SET_VERBOSE(errors, host_result != result);
        if (host_result != result){
            break;
        }
    }

    if (num_teams == 1){
        OMPVV_WARNING("Test operated with one team.  Testing of reduction clause cannot be done.");
    }
    return errors;
}

int test_bitand(){
    unsigned int a[1024];
    double false_margin = pow(exp(1), log(.5)/1024);
    int errors = 0;
    int num_teams = 0;

    for (int x = 0; x < 1024; ++x){
        for (int y = 0; y < 16; ++y){
            if (rand() / (double) RAND_MAX < false_margin){
                a[x] += 1 << y;
            }
        }
    }

    unsigned int b = 0;
    for (int x = 0; x < 16; ++x){
        b = b + (1 << x);
    }

    #pragma omp target data map(tofrom: num_teams) map(to: a[0:1024])
    {
        #pragma omp target teams distribute reduction(&:b) map(alloc: a[0:1024], num_teams)
        for (int x = 0; x < 1024; ++x){
            num_teams = omp_get_num_teams();
            b = b & a[x];
        }
    }
    unsigned int host_b = a[0];

    for (int x = 0; x < 1024; ++x){
        host_b = host_b & a[x];
    }

    OMPVV_TEST_AND_SET_VERBOSE(errors, b != host_b);

    if (num_teams == 1){
        OMPVV_WARNING("Test operated with one team.  Testing of reduction clause cannot be done.");
    }
    return errors;
}

int test_bitor(){
    int a[1024];
    double false_margin = pow(exp(1), log(.5)/1024);
    int errors = 0;
    int num_teams = 0;

    for (int x = 0; x < 1024; ++x){
        for (int y = 0; y < 16; ++y){
            if (rand() / (double) RAND_MAX > false_margin){
                a[x] += (1 << y);
            }
        }
    }

    unsigned int b = 0;

    #pragma omp target data map(tofrom: num_teams) map(to: a[0:1024])
    {
        #pragma omp target teams distribute reduction(|:b) map(alloc: a[0:1024], num_teams)
        for (int x = 0; x < 1024; ++x){
            num_teams = omp_get_num_teams();
            b = b | a[x];
        }
    }

    unsigned int host_b = 0;

    for (int x = 0; x < 1024; ++x){
        host_b = host_b | a[x];
    }

    OMPVV_TEST_AND_SET_VERBOSE(errors, b != host_b);

    if (num_teams == 1){
        OMPVV_WARNING("Test operated with one team.  Testing of reduction clause cannot be done.");
    }
    return errors;
}

int test_bitxor(){
    unsigned int a[1024];
    int errors = 0;
    int num_teams = 0;

    for (int x = 0; x < 1024; ++x){
        a[x] = (unsigned int) rand() / (double) (RAND_MAX / 2);
    }

    unsigned int b = 0;

    #pragma omp target data map(tofrom: num_teams) map(to: a[0:1024])
    {
        #pragma omp target teams distribute reduction(^:b) map(alloc: a[0:1024], num_teams)
        for (int x = 0; x < 1024; ++x){
            num_teams = omp_get_num_teams();
            b = (b ^ a[x]);
        }
    }

    unsigned int host_b = 0;

    for (int x = 0; x < 1024; ++x){
        host_b = (host_b ^ a[x]);
    }

    OMPVV_TEST_AND_SET_VERBOSE(errors, b != host_b);

    if (num_teams == 1){
        OMPVV_WARNING("Test operated with one team.  Testing of reduction clause cannot be done.");
    }
    return errors;
}

int test_max(){
    int a[1024];
    int b[1024];
    int errors = 0;
    int num_teams = 0;

    for (int x = 0; x < 1024; ++x){
        a[x] = (int) rand() / (double)(RAND_MAX / 100);
        b[x] = (int) rand() / (double)(RAND_MAX / 100);
    }

    int result = 0;

    #pragma omp target data map(tofrom: num_teams) map(to: a[0:1024], b[0:1024])
    {
        #pragma omp target teams distribute reduction(max:result) map(alloc: a[0:1024], b[0:1024], num_teams)
        for (int x = 0; x < 1024; ++x){
            result = fmax(a[x] + b[x], result);
            num_teams = omp_get_num_teams();
        }
    }

    int host_max = 0;

    for (int x = 0; x < 1024; ++x){
        host_max = fmax(host_max, a[x] + b[x]);
    }

    OMPVV_TEST_AND_SET_VERBOSE(errors, result != host_max);

    if (num_teams == 1){
        OMPVV_WARNING("Test operated with one team.  Testing of reduction clause cannot be done.");
    }
    return errors;
}

int test_min(){
    int a[1024];
    int b[1024];
    int errors = 0;
    int num_teams = 0;

    for (int x = 0; x < 1024; ++x){
        a[x] = (int) rand() / (double) (RAND_MAX / 100);
        b[x] = (int) rand() / (double) (RAND_MAX / 100);
    }

    int result = a[0] + b[0];

    #pragma omp target data map(tofrom: num_teams) map(to: a[0:1024], b[0:1024])
    {
        #pragma omp target teams distribute reduction(min:result) map(alloc: a[0:1024], b[0:1024], num_teams)
        for (int x = 0; x < 1024; ++x){
            num_teams = omp_get_num_teams();
            result = fmin(result, a[x] + b[x]);
        }
    }

    int host_min = a[0] + b[0];

    for (int x = 0; x < 1024; ++x){
        host_min = fmin(host_min, a[x] + b[x]);
    }

    OMPVV_TEST_AND_SET_VERBOSE(errors, host_min != result);

    if (num_teams == 1){
        OMPVV_WARNING("Test operated with one team.  Testing of reduction clause cannot be done.");
    }
    return errors;
}

int test_multiply(){
    int a[1024];
    int errors = 0;
    int num_teams = 0;

    for (int x = 0; x < 1024; ++x){
        a[x] = 1 + (int) rand() / (double) RAND_MAX;
    }

    int result = 1;
    int host_result;

    #pragma omp target data map(tofrom: num_teams) map(to: a[0:1024])
    {
        for (int x = 0; x < 1024; x = x + 16){
            result = 1;
            #pragma omp target teams distribute reduction(*:result) map(alloc: a[0:1024], num_teams)
            for (int y = 0; y < 16; ++y){
                result *= a[x];
            }
            host_result = 1;
            for (int y = 0; y < 16; ++y){
                host_result *= a[x];
            }
            OMPVV_TEST_AND_SET_VERBOSE(errors, host_result != result);
            if (host_result != result){
                break;
            }
        }
    }

    if (num_teams == 1){
        OMPVV_WARNING("Test operated with one team.  Testing of reduction clause cannot be done.");
    }

    return errors;
}

int test_or(){
    char a[1024];
    double false_margin = pow(exp(1), log(.5)/1024);
    int errors = 0;
    int num_teams = 0;

    for (int x = 0; x < 1024; ++x){
        if (rand() / (double)(RAND_MAX) > false_margin){
            a[x] = 1;
        }
        else{
            a[x] = 0;
        }
    }

    char result = 0;

    #pragma omp target data map(tofrom: num_teams) map(to: a[0:1024])
    {
        #pragma omp target teams distribute reduction(||:result) map(alloc: a[0:1024], num_teams)
        for (int x = 0; x < 1024; ++x){
            num_teams = omp_get_num_teams();
            result = result || a[x];
        }
    }

    char host_result = 0;
    for (int x = 0; x < 1024; ++x){
        host_result = host_result || a[x];
    }

    OMPVV_TEST_AND_SET_VERBOSE(errors, host_result != result);

    if (num_teams == 1){
        OMPVV_WARNING("Test operated with one team.  Testing of reduction clause cannot be done.");
    }

    return errors;
}

int test_subtraction(){
  int a[1024];
  int b[1024];
  int total = 0;
  int host_total = 0;
  int errors = 0;
  int num_teams = 0;

  for (int x = 0; x < 1024; ++x){
      a[x] = 1;
      b[x] = x;
  }

  #pragma omp target data map(tofrom: num_teams) map(to: a[0:1024], b[0:1024])
  {
      #pragma omp target teams distribute reduction(-:total) map(alloc: a[0:1024], b[0:1024], num_teams)
      for (int x = 0; x < 1024; ++x){
          num_teams = omp_get_num_teams();
          total -= a[x] + b[x];
      }
  }

  for (int x = 0; x < 1024; ++x){
      host_total -= a[x] + b[x];
  }

  OMPVV_TEST_AND_SET_VERBOSE(errors, host_total != total);

  if (num_teams == 1){
      OMPVV_WARNING("Test operated with one team.  Testing of reduction clause cannot be done.");
  }
  return errors;
}

int main() {
  int total_errors = 0;
  OMPVV_TEST_AND_SET_VERBOSE(total_errors, test_add() != 0);
  OMPVV_TEST_AND_SET_VERBOSE(total_errors, test_and() !=0);
  OMPVV_TEST_AND_SET_VERBOSE(total_errors, test_bitand() != 0);
  OMPVV_TEST_AND_SET_VERBOSE(total_errors, test_bitor() != 0);
  OMPVV_TEST_AND_SET_VERBOSE(total_errors, test_bitxor() != 0);
  OMPVV_TEST_AND_SET_VERBOSE(total_errors, test_max() != 0);
  OMPVV_TEST_AND_SET_VERBOSE(total_errors, test_min() != 0);
  OMPVV_TEST_AND_SET_VERBOSE(total_errors, test_multiply() != 0);
  OMPVV_TEST_AND_SET_VERBOSE(total_errors, test_or() != 0);
  OMPVV_TEST_AND_SET_VERBOSE(total_errors, test_subtraction() != 0);
  OMPVV_REPORT_AND_RETURN(total_errors);
}

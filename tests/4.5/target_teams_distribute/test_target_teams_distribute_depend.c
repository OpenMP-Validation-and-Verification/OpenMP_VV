//===--- test_target_teams_distribute_depend.c-------------------------------===//
//
// OpenMP API Version 4.5 Nov 2015
//
// This test defines a series of functions that enumerate the possible
// combinations of the interactions of the depends clause with the various
// dependence-types: in, out, inout.  With each combination, it tests if
// the dependence between them (if necessary) is forced.  If there is no
// required dependence, then the test tries to see if race conditions between
// the two independent target regions can be formed.  However, if it fails
// to do so, it only issues a warning as this is both a imperfect test of
// the independence and it is not requried that they both execute at the
// same time.
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define SIZE_THRESHOLD 512

int test_target_teams_distribute_depend_in_in(){
    int isOffloading = 0;
    int a[1024];
    int b[1024];
    int c[1024];
    int all_valid = 1;
    int race_found = 0;

    for (int x = 0; x < 1024; ++x){
        a[x] = x;
        b[x] = 2 * x;
        c[x] = 0;
    }

    #pragma omp target data map(to: a[0:1024], b[0:1024]) map(tofrom:c[0:1024])
    {
        #pragma omp target teams distribute nowait depend(in:c) map(alloc: a[0:1024], b[0:1024], c[0:1024])
        for (int x = 0; x < 1024; ++x){
            c[x] += a[x] + b[x];
        }
        #pragma omp target teams distribute nowait depend(in:c) map(alloc: a[0:1024], b[0:1024], c[0:1024])
        for (int x = 0; x < 1024; ++x){
            c[x] += 2 * (a[x] + b[x]);
        }
    }

    for (int x = 0; x < 1024; ++x){
        if (!(c[x] == 3 * x || c[x] == 6 * x || c[x] == 9 * x)){
            all_valid = 0;
        }
        if (c[x] == 3 * x || c[x] == 6 * x){
            race_found = 1;
        }
    }

    if (!(all_valid == 1 && race_found == 1)){
        OMPVV_WARNING("Test could not prove asyncronous operations of depend(in) task with other depend(in) task with offloading %s", (isOffloading ? "enabled" : "disabled"));
    }
    return 0;
}

int test_target_teams_distribute_depend_in_out(){
    int isOffloading = 0;
    int a[1024];
    int b[1024];
    int c[1024];
    int d[1024];
    int errors = 0;

    for (int x = 0; x < 1024; ++x){
        a[x] = x;
        b[x] = 2 * x;
        c[x] = 0;
        d[x] = 0;
    }

    #pragma omp target data map(to: a[0:1024], b[0:1024]) map(alloc: c[0:1024]) map(from: d[0:1024])
    {
        #pragma omp target teams distribute nowait depend(in: c) map(alloc: a[0:1024], b[0:1024], c[0:1024])
        for (int x = 0; x < 1024; ++x){
            c[x] = a[x] + b[x];
        }
        #pragma omp target teams distribute nowait depend(out: c) map(alloc: b[0:1024], c[0:1024], d[0:1024])
        for (int x = 0; x < 1024; ++x){
            d[x] = c[x] + b[x];
        }
    }

    for (int x = 0; x < 1024; ++x){
        if (d[x] != 5 * x){
            errors = 1;
            break;
        }
    }

    #pragma omp target data map(to: a[0:1024], b[0:1024]) map(alloc: c[0:1024]) map(from: d[0:1024])
    {
        #pragma omp target teams distribute nowait depend(in: c) map(alloc: a[0:1024], b[0:1024], c[0:1024])
        for (int x = 0; x < 1024; ++x){
            c[x] = a[x] + b[x];
        }
        #pragma omp target teams distribute nowait depend(inout: c) map(alloc: a[0:1024], c[0:1024], d[0:1024])
        for (int x = 0; x < 1024; ++x){
            d[x] = c[x] + a[x];
        }
    }

    for (int x = 0; x < 1024; ++x){
        if (d[x] != 4 * x){
            if (errors == 1){
                OMPVV_ERROR("Test of depend(inout/out) task becoming dependent task of depend(in) task did not pass with offloading %s", (isOffloading ? "enabled" : "disabled"));
                return 1;
            }
            else{
                OMPVV_ERROR("Test of depend(inout) task becoming dependent task of depend(in) task did not pass with offloading %s", (isOffloading ? "enabled" : "disabled"))
                return 1;
            }
        }
    }
    if (errors == 1) {
        OMPVV_ERROR("Test of depend(out) task becoming dependent task of depend(in) task did not pass with offloading %s", (isOffloading ? "enabled" : "disabled"))
        return 1;
    }
    else{
        return 0;
    }
}

int test_target_teams_distribute_depend_out_out(){
    int isOffloading = 0;
    int a[1024];
    int b[1024];
    int c[1024];
    int d[1024];
    int out_out_errors = 0;
    int inout_out_errors = 0;
    int out_inout_errors = 0;
    int inout_inout_errors = 0;

    for (int x = 0; x < 1024; ++x){
        a[x] = x;
        b[x] = 2 * x;
        c[x] = 0;
        d[x] = 0;
    }

    #pragma omp target data map(to: a[0:1024], b[0:1024]) map(alloc: c[0:1024]) map(from: d[0:1024])
    {
        #pragma omp target teams distribute nowait depend(out: c) map(alloc: a[0:1024], b[0:1024], c[0:1024])
        for (int x = 0; x < 1024; ++x){
            c[x] = a[x] + b[x];
        }
        #pragma omp target teams distribute nowait depend(out: c) map(alloc: b[0:1024], c[0:1024], d[0:1024])
        for (int x = 0; x < 1024; ++x){
            d[x] = c[x] + b[x];
        }
    }

    for (int x = 0; x < 1024; ++x){
        if (d[x] != 5 * x){
            out_out_errors = 1;
            break;
        }
    }

    #pragma omp target data map(to: a[0:1024], b[0:1024]) map(alloc: c[0:1024]) map(from: d[0:1024])
    {
        #pragma omp target teams distribute nowait depend(out: c) map(alloc: a[0:1024], b[0:1024], c[0:1024])
        for (int x = 0; x < 1024; ++x){
            c[x] = a[x] + b[x];
        }
        #pragma omp target teams distribute nowait depend(inout: c) map(alloc: a[0:1024], c[0:1024], d[0:1024])
        for (int x = 0; x < 1024; ++x){
            d[x] = c[x] + a[x];
        }
    }

    for (int x = 0; x < 1024; ++x){
        if (d[x] != 4 * x){
            out_inout_errors = 1;
        }
    }

    #pragma omp target data map(to: a[0:1024], b[0:1024]) map(alloc: c[0:1024]) map(from: d[0:1024])
    {
        #pragma omp target teams distribute nowait depend(inout: c) map(alloc: a[0:1024], b[0:1024], c[0:1024])
        for (int x = 0; x < 1024; ++x){
            c[x] = a[x] + b[x];
        }
        #pragma omp target teams distribute nowait depend(out: c) map(alloc: b[0:1024], c[0:1024], d[0:1024])
        for (int x = 0; x < 1024; ++x){
            d[x] = c[x] + b[x];
        }
    }

    for (int x = 0; x < 1024; ++x){
        if (d[x] != 5 * x){
            inout_out_errors = 1;
        }
    }

    #pragma omp target data map(to: a[0:1024], b[0:1024]) map(alloc: c[0:1024]) map(from: d[0:1024])
    {
        #pragma omp target teams distribute nowait depend(inout: c) map(alloc: a[0:1024], b[0:1024], c[0:1024])
        for (int x = 0; x < 1024; ++x){
            c[x] = a[x] + b[x];
        }
        #pragma omp target teams distribute nowait depend(inout: c) map(alloc: a[0:1024], c[0:1024], d[0:1024])
        for (int x = 0; x < 1024; ++x){
            d[x] = c[x] + a[x];
        }
    }

    for (int x = 0; x < 1024; ++x){
        if (d[x] != 4 * x){
            inout_inout_errors = 1;
        }
    }

    if (out_out_errors == 1){
        OMPVV_ERROR("Test of depend(out) task becoming dependent task of depend(out) task did not pass with offloading %s", (isOffloading ? "enabled" : "disabled"));
    }
    if (inout_out_errors == 1){
        OMPVV_ERROR("Test of depend(out) task becoming dependent task of depend(inout) task did not pass with offloading %s", (isOffloading ? "enabled" : "disabled"));
    }
    if (out_inout_errors == 1){
        OMPVV_ERROR("Test of depend(inout) task becoming dependent task of depend(out) task did not pass with offloading %s", (isOffloading ? "enabled" : "disabled"));
    }
    if (inout_inout_errors == 1){
        OMPVV_ERROR("Test of depend(inout) task becoming dependent task of depend(inout) task did not pass with offloading %s", (isOffloading ? "enabled" : "disabled"));
    }

    if (inout_inout_errors + inout_out_errors + out_inout_errors + out_out_errors > 0){
        return 1;
    }
    else{
        return 0;
    }
}

int test_target_teams_distribute_depend_out_in(){
    int isOffloading = 0;
    int a[1024];
    int b[1024];
    int c[1024];
    int d[1024];
    int errors = 0;

    for (int x = 0; x < 1024; ++x){
        a[x] = x;
        b[x] = 2 * x;
        c[x] = 0;
        d[x] = 0;
    }

    #pragma omp target data map(to: a[0:1024], b[0:1024]) map(alloc: c[0:1024]) map(from: d[0:1024])
    {
        #pragma omp target teams distribute nowait depend(out: c) map(alloc: a[0:1024], b[0:1024], c[0:1024])
        for (int x = 0; x < 1024; ++x){
            c[x] = a[x] + b[x];
        }
        #pragma omp target teams distribute nowait depend(in: c) map(alloc: b[0:1024], c[0:1024], d[0:1024])
        for (int x = 0; x < 1024; ++x){
            d[x] = c[x] + b[x];
        }
    }

    for (int x = 0; x < 1024; ++x){
        if (d[x] != 5 * x){
            errors = 1;
            break;
        }
    }

    #pragma omp target data map(to: a[0:1024], b[0:1024]) map(alloc: c[0:1024]) map(from: d[0:1024])
    {
        #pragma omp target teams distribute nowait depend(inout: c) map(alloc: a[0:1024], b[0:1024], c[0:1024])
        for (int x = 0; x < 1024; ++x){
            c[x] = a[x] + b[x];
        }
        #pragma omp target teams distribute nowait depend(in: c) map(alloc: a[0:1024], c[0:1024], d[0:1024])
        for (int x = 0; x < 1024; ++x){
            d[x] = c[x] + a[x];
        }
    }

    for (int x = 0; x < 1024; ++x){
        if (d[x] != 4 * x){
            if (errors == 1){
                OMPVV_ERROR("Test of depend(in) task becoming dependent task of depend(inout/in) task did not pass with offloading %s", (isOffloading ? "enabled" : "disabled"));
                return 1;
            }
            else{
                OMPVV_ERROR("Test of depend(in) task becoming dependent task of depend(inout) task did not pass with offloading %s", (isOffloading ? "enabled" : "disabled"))
                return 1;
            }
        }
    }
    if (errors == 1) {
        OMPVV_ERROR("Test of depend(in) task becoming dependent task of depend(out) task did not pass with offloading %s", (isOffloading ? "enabled" : "disabled"))
        return 1;
    }
    else{
        return 0;
    }
}

int test_target_teams_distribute_depend_array_section(){
    int isOffloading = 0;
    int a[1024];
    int b[1024];
    int c[1024];
    int d[1024];

    for (int x = 0; x < 1024; ++x){
        a[x] = x;
        b[x] = 2 * x;
        c[x] = 0;
        d[x] = 0;
    }

    #pragma omp target data map(to: a[0:1024], b[0:1024]) map(alloc: c[0:1024]) map(from: d[0:1024])
    {
        #pragma omp target teams distribute nowait depend(out: c[0:1024]) map(alloc: a[0:1024], b[0:1024], c[0:1024])
        for (int x = 0; x < 1024; ++x){
            c[x] = a[x] + b[x];
        }
        #pragma omp target teams distribute nowait depend(out: c[0:1024]) map(alloc: b[0:1024], c[0:1024], d[0:1024])
        for (int x = 0; x < 1024; ++x){
            d[x] = c[x] + b[x];
        }
    }

    for (int x = 0; x < 1024; ++x){
        if (d[x] != 5 * x){
            OMPVV_ERROR("Test of depend clause using array sections did not pass with offloading %s", (isOffloading ? "enabled" : "disabled"));
            return 1;
        }
    }
    return 0;
}

int test_target_teams_distribute_depend_disjoint_section(){
    int isOffloading = 0;
    int a[1024];
    int b[1024];
    int c[1024];
    int all_valid = 1;
    int race_found = 0;

    for (int x = 0; x < 1024; ++x){
        a[x] = x;
        b[x] = 2 * x;
        c[x] = 0;
    }

    #pragma omp target data map(to: a[0:1024], b[0:1024]) map(tofrom:c[0:1024])
    {
        #pragma omp target teams distribute nowait depend(in:c[0:512]) map(alloc: a[0:1024], b[0:1024], c[0:1024])
        for (int x = 0; x < 1024; ++x){
            c[x] += a[x] + b[x];
        }
        #pragma omp target teams distribute nowait depend(in:c[512:512]) map(alloc: a[0:1024], b[0:1024], c[0:1024])
        for (int x = 0; x < 1024; ++x){
            c[x] += 2 * (a[x] + b[x]);
        }
    }

    for (int x = 0; x < 1024; ++x){
        if (!(c[x] == 3 * x || c[x] == 6 * x || c[x] == 9 * x)){
            all_valid = 0;
        }
        if (c[x] == 3 * x || c[x] == 6 * x){
            race_found = 1;
        }
    }

    if (!(all_valid == 1 && race_found == 1)){
        OMPVV_WARNING("Test could not prove asyncronous operations of tasks dependent on disjoint array sections");
    }
    return 0;
}

int test_target_teams_distribute_depend_list(){
    int isOffloading = 0;
    int a[1024];
    int b[1024];
    int c[1024];
    int d[1024];
    int e[1024];
    int f[1024];
    int g[1024];

    for (int x = 0; x < 1024; ++x){
        a[x] = x;
        b[x] = 2 * x;
        c[x] = 0;
        d[x] = 0;
        e[x] = 0;
        f[x] = 0;
        g[x] = 0;
    }

    #pragma omp target data map(to: a[0:1024], b[0:1024]) map(alloc: c[0:1024], d[0:1024], e[0:1024]) map(from: f[0:1024], g[0:1024])
    {
        #pragma omp target teams distribute nowait depend(out: c) map(alloc: a[0:1024], b[0:1024], c[0:1024])
        for (int x = 0; x < 1024; ++x){
            c[x] = a[x] + b[x];
        }
        #pragma omp target teams distribute nowait depend(out: d) map(alloc: a[0:1024], b[0:1024], d[0:1024])
        for (int x = 0; x < 1024; ++x){
            d[x] = a[x] + b[x] + x;
        }
        #pragma omp target teams distribute nowait depend(out: c, d, e) map(alloc: c[0:1024], d[0:1024], e[0:1024])
        for (int x = 0; x < 1024; ++x){
            e[x] = c[x] + d[x];
        }
        #pragma omp target teams distribute nowait depend(out: e) map(alloc: a[0:1024], e[0:1024], f[0:1024])
        for (int x = 0; x < 1024; ++x){
            f[x] = e[x] + a[x];
        }
        #pragma omp target teams distribute nowait depend(out: e) map(alloc: b[0:1024], e[0:1024], g[0:1024])
        for (int x = 0; x < 1024; ++x){
            g[x] = e[x] + b[x];
        }
    }

    for (int x = 0; x < 1024; ++x){
        if (f[x] != 8 * x || g[x] != 9 * x){
            OMPVV_ERROR("Test of depend clause using multiple dependencies did not pass with offloading %s", (isOffloading ? "enabled" : "disabled"));
            return 1;
        }
    }
    return 0;
}

int test_target_teams_distribute_depend_unused_data(){
    int isOffloading = 0;
    int a[1024];
    int b[1024];
    int c[1024];
    int d[1024];
    int random_data[1];

    for (int x = 0; x < 1024; ++x){
        a[x] = x;
        b[x] = 2 * x;
        c[x] = 0;
        d[x] = 0;
    }

    #pragma omp target data map(to: a[0:1024], b[0:1024]) map(alloc: c[0:1024], random_data[0:1]) map(from: d[0:1024])
    {
        #pragma omp target teams distribute nowait depend(out: random_data) map(alloc: a[0:1024], b[0:1024], c[0:1024])
        for (int x = 0; x < 1024; ++x){
            c[x] = a[x] + b[x];
        }
        #pragma omp target teams distribute nowait depend(out: random_data) map(alloc: b[0:1024], c[0:1024], d[0:1024])
        for (int x = 0; x < 1024; ++x){
            d[x] = c[x] + b[x];
        }
    }

    for (int x = 0; x < 1024; ++x){
        if (d[x] != 5 * x){
            OMPVV_ERROR("Test of depend clause using array sections did not pass with offloading %s", (isOffloading ? "enabled" : "disabled"));
            return 1;
        }
    }
    return 0;
}


int main() {
  int errors = 0;
  int isOffloading = 0;
  OMPVV_TEST_AND_SET_OFFLOADING(isOffloading);
  errors += test_target_teams_distribute_depend_in_in();
  errors += test_target_teams_distribute_depend_in_out();
  errors += test_target_teams_distribute_depend_out_in();
  errors += test_target_teams_distribute_depend_out_out();
  errors += test_target_teams_distribute_depend_array_section();
  errors += test_target_teams_distribute_depend_disjoint_section();
  errors += test_target_teams_distribute_depend_list();
  errors += test_target_teams_distribute_depend_unused_data();
  if (errors != 0){
      OMPVV_INFOMSG("Test passed with offloading %s", (isOffloading ? "enabled" : "disabled"));
  }
  OMPVV_REPORT_AND_RETURN(errors);
}

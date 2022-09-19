//---------------test_omp_places_numa_domain--------------------//
//This test intends to test the omp_places numa domain option. 
//First the test sets the value of omp_placesto numa_domain. 
//Then the test checks for equal distribution in threads.
//-------------------------------------------------------------//

#include <omp.h>
#include <stdio.h>
#include "ompvv.h"
#include <math.h>

#define N 1024
int test_places(){
	int errors = 0;
	int arr[N];
	for (int i = 0; i < N; i++){
		arr[i] = -1;
	}
	setenv("OMP_PLACES", "numa_domains(2)",1);
	omp_set_num_threads(4);
	#pragma omp parallel shared(arr) 
		for (int i = 0; i<N; i++){
			arr[N] = omp_get_thread_num();
		}
	int to = 0;
	int t1 = 0;
	int t2 = 0;
	int t3 = 0;
	for (int i = 0; i < N; i++){
		if (arr[i] == 0){
			to += 1;
		}
		else if (arr[i] == 1){
			t1 += 1;
		}
		else if (arr[i] == 2){
			t2 += 1
		}
		else if (arr[i] == 3){
			t3 += 1;
		}
	} 
	//TODO add test cases
}


int main(){
	int errors = 0;
	OMPVV_TEST_OFFLOADING;
	OMPVV_TEST_AND_SET_VERBOSE(errors, test_places() != 0);
	OMPVV_REPORT_AND_RETURN(errors);
}

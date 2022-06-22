//--------------- test_scope_construct.c---------------------------------//
//
// OpenMP API Version 5.1 Aug 2021
//
// The objective of this test is ensure the scope construct works as 
// intended. The scope construct defines a structured block that is
// executed by all threads in a team but where additional OpenMP operation
// can be specified. This test follows the example given by the OpenMP
// 5.1 Specification Example sheet. 
//-----------------------------------------------------------------------//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

#define N 1024

int test_scope(int n, int a[], int s){
	#pragma omp parallel
	{
		float loc_s = 0.0f;
		static int nthrs;
		#pragma omp for
		{
			for (int i = 0; i < n; i++)
				loc_s += a[i];
		}
		#pragma omp single
		{
			s += loc_s;
			nthrs++;
		}
		#pragma omp scope reduction(+:s,nthrs)
		{
			s += loc_s;
			nthrs++;
		}
	}
}

int main(){
	int a[N];
	int s = 0;
	for (int i; i < N; i++){
		a[i] = 1;
	}
	test_scope(N,a,s);
	return s;

}




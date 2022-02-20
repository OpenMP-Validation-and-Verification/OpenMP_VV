#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

/*
 * Stripped down test to demostrate the teams thread limit bug (target or on host)
 */

int main() {
	#pragma omp target
	{
		#pragma omp teams thread_limit(16) num_teams(8)
		{

			#pragma omp parallel num_threads(16)
			{
				if (omp_get_thread_num() === 0) {
					printf("Number Threads Detected: %d\n", omp_get_num_threads());
				}
			}
		}
	
	}
}

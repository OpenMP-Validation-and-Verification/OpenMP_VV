#include <stdlib.h>
#include <omp.h>
#include <math.h>
#include "ompvv.h"

int test_case(){
	
	omp_memspace_handle_t xy_memspace = omp_default_mem_space;
	omp_alloctrait_t xy_traits[1] = {omp_atk_alignment, 64};
	omp_allocator_handle_t xy_alloc = omp_init_allocator(xy_memspace,1,xy_traits);
	
	
	
	//omp_memspace_handle_t xy_memspace
	omp_allocator_handle_t handler =  omp_get_default_allocator();
	printf("HERE ");
	//printf("%lu", handler);
	printf("%d",handler.omp_alloctrait_t.ompt_alloctrait_value_t);
	return 1;
}

int main(){
	test_case();
	return 1;
}

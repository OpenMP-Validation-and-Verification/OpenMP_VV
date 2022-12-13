//===------ test_omp_aligned_alloc.c ------------------------------------===//
//
// OpenMP API Version 5.1 Nov 2020
//
// Tests the allocate directive with allocator and align clause.
// The declarative allocator statement uses the omp_default_mem_alloc handle
// for default memory allocation for "x", aligned to 64-byte alignment via the 
// align clause. Parallel region checks that 64-byte alignment is correct 
// and that the memory can be written to in and the values
// were written correctly, and then frees the memory.
//
//===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024



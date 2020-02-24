#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

#define N 1000

int test_struct() {
   
    puts("test_struct");

    int errors = 0, isHost = -1;
    int* pointers[6];

    struct {
        int a; // firstprivate
        int b[N]; // tofrom:b[0:N]
        int *p; // tofrom:p[0:0]
    } single, array[5];

    single.p = (int*) malloc(5 * sizeof(int));
    pointers[0] = single.p;

    for (int i = 0; i < 5; ++i) {
        array[i].p = (int*) malloc(5 * sizeof(int));
        pointers[i + 1] = array[i].p;
    }

    // unstructured mapping
    {
#pragma omp target enter data map(to: single) map(to: array[0:5])
        printf(""); // forcing the compiler to not moving out of the scope
    }
    // operation
#pragma omp target map(tofrom: isHost) map(alloc: single) map(alloc: array[0:5])
    {
      isHost = omp_is_initial_device();
      single.a = 1;
      for (int i = 0; i < N; ++i)
          single.b[i] = 1;
      
      for (int i = 0; i < 5; ++i) {
          array[i].a = 1;
          for (int j = 0; j < N; ++j)
              array[i].b[j] = 1;
      }
    }
    // unstructured exit
    {
#pragma omp target exit data map(from: single) map(from: array[0:5])
        printf("");
    }

    // checking results
    errors |= (single.a != 1); 
    for (int i = 0; i < N; ++i)
        errors |= (single.b[i] != 1);
    errors |= (pointers[0] != single.p);
    for (int i = 0; i < 5; ++i) {
        errors |= (array[i].a != 1); 
        for (int j = 0; j < N; ++j)
            errors |= (array[i].b[j] != 1);
        errors |= (pointers[i + 1] != array[i].p);
    }

    if (!errors)
      printf("Test passed on %s\n", (isHost ? "host" : "device"));
    else
      printf("Test failed on %s\n", (isHost ? "host" : "device"));

    return errors;
}

int test_typedef() {
    
    puts("test_typedef");
    
    int errors = 0, isHost = -1;
    int* pointers[6];

    typedef struct {
        int a;
        int b[N];
        int *p;
    } test_struct;
 
    test_struct single, array[5];

    single.p = (int*) malloc(5 * sizeof(int));
    pointers[0] = single.p;

    for (int i = 0; i < 5; ++i) {
        array[i].p = (int*) malloc(5 * sizeof(int));
        pointers[i + 1] = array[i].p;
    }

    // unstructured mapping
    {
#pragma omp target enter data map(to: single) map(to: array[0:5])
        printf(""); // forcing the compiler to not moving out of the scope
    }
    // operation
#pragma omp target map(tofrom: isHost) map(alloc: single) map(alloc: array[0:5])
    {
      isHost = omp_is_initial_device();
      single.a = 1;
      for (int i = 0; i < N; ++i)
          single.b[i] = 1;
      
      for (int i = 0; i < 5; ++i) {
          array[i].a = 1;
          for (int j = 0; j < N; ++j)
              array[i].b[j] = 1;
      }
    }
    // unstructured exit
    {
#pragma omp target exit data map(from: single) map(from: array[0:5])
        printf("");
    }

    // checking results
    errors |= (single.a != 1); 
    for (int i = 0; i < N; ++i)
        errors |= (single.b[i] != 1);
    errors |= (pointers[0] != single.p);
    for (int i = 0; i < 5; ++i) {
        errors |= (array[i].a != 1); 
        for (int j = 0; j < N; ++j)
            errors |= (array[i].b[j] != 1);
        errors |= (pointers[i + 1] != array[i].p);
    }

    if (!errors)
      printf("Test passed on %s\n", (isHost ? "host" : "device"));
    else
      printf("Test failed on %s\n", (isHost ? "host" : "device"));

    return errors;
}

int main () {
    int errors = 0;
    errors += test_struct();
    errors += test_typedef();
    return errors;
}

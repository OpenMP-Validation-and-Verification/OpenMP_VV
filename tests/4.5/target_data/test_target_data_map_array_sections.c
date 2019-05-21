#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

#define N 1000

void init_1d(int* a);
void init_2d(int a[N][2]);
void init_3d(int a[N][2][2]);

// Test for OpenMP 4.5 target data map with array section [lower:length]
int test_lower_length_1d() {
  // array sections of the form a[lower:length] 
  puts("test_lower_length_1d");

  int errors = 0, isHost = 0;

  int a1d[N];
  init_1d(a1d);

#pragma omp target data map(from: a1d[1:N - 2])
  {
#pragma omp target map(tofrom: isHost) \
 map(alloc: a1d[1:N - 2]) // to avoid default mapping tofrom
    {
      isHost = omp_is_initial_device();
      for (int i = 1; i < N - 1; ++i)
        a1d[i] = 1;
    } // end target
  } // end target data

  // checking errors
  for (int i = 0; i < N; ++i) {
    if (i == 0 || i == N - 1)
      errors += a1d[i] == 0 ? 0 : 1;
    else
      errors += a1d[i] == 1 ? 0 : 1;
  }

  if (!errors)
    printf("Test passed on %s\n", (isHost ? "host" : "device"));
  else
    printf("Test failed on %s\n", (isHost ? "host" : "device"));

  return errors;
}

// Test for OpenMP 4.5 target data map with array 2d section [lower:length]
int test_lower_length_2d() {
  // array sections of the form a[lower:length]
  puts("test_lower_length_2d");

  int errors = 0, isHost = 0;

  // stack
  int a2d[N][2];
  init_2d(a2d);

  // OpenMP API v 4.5 Nov 2015. 2.15.5.1 map Clause, page 218 line 17: 
  // If a list item is an array section, it must specify contiguous storage.
#pragma omp target data map(from: a2d[1:N - 2][0:2])
  {
#pragma omp target map(tofrom: isHost) \
 map(alloc: a2d[1:N - 2][0:2]) // to avoid default mapping tofrom
    {
      isHost = omp_is_initial_device();
      for (int i = 1; i < N - 1; ++i) {
        a2d[i][0] = 1;
        a2d[i][1] = 1;
      }
    } // end target
  } // end target data

  // checking errors 
  for (int i = 0; i < N; ++i) {
    if (i == 0 || i == N - 1)
      errors += a2d[i][0] == 0 && a2d[i][1] == 0 ? 0 : 1;
    else
      errors += a2d[i][0] == 1 && a2d[i][1] == 1 ? 0 : 1;
  }

  if (!errors)
    printf("Test passed on %s\n", (isHost ? "host" : "device"));
  else
    printf("Test failed on %s\n", (isHost ? "host" : "device"));

  return errors;
}

// Test for OpenMP 4.5 target data map with array 3d section [lower:length]
int test_lower_length_3d() {
  // array sections of the form a[lower:length] 
  puts("test_lower_length_3d");
  // If a list item is an array section, it must specify contiguous storage. 

  int errors = 0, isHost = 0;

  // stack
  int a3d[N][2][2];
  init_3d(a3d);
  int a3d2[N][2][2];
  init_3d(a3d2);

  // OpenMP API v 4.5 Nov 2015. 2.15.5.1 map Clause, page 218 line 17: 
  // If a list item is an array section, it must specify contiguous storage.
#pragma omp target data map(from: a3d[1:N - 2][0:2][0:2])  \
        map(from: a3d2[0:N][0:2][0:2])
  {
#pragma omp target map(tofrom: isHost) \
 map(alloc: a3d[1:N - 2][0:2][0:2] ,a3d2[0:N][0:2][0:2]) // to avoid default mapping tofrom
    {
      isHost = omp_is_initial_device();
      for (int i = 0; i < N; ++i) {
        for (int j = 0; j < 2; ++j) {
          if (i > 0 && i < N - 1) {
            a3d[i][j][0] = 1;
            a3d[i][j][1] = 1;
          }
          a3d2[i][j][0] = 1;
          a3d2[i][j][1] = 1;
        }
      }
    } // end target
  } // end target data

  // checking errors
  for (int i = 0; i < N; ++i) {
    for (int j = 0; j < 2; ++j) {
      // a3d
      if (i == 0 || i == N - 1)
        errors += a3d[i][j][0] == 0 && a3d[i][j][1] == 0 ? 0 : 1;
      else
        errors += a3d[i][j][0] == 1 && a3d[i][j][1] == 1 ? 0 : 1;
      // a3d2
      errors += a3d2[i][j][0] == 1 && a3d2[i][j][1] == 1 ? 0 : 1;
    }
  }

  if (!errors)
    printf("Test passed on %s\n", (isHost ? "host" : "device"));
  else
    printf("Test failed on %s\n", (isHost ? "host" : "device"));

  return errors;
}

// Test for OpenMP 4.5 target data map with array 1d section [:length]
int test_length_1d() {
  // array sections of the form a[:length]
  puts("test_length_1d");

  int errors = 0, isHost = 0;

  int a1d[N];
  init_1d(a1d);

  // OpenMP API - V4.5 Nov2015. 2.4. Array sections, page 45 line 14: 
  // When the lower-bound is absent it defaults to 0.
#pragma omp target data map(from: a1d[:N - 2]) 
  {
#pragma omp target map(tofrom: isHost) \
 	map(alloc: a1d[:N - 2]) // to avoid default mapping tofrom
    {
      isHost = omp_is_initial_device();
      for (int i = 0; i < N - 2; ++i)
        a1d[i] = 1;
    } // end target
  } // end target data

  // checking errors
  for (int i = 0; i < N - 2; ++i)
    errors += a1d[i] == 1 ? 0 : 1;
  // N-2
  errors += a1d[N - 2] == 0 ? 0 : 1;
  // N-1
  errors += a1d[N - 1] == 0 ? 0 : 1;

  if (!errors)
    printf("Test passed on %s\n", (isHost ? "host" : "device"));
  else
    printf("Test failed on %s\n", (isHost ? "host" : "device"));

  return errors;
}

// Test for OpenMP 4.5 target data map with array 2d section [:length]
int test_length_2d() {
  // array sections of the form a[:length]
  puts("test_length_2d");

  int errors = 0, isHost = 0;

  int a2d[N][2];
  init_2d(a2d);

  // OpenMP API - V4.5 Nov2015. 2.4. Array sections, page 45 line 14: 
  // When the lower-bound is absent it defaults to 0.
  // OpenMP API v 4.5 Nov 2015. 2.15.5.1 map Clause, page 218 line 17:
  // If a list item is an array section, it must specify contiguous storage.
#pragma omp target data map(from: a2d[:N - 2][:2])
  {
#pragma omp target map(tofrom: isHost) \
 	map(alloc: a2d[:N - 2][:2]) // To avoid default mapping tofrom
    {
      isHost = omp_is_initial_device();
      for (int i = 0; i < N - 2; ++i) {
        a2d[i][0] = 1;
        a2d[i][1] = 1;
      }
    } // end target
  } // end target data

  // checking errors
  for (int i = 0; i < N - 2; ++i)
    errors += a2d[i][0] == 1 && a2d[i][1] == 1 ? 0 : 1;
  errors += a2d[N - 2][0] == 0 && a2d[N - 2][1] == 0 ? 0 : 1;
  errors += a2d[N - 1][0] == 0 && a2d[N - 1][1] == 0 ? 0 : 1;

  if (!errors)
    printf("Test passed on %s\n", (isHost ? "host" : "device"));
  else
    printf("Test failed on %s\n", (isHost ? "host" : "device"));

  return errors;
}

// Test for OpenMP 4.5 target data map with array 3d section [:length]
int test_length_3d() {
  // array sections of the form a[:length]
  puts("test_length_3d");

  int errors = 0, isHost = 0;

  int a3d[N][2][2];
  init_3d(a3d);
  int a3d2[N][2][2];
  init_3d(a3d2);

  // OpenMP API - V4.5 Nov2015. 2.4. Array sections, page 45 line 14:
  // When the lower-bound is absent it defaults to 0.
  // OpenMP API v 4.5 Nov 2015. 2.15.5.1 map Clause, page 218 line 17:
  // If a list item is an array section, it must specify contiguous storage.
#pragma omp target data map(from: a3d[:N - 2][:2][:2])   \
        map(from: a3d2[:N][:2][:2])
  {
#pragma omp target map(tofrom: isHost) \
        map(alloc: a3d[:N - 2][:2][:2], a3d2[:N][:2][:2]) // To avoid default mapping tofrom
    {
      isHost = omp_is_initial_device();
      for (int i = 0; i < N; ++i) {
        for (int j = 0; j < 2; ++j) {
          if (i < N - 2) {
            a3d[i][j][0] = 1;
            a3d[i][j][1] = 1;
          }
          a3d2[i][j][0] = 1;
          a3d2[i][j][1] = 1;
        }
      }
    } // end target
  } // end target data

  // checking errors
  for (int i = 0; i < N; ++i) {
    for (int j = 0; j < 2; ++j) {
      if (i >= N - 2)
        errors += a3d[i][j][0] == 0 && a3d[i][j][1] == 0 ? 0 : 1;
      else
        errors += a3d[i][j][0] == 1 && a3d[i][j][1] == 1 ? 0 : 1;
      // a3d2
      errors += a3d2[i][j][0] == 1 && a3d2[i][j][1] == 1 ? 0 : 1;
    }
  }

  if (!errors)
    printf("Test passed on %s\n", (isHost ? "host" : "device"));
  else
    printf("Test failed on %s\n", (isHost ? "host" : "device"));

  return errors;
}

// Test for OpenMP 4.5 target data map with array 1d section [lower:]
int test_lower_1d() {
  // array sections of the form a[lower:]
  puts("test_lower_1d");

  int errors = 0, isHost = 0;

  int a1d[N];
  init_1d(a1d);

  // OpenMP API - V4.5 Nov2015. 2.4. Array sections, page 45 line 13:
  // When the length is absent, it defaults to the size of the array
  // dimension minus the lower-bound.
#pragma omp target data map(from: a1d[1:])
  {
#pragma omp target map(tofrom: isHost) \
	map(alloc: a1d[1:]) // To avoid default mapping tofrom
    {
      isHost = omp_is_initial_device();
      for (int i = 1; i < N; ++i)
        a1d[i] = 1;
    } // end target
  } // end target data

  // checking errors 
  for (int i = 0; i < N; ++i) {
    if (i == 0)
      errors += a1d[i] == 0 ? 0 : 1;
    else
      errors += a1d[i] == 1 ? 0 : 1;
  }

  if (!errors)
    printf("Test passed on %s\n", (isHost ? "host" : "device"));
  else
    printf("Test failed on %s\n", (isHost ? "host" : "device"));

  return errors;
}

// Test for OpenMP 4.5 target data map with array 2d section [lower:]
int test_lower_2d() {
  // array sections of the form a[lower:] 
  puts("test_lower_2d");

  int errors = 0, isHost = 0;

  int a2d[N][2];
  init_2d(a2d);

  // OpenMP API - V4.5 Nov2015. 2.4. Array sections, page 45 line 13: 
  // When the length is absent, it defaults to the size of the array 
  // dimension minus the lower-bound.
  // OpenMP API v 4.5 Nov 2015. 2.15.5.1 map Clause, page 218 line 17:
  // If a list item is an array section, it must specify contiguous storage.
#pragma omp target data map(from: a2d[1:][0:])
  {
#pragma omp target map(tofrom: isHost) \
	map(alloc: a2d[1:][0:]) // To avoid default mapping tofrom
    {
      isHost = omp_is_initial_device();
      for (int i = 1; i < N; ++i) {
        a2d[i][0] = 1;
        a2d[i][1] = 1;
      }
    } // end target
  } // end target data

  // checking errors 
  for (int i = 0; i < N; ++i) {
    if (i == 0)
      errors += a2d[i][0] == 0 && a2d[i][1] == 0 ? 0 : 1;
    else
      errors += a2d[i][0] == 1 && a2d[i][1] == 1 ? 0 : 1;
  }

  if (!errors)
    printf("Test passed on %s\n", (isHost ? "host" : "device"));
  else
    printf("Test failed on %s\n", (isHost ? "host" : "device"));

  return errors;
}

// Test for OpenMP 4.5 target data map with array 3d section [lower:]
int test_lower_3d() {
  // array sections of the form a[lower:] 
  puts("test_lower_3d");

  int errors = 0, isHost = 0;

  int a3d[N][2][2];
  init_3d(a3d);
  int a3d2[N][2][2];
  init_3d(a3d2);

  // OpenMP API - V4.5 Nov2015. 2.4. Array sections, page 45 line 13: 
  // When the length is absent, it defaults to the size of the array 
  // dimension minus the lower-bound.
  // OpenMP API v 4.5 Nov 2015. 2.15.5.1 map Clause, page 218 line 17: 
  // If a list item is an array section, it must specify contiguous storage.
#pragma omp target data map(from: a3d[1:][0:][0:])   \
        map(from: a3d2[0:][0:][0:])
  {
#pragma omp target map(tofrom: isHost) \
	map(alloc: a3d[1:][0:][0:], a3d2[0:][0:][0:]) // To avoid default mapping tofrom
    {
      isHost = omp_is_initial_device();
      for (int i = 0; i < N; ++i) {
        for (int j = 0; j < 2; ++j) {
          if (i > 0) {
            a3d[i][j][0] = 1;
            a3d[i][j][1] = 1;
          }
          a3d2[i][j][0] = 1;
          a3d2[i][j][1] = 1;
        }
      }
    } // end target
  } // end target data

  // checking errors 
  for (int i = 0; i < N; ++i) {
    for (int j = 0; j < 2; ++j) {
      // a3d
      if (i == 0)
        errors += a3d[i][j][0] == 0 && a3d[i][j][1] == 0 ? 0 : 1;
      else
        errors += a3d[i][j][0] == 1 && a3d[i][j][1] == 1 ? 0 : 1;
      // a3d2
      errors += a3d2[i][j][0] == 1 && a3d2[i][j][1] == 1 ? 0 : 1;
    }
  }

  if (!errors)
    printf("Test passed on %s\n", (isHost ? "host" : "device"));
  else
    printf("Test failed on %s\n", (isHost ? "host" : "device"));

  return errors;
}

int main() {

  int errors = 0;

  errors += test_lower_length_1d();
  errors += test_lower_length_2d();
  errors += test_lower_length_3d();
  errors += test_length_1d();
  errors += test_length_2d();
  errors += test_length_3d();
  errors += test_lower_1d();
  errors += test_lower_2d();
  errors += test_lower_3d();

  return errors;
}

void init_1d(int* a) {
  for (int i = 0; i < N; ++i)
    a[i] = 0;
}

void init_2d(int a[N][2]) {
  for (int i = 0; i < N; ++i) {
    a[i][0] = 0;
    a[i][1] = 0;
  }
}

void init_3d(int a[N][2][2]) {
  for (int i = 0; i < N; ++i)
    for (int j = 0; j < 2; ++j) {
      a[i][j][0] = 0;
      a[i][j][1] = 0;
    }
}

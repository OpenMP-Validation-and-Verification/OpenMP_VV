
#include <iostream>
#include <omp.h>
#include <cassert>

using namespace std;

#define N 1000

class Simple {
private:
  int *h_array;
  int size;
  int sum;

public:
  Simple(int s) : size(s) { 
      sum = 0;
      h_array = new int[size];
#pragma omp target enter data map(to:this)
#pragma omp target enter data map(to:h_array[0:size])
  }

  // TODO: Add virtual once supported
  ~Simple() { 
#pragma omp target exit data map(delete:h_array[0:size])
#pragma omp target exit data map(delete:this)
      delete[] h_array; 
  }
};

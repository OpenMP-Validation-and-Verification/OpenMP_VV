#include "ompvv.h"
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
      Simple * this_ptr = this; // This is the offered solution
#pragma omp target enter data map(to:this_ptr)
#pragma omp target enter data map(to:h_array[0:size])
  }

  // TODO: Add virtual once supported
  ~Simple() { 
#pragma omp target exit data map(delete:h_array[0:size])
      Simple * this_ptr = this; // This is the offered solution
#pragma omp target exit data map(delete:this_ptr)
      delete[] h_array; 
  }
};

int main() {
  OMPVV_TEST_OFFLOADING;
  Simple mySimpleObj(10);
  int errors = 0;
  OMPVV_REPORT_AND_RETURN(errors);
}

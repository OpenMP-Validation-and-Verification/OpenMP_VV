#include <iostream>
#include <omp.h>

using namespace std;

#define N 1000

class A {

public:
  // breaks encapsulation
  int *h_array;
  int size;
  int sum;

  A(int *array, const int s) : h_array(array), size(s) { sum = 0; }

  // TODO: Add virtual once supported 
  ~A() { h_array = NULL; }
};

// Test for OpenMP 4.5 target data mapping objects in the heap
int test_map_tofrom_class_heap() {

  cout << "test_map_tofrom_class_heap" << endl;

  int sum = 0, errors = 0, isHost = 0;

  int *array = new int[N];
  A *obj = new A(array, N);

  // mapping an object + array: it is shallow copy thus 
  // pointers are not translated automatically
#pragma omp target data map(from: array[0:N]) map(tofrom: obj[0:1])
  {
#pragma omp target map(tofrom: isHost)
    {
      isHost = omp_is_initial_device();
      
      // assign device array ptr to device obj 
      int *tmp_h_array = obj->h_array;
      obj->h_array = array;
      int tmp = 0;
      for (int i = 0; i < N; ++i) {
        obj->h_array[i] = 1;
        tmp += 1;
      }
      // swap array device ptr to host ptr 
      obj->h_array = tmp_h_array;

      obj->sum = tmp;
    } // end target
  } // end target data

  // checking results 
  for (int i = 0; i < N; ++i)
    sum += obj->h_array[i];

  errors = (N != sum) || (N != obj->sum);
  if (!errors)
    cout << "Test passed on " << (isHost ? "host" : "device") << endl;
  else
    cout << "Test failed on " << (isHost ? "host" : "device") << ": sum=" << sum << ", N=" << N << endl;

  delete obj;
  delete[] array;

  return errors;
}

// Test for OpenMP 4.5 target data mapping objects on the stack
int test_map_tofrom_class_stack() {

  cout << "test_map_tofrom_class_stack" << endl;

  int sum = 0, errors = 0, isHost = 0;

  int array[N];
  A obj(array, N);

  // mapping an object + array: it is shallow copy thus 
  // pointers are not translated automatically
#pragma omp target data map(from: array[0:N]) map(tofrom: obj)
  {
#pragma omp target map(tofrom: isHost)
    {
      isHost = omp_is_initial_device();
      
      // assign device array ptr to device obj 
      int *tmp_h_array = obj.h_array;
      obj.h_array = array;
      int tmp = 0;
      for (int i = 0; i < N; ++i) {
        obj.h_array[i] = 1;
        tmp += 1;
      }
      // swap array device ptr to host ptr 
      obj.h_array = tmp_h_array;

      obj.sum = tmp;
    } // end target
  } // end target data

  // checking results 
  for (int i = 0; i < N; ++i)
    sum += obj.h_array[i];

  errors = (N != sum) || (N != obj.sum);
  if (!errors)
    cout << "Test passed on " << (isHost ? "host" : "device") << endl;
  else
    cout << "Test failed on " << (isHost ? "host" : "device") << ": sum=" << sum << ", N=" << N << endl;

  return errors;
}

int main() {

  int errors = 0;

  errors += test_map_tofrom_class_heap();
  errors += test_map_tofrom_class_stack();

  return errors;
}

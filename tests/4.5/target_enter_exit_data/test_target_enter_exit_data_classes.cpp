#include <iostream>
#include <omp.h>
#include <cassert>

using namespace std;

#define N 1000

template<typename T>
class Mapper {
private:
  T* ptr;
  bool not_mapped;
public:
  Mapper (T* p) : ptr(p) {
    not_mapped = !omp_target_is_present(ptr, omp_get_default_device());
    // maps if target is not present
#pragma omp target enter data map(to:ptr[0:1]) if(not_mapped)
  }
  ~Mapper() {
    // unmaps iff this mapper mapped the target
#pragma omp target exit data map(delete: ptr[0:1]) if(not_mapped)
    ptr = NULL;
  }
};

class B : public Mapper<B> {
private:
  int n;
  double* x;

public:
  B(int n) : Mapper<B>(this), n(n) {
    x = new double[n];
#pragma omp target update to(this->n)
#pragma omp target enter data map(to:x[0:n])
  }

  void modifyB(int* isHost, double* obj_x_ptr) {
#pragma omp target map(tofrom: isHost) map(from: obj_x_ptr)
    {
      *isHost = omp_is_initial_device();

      for (int i = 0; i < n; ++i)
        x[i] = 1.0;
      
      obj_x_ptr = x;
    } 
  }

  double* getX() {
    return x;
  }
};

class A : public Mapper<A>, public B {
private:
  int n;
  int* y;

public:
  A(int s) : Mapper<A>(this), B(s), n(s) { 
      y = new int[n];
  }

  void modifyA(int* isHost, double* obj_x_ptr, int* obj_y_ptr) {
#pragma omp target map(tofrom: isHost) map(from: obj_x_ptr) map(from: obj_y_ptr)
    {
      modifyB(isHost, obj_x_ptr);
      *isHost = omp_is_initial_device();

      for (int i = 0; i < n; ++i)
        y[i] = 1;
      
      obj_y_ptr = y;
    }
  }
  
  int* getY() {
    return y;
  }
  
  // TODO: Add virtual once supported
  //~A() { h_array = NULL; }
};

class Simple {
private:
  int *h_array;
  int size;
  int sum;

public:
  Simple(int s) : size(s) { 
      sum = 0;
      h_array = new int[size];
#pragma omp target enter data map(to:this[0:1])
#pragma omp target enter data map(to:h_array[0:size])
  }

  // TODO: Add virtual once supported
  ~Simple() { 
#pragma omp target exit data map(delete:h_array[0:size])
#pragma omp target exit data map(delete:this[0:1])
      delete[] h_array; 
  }
  
  void modify(int* isHost, int* obj_sum_ptr, int* obj_array_ptr) {
#pragma omp target map(tofrom: isHost) map(from: obj_sum_ptr) map(from: obj_array_ptr)
    {
      *isHost = omp_is_initial_device();

      sum = 0;
      for (int i = 0; i < size; ++i) {
        h_array[i] = 1;
        sum += h_array[i];
      }
      
      obj_sum_ptr = &sum; 
      obj_array_ptr = h_array;
    }
  }
  
  int* getArray() {
    return h_array;
  }
  
  int* getSum() {
    return &sum;
  }
};

int test_simple_class() {

  cout << "test_simple_class" << endl;

  int sum = 0, isHost = 0, errors = 0, h_sum = 0;
  int* h_array = new int[N];
  int* obj_array_ptr = NULL, *obj_sum_ptr = NULL;

  // allocation on the device
  Simple *obj = new Simple(N);

  obj->modify(&isHost, obj_sum_ptr, obj_array_ptr);

  assert(0 == omp_target_memcpy(h_array, obj_array_ptr, N*sizeof(int), 0, 0, omp_get_initial_device(), omp_get_default_device()));
  assert(0 == omp_target_memcpy(&h_sum, obj_sum_ptr, sizeof(int), 0, 0, omp_get_initial_device(), omp_get_default_device()));

  // checking results
  for (int i = 0; i < N; ++i)
    sum += h_array[i];

  delete obj;
  delete[] h_array;
  
  int res_cpy_array = omp_target_memcpy(h_array, obj_array_ptr, N*sizeof(int), 0, 0, omp_get_initial_device(), omp_get_default_device());
  int res_cpy_sum = omp_target_memcpy(&h_sum, obj_sum_ptr, sizeof(int), 0, 0, omp_get_initial_device(), omp_get_default_device());

  errors = (N != sum) || (N != h_sum) || (0 == res_cpy_sum) || (0 == res_cpy_array) ;
  if (!errors)
    cout << "Test passed on " << (isHost ? "host" : "device") << ": sum=" << sum << ", N=" << N << endl;
  else
    cout << "Test failed on " << (isHost ? "host" : "device") << ": sum=" << sum << ", N=" << N << endl;

  return errors;
}

int test_complex_class() {

  cout << "test_complex_class" << endl;

  int sumY = 0, isHost = 0, errors = 0;
  double sumX = 0.0;
  int *h_y = new int[N], *obj_y_ptr = NULL;
  double *h_x = new double[N], *obj_x_ptr = NULL;
  // allocation on the device
  A *obj = new A(N);

  obj->modifyA(&isHost, obj_x_ptr, obj_y_ptr);

  assert(0 == omp_target_memcpy(h_y, obj_y_ptr, N*sizeof(int), 0, 0, omp_get_initial_device(), omp_get_default_device()));
  assert(0 == omp_target_memcpy(h_x, obj_x_ptr, N*sizeof(double), 0, 0, omp_get_initial_device(), omp_get_default_device()));

  // checking results
  for (int i = 0; i < N; ++i) {
    sumY += h_y[i];
    sumX += h_x[i];
  }

  delete obj;
  delete[] h_x;
  delete[] h_y;
    
  int res_cpy_y = omp_target_memcpy(h_y, obj_y_ptr, N*sizeof(int), 0, 0, omp_get_initial_device(), omp_get_default_device());
  int res_cpy_x = omp_target_memcpy(h_x, obj_x_ptr, N*sizeof(double), 0, 0, omp_get_initial_device(), omp_get_default_device());

  errors = (N != sumY) || (N != sumX) || (0 == res_cpy_x) || (0 == res_cpy_y) ;
  if (!errors)
    cout << "Test passed on " << (isHost ? "host" : "device") << ": sumX=" << sumX << ", sumY=" << sumY << ", N=" << N << endl;
  else
    cout << "Test failed on " << (isHost ? "host" : "device") << ": sumX=" << sumX << ", sumY=" << sumY << ", N=" << N << endl;

  return errors;
}

int main() {

  int errors = 0;

  errors += test_simple_class();
  errors += test_complex_class();

  return errors;
}

#include <iostream>
#include <omp.h>

using namespace std;

#define N 1000

class A {

private:
  int h_array[N];
  int size;

public:
  A(const int s) : size(s) {}

  void modify(int* isHost) {
#pragma omp target map(tofrom:this->h_array) map(this->size) map(tofrom: isHost)     
    {
      *isHost = omp_is_initial_device();
      for (int i = 0; i < size; ++i)
          h_array[i] = 1;
    }
  }

  int* getArray() {
    return &h_array[0];
  }
};

int main() {

  cout << "test_explicit" << endl;

  int sum = 0, isHost = 0, errors = 0;

  A *obj = new A(N);

  obj->modify(&isHost);

  // checking results
  int* h_array = obj->getArray();
  for (int i = 0; i < N; ++i)
    sum += h_array[i];

  errors = N != sum;
  if (!errors)
    cout << "Test passed on " << (isHost ? "host" : "device") << ": sum=" << sum << ", N=" << N << endl;
  else
    cout << "Test failed on " << (isHost ? "host" : "device") << ": sum=" << sum << ", N=" << N << endl;

  delete obj;

  return errors;
}

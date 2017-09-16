#include <iostream>
#include <omp.h>

using namespace std;

#define N 1000

class B {
public:
  static double VAR;
  B() { 
  }
 
  static void modify(int* isHost, double* res) {
#pragma omp target map(tofrom: res)  map(tofrom:isHost)
    {
      *isHost = omp_is_initial_device();
      *res = B::VAR;
    } 
  }

  // TODO: Add virtual once supported
  ~B() { 
  }
};
double B::VAR = 1.0;

int main() {
  cout << "test_static" << endl;

  int isHost = 0, errors = 0;
  double exp = 1.0, res = 0.0;
  
  B *obj = new B();

  obj->modify(&isHost, &res);

  // checking results
  errors = res != exp;
  if (!errors)
    cout << "Test passed on " << (isHost ? "host" : "device") << ": exp=" << exp << ", res=" << res << endl;
  else
    cout << "Test failed on " << (isHost ? "host" : "device") << ": exp=" << exp << ", res=" << res << endl;

  delete obj;

  return errors;
}

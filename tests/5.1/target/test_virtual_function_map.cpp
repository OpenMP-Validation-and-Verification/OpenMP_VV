#include <iostream>
#include <omp.h>
#include "ompvv.h"

using namespace std;

class base {
public:
	virtual int test()
	{
		return 1;
	}

};

class derived : public base {
	public: 
		int test()
		{
			return 2;
		}

};


#pragma declare target to(test)
int main(){
	int errors = 0;
	base *bptr;
	derived d;
	bptr = &d;
	OMPVV_TEST_OFFLOADING;
	OMPVV_TEST_AND_SET_VERBOSE(errors, bptr->test() != 2);
	OMPVV_REPORT_AND_RETURN(errors)

	return 0;
}

//-----------test_map_virtual_function.cpp------------------
//
// OpenMP API Version 5.1 Mar 2023
//
//This test is designed to ensure that virtual functions 
//are correctly passed into a target region. The class base
//should use the derived classes test function instead of 
//the virtual one declared in the base class
//----------------------------------------------------------


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

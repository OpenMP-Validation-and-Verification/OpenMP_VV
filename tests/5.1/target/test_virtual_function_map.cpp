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

#pragma omp begin declare target
class base {
public:
	virtual int test()
	{
		return 1;
	}

};
#pragma omp end declare target

#pragma omp begin declare target
class derived : public base {
	public: 
		int test()
		{
			return 2;
		}

};
#pragma omp end declare target

int test_case(){
    base *bptr;
    derived d;
    bptr = &d;
    int test_val = 0;

    #pragma omp target map(toform: test_val)
    {
	test_val = bptr->test();
    }
	return test_val;
}

int main(){
	int errors = 0;
	OMPVV_TEST_OFFLOADING;
	OMPVV_TEST_AND_SET_VERBOSE(errors, test_case() != 2);
	OMPVV_REPORT_AND_RETURN(errors)
	return errors;
}

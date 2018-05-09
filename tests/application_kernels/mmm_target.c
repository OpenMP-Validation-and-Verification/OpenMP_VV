/******************************************************************************
* DESCRIPTION:  
*   OpenMp Example - Matrix Multiply - C Version
*   Demonstrates a matrix multiply using OpenMP. Threads share row iterations
******************************************************************************/

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

#define rowA 500        
#define colA 500        
#define colB 500        

struct Time{

    struct timeval tmStart;

    struct timeval tmEnd;
};

void startTime(struct Time * time){
    gettimeofday(&time->tmStart, NULL);
}

void endTime(struct Time * time){ //Report time in seconds
    gettimeofday(&time->tmEnd, NULL);
    unsigned long long seconds =(time->tmEnd.tv_sec - time->tmStart.tv_sec) ;
    unsigned long long milliseconds = (time->tmEnd.tv_usec - time->tmStart.tv_usec) / 1000;

    unsigned long long totalMilliseconds =1000*seconds + milliseconds;
    int totalSeconds =(int)totalMilliseconds/1000;

    printf("Total time for A[%d][%d] X B[%d][%d] on device using target directive only:%d \n",rowA,colA,colA,colB,totalSeconds);

}


int main (int argc, char *argv[]) 
{
  int tid, nthreads, i, j, k;
  int	*a = (int*)malloc(sizeof(int) * rowA * colA);           /* matrix A to be multiplied */
  int	*b = (int*)malloc(sizeof(int) * colA * colB);           /* matrix B to be multiplied */
  int	*c = (int*)malloc(sizeof(int) * rowA * colB);           /* result matrix C */
  
  
    /*** Initialize matrices ***/
    for (i=0; i<rowA; i++)
      for (j=0; j<colA; j++)
        a[i*rowA+j]= 10; // i+j;
    for (i=0; i<colA; i++)
      for (j=0; j<colB; j++)
        b[i*colA+j]= 50; //i*j;
    for (i=0; i<rowA; i++)
      for (j=0; j<colB; j++){
        c[i*rowA+j]= 0;
      }
   
  
  int DimA=rowA*colA;
  int DimB=colB*colA;
  int DimC=rowA*colA;
  struct Time * time= (struct Time *)malloc(sizeof(struct Time));
  startTime(time);
  
#pragma omp target map(to: a[0:DimA],b[0:DimB]) map(from: c[0:DimC])
{
    for (i=0; i<rowA; i++)
      for(j=0; j<colB; j++)
        for(k=0; k<colA; k++)
          c[i*rowA+j] = a[i*rowA+j] * b[k*colA+j];
}//end-target
  
  endTime(time);
  
  /*** Print results ***/
  int error=0;
  for (i=0; i<rowA; i++)
  {
    for (j=0; j<colB; j++)
      if( 500 != c[i*rowA+j]){
        printf("Error: [%d][%d] should be 500 is %d\n",i,j,c[i*rowA+j]);
        error += error;
     }
   
  }
  if(error)
    printf("Test NOT PASSED\n");
  else
    printf ("Test PASSED.\n");
  
}


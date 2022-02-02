 #include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main() {
int scalar_var = 6;
    #pragma omp taskloop default(firstprivate)
        for (int i = 0; i <1; i++) {
            scalar_var += 7;
        }
printf("Scalar_var is: %d \n", scalar_var);
if (scalar_var == 6)
    printf("Test passed first private.");
else 
    printf("Test did not pass first private.");
return 0;
}

#ifndef __OMPVV_TIMING__
#define __OMPVV_TIMING__

#include "ompvv.h"
#include <sys/time.h>
#include <stdint.h>

#define OMPVV_GET_TIME(timer) \
{ \
  struct timeval time;\
  gettimeofday(&time, NULL);\
  timer = (uint64_t)time.tv_sec*1e6 + (uint64_t)time.tv_usec; \
}

#define OMPVV_PRINT_TIME_LAPSED(start, stop) \
{ \
  OMPVV_INFOMSG("Time(us) = %ju", stop - start)\
}
#endif

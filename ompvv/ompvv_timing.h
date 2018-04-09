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

// For the experiments
#define NUM_REP 1002
#define OMPVV_INIT_TIMERS \
  uint64_t _ompvv_start, _ompvv_stop, _ompvv_max, _ompvv_min, _ompvv_partial, _ompvv_accum, _ompvv_aux;

#define OMPVV_TIMING_LOAD \
{ \
  uint64_t volatile __ompvv_b = 0;\
    __ompvv_b++; \
}

#define OMPVV_START_TIMER OMPVV_GET_TIME(_ompvv_start)
#define OMPVV_STOP_TIMER OMPVV_GET_TIME(_ompvv_stop)
#define OMPVV_GET_TIME_LAPSED (_ompvv_stop - _ompvv_start)

#define OMPVV_INIT_TEST \
  _ompvv_accum = 0; \
  _ompvv_max = 0; \
  _ompvv_min = 0xFFFFFFFFFFFFFFFF;

#define OMPVV_REGISTER_TEST \
  _ompvv_partial = OMPVV_GET_TIME_LAPSED; \
  _ompvv_aux = 0; \
  if (_ompvv_partial > _ompvv_max) {\
    _ompvv_accum += _ompvv_max; \
    _ompvv_max = _ompvv_partial; \
    _ompvv_aux = 1; \
  } \
  if (_ompvv_partial < _ompvv_min) { \
    _ompvv_accum += _ompvv_min; \
    _ompvv_min = _ompvv_partial; \
    _ompvv_aux = 1; \
  } \
  if (_ompvv_aux == 0) \
    _ompvv_accum += _ompvv_partial;\

#define OMPVV_TIMER_RESULT(clause) \
  OMPVV_INFOMSG("["clause"] AVG_TIME = %f us, MAX_TIME = %ju us, MIN_TIME = %ju us", ((double)_ompvv_accum) / NUM_REP, _ompvv_max, _ompvv_min);

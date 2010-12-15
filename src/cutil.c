
#include "cutil.h"
#include <sys/time.h>
#include <stdio.h>

int64_t timeofday_usecs ()
{
  struct timeval tv;
  struct timezone tz;
  gettimeofday(&tv, &tz); // todo: check for errors
  return tv.tv_sec * 1000000 + tv.tv_usec;
}

// compile with: gcc -o ex1 -Wall ex1.c
#include "stdio.h"
#include "sys/time.h"
#include "time.h"

int main (const int argc, const char ** argv ) {
  time_t curr_time;
  char buff[1024];
  // time(&curr_time);
  curr_time = 1359684105; // Thu Jan 31 2013
  struct tm *now = localtime(&curr_time);
  strftime(buff, sizeof(buff),  "%a %b %d %Y", now);
  printf("time: %ld\n", curr_time);
  printf("time: %s\n", buff);

  curr_time += 24 * 60 * 60; // Fri Feb 01 2013
  now = localtime(&curr_time);
  strftime(buff, sizeof(buff),  "%a %b %d %Y", now);
  printf("time: %ld\n", curr_time);
  printf("time: %s\n", buff);
  return 0;
}

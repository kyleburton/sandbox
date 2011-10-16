#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <unistd.h>
#include <time.h>
#include <fcntl.h>


char* int_to_binary_ascii( int x ) {
  static char buff[8*sizeof(int) + 1];
  unsigned int ii, num_bits = (8 * sizeof(unsigned int));
  memset(buff,'\0',sizeof(buff));
  for ( ii = 0; ii < num_bits; ii++ ) {
    buff[num_bits - ii - 1] = ( x & (1 << ii ) ) ? '1' : '0';
  }
  return buff;
}

void clear_line () {
  printf("%c[2K\r", 27);
}

static useconds_t delay = 100;
int keep_running = 1;

useconds_t set_delay(long new) {
  if ( (delay + new) > 1000000 || (delay+new)<0) {
    return delay;
  }
  delay += new;
  return delay;
}

void adjust () {
  static char input[16] = {0};
  memset(input,'\0',sizeof(input)*sizeof(char));
  int res = read(0, &input, sizeof(input)*sizeof(char));

  if ( -1 == res ) {
    return;
  }

  printf("\r\ninput:'%s'\r\n",input);

  switch(input[0]) {
    // escape: 1'st part of a cursor/arrow key
    case 27:
      switch(input[1]) {
        // '[' 2'nd part of an arrow key
        case 91:
          switch(input[2]) {
            // 3'rd part of an arrorw key, A[^] B[v] C[>] or D[<]
            case 65: set_delay( 10000);  printf("\r\n10k   : %u\r\n", delay); break;
            case 66: set_delay(-10000);  printf("\r\n10k   : %u\r\n", delay); break;
            case 67: set_delay(-100000); printf("\r\n100k  : %u\r\n", delay); break;
            case 68: set_delay( 100000); printf("\r\n100k  : %u\r\n", delay); break;
          }
          break;
      }
      break;
    case 's': set_delay(1);     printf("\r\nONE:    %u\r\n", delay); break;
    case 'S': set_delay(-1);    printf("\r\nONE:    %u\r\n", delay); break;

    case 't': set_delay(10);    printf("\r\nTEN:    %u\r\n", delay); break;
    case 'T': set_delay(-10);   printf("\r\nTEN:    %u\r\n", delay); break;

    case 'h': set_delay(100);   printf("\r\nHUNDR:  %u\r\n", delay); break;
    case 'H': set_delay(-100);  printf("\r\nHUNDR:  %u\r\n", delay); break;

    case 'k': set_delay(1000);  printf("\r\nTHOUS:  %u\r\n", delay); break;
    case 'K': set_delay(-1000); printf("\r\nTHOUS:  %u\r\n", delay); break;

    case '2': delay *= 2;    printf("\r\nDOUBLE: %u\r\n", delay); break;
    case '1': delay /= 2;    printf("\r\nHALF  : %u\r\n", delay); break;
    case 'z': delay  = 0;    printf("\r\nZERO  : %u\r\n", delay); break;
    case 'q':
              keep_running = 0;
              break;
  }
  fflush(stdout);
}

static long x,t;

void bit_pattern_1 () {
  x = (t * ( t>>11 & t>>8 & 123 & t>>3));
}

int main ( int argc, char** argv ) {
  time_t started_at = time(NULL) - 1;
  float rate = 0.0;
  int res = 0;
  int flags;

  printf("started_at = %ld\r\n", started_at);

  flags = fcntl(0,F_GETFL,0);
  flags |= O_NONBLOCK;
  res = fcntl(0,F_SETFL,flags);
  printf("set stdin(0) to nonblock, res=%d\r\n",res);

  // TODO: use the usleep / rate to hit a target cycles per second rather than
  // just a static adjustment, proably shooting for 20k-40k/s
  for (t = 0 ;; ++t ) {
    bit_pattern_1();
    rate = t / (time(NULL) - started_at);
    clear_line(); 
    printf ("% 12ld: %s %12lu  %3.2f/s", t, int_to_binary_ascii(x), x, rate);
    fflush(stdout);
    adjust();
    if ( ! keep_running ) {
      break;
    }
    usleep(delay);
  }
  return 0;
}

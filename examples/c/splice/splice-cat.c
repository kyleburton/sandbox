// https://gist.github.com/karthick18/1234187
// https://lwn.net/Articles/178199/
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

int copy_via_splice(int fdin, int fdout, off_t len) {
  ssize_t num_read = 0;
  ssize_t num_written = 0;
  loff_t in_off = 0;
  loff_t out_off = 0;
  int pipefds[2];

  if (-1 == pipe(pipefds)) {
    printf("pipe error! error=%d / %s\n", errno, strerror(errno));
    return -1;
  }

  off_t left_to_read = len;
  off_t left_to_write = 0;

  // NB: I'm not sure if tracking the read vs written separately is strictly
  // necessary it's there in case splice/2 happens to be asymmetrical betwween
  // the number of bytes read vs the number written (eg the write is less than
  // the read).  I'm also unsure if it's valid for both of the splice/2 calls
  // to process zero bytes during a given iteration of the loop when neither of
  // the file are in non blocking mode.  Though I suppose I could check for -1
  // and EAGAIN ... that might make copy_via_splice more robust against the fds
  // being in blocking or non-blocking mode.

  while (left_to_read > 0 || left_to_read > 0) {
    int either_read_or_wrote = 0;

    if (left_to_read > 0) {
      num_read = splice(fdin, &in_off, pipefds[1], NULL, len, 0);
      if (-1 == num_read) {
        printf("splice error! error=%d / %s\n", errno, strerror(errno));
        return -1;
      }

      left_to_write += num_read;
      left_to_read -= num_read;
      either_read_or_wrote |= 1;
    }

    if (left_to_write > 0) {
      num_written = splice(pipefds[0], NULL, fdout, &out_off, left_to_write, 0);
      if (-1 == num_written) {
        printf("splice error! error=%d / %s\n", errno, strerror(errno));
        return -1;
      }

      if (left_to_write != num_written) {
        printf("NB: pipe still needs to be drained: left_to_write=%ld != num_written=%ld\n", left_to_write, num_written);
      }

      left_to_write -= num_written;
      either_read_or_wrote |= 2;
    }

    if (!either_read_or_wrote) {
      printf("Error: looks like there was data left to read (left_to_read=%ld) or write (left_to_write=%ld) though nothing was read or written in the loop\n", left_to_read, left_to_write);
      return -1;
    }
  }

  return len;
}

int main (int argc, char **argv) {
  if (argc < 3) {
    printf("Error: you have to specify a from file and a to file: %s <from> <to>\n", argv[0]);
    exit(1);
  }
  char* from = argv[1];
  char* to = argv[2];

  int fdin = open(from, 0, O_RDONLY);
  if (fdin == -1) {
    printf("Error opening from file[%s] error=%d / %s\n", from, errno, strerror(errno));
    exit(errno);
  }

  int fdout = open(to, O_CREAT | O_WRONLY, 0644);
  if (fdout == -1) {
    printf("Error opening to file[%s] error=%d / %s\n", to, errno, strerror(errno));
    exit(errno);
  }

  int res = 0;

  struct stat inf_stat;
  bzero(&inf_stat, sizeof(struct stat));
  res = fstat(fdin, &inf_stat);
  if (res == -1) {
    printf("Error stating the from file[%s] error=%d / %s\n", from, errno, strerror(errno));
    exit(errno);
  }

  printf("splicing %ld bytes from=%s to=%s\n", inf_stat.st_size, from, to);
  // res = splice(fdin, NULL, fdout, NULL, inf_stat.st_size, 0);
  res = copy_via_splice(fdin, fdout, inf_stat.st_size);
  printf("splice returned: res=%d\n", res);

  if (-1 == res) {
    printf("splice error! error=%d / %s\n", errno, strerror(errno));
    exit(errno);
  }

  if (-1 == close(fdin)) {
    printf("Error closing the fromfile [%s] error=%d / %s\n", from, errno, strerror(errno));
    exit(errno);
  }

  if (-1 == close(fdout)) {
    printf("Error closing the tofile [%s] error=%d / %s\n", from, errno, strerror(errno));
    exit(errno);
  }


  return 0;
}

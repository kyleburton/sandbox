// https://gist.github.com/karthick18/1234187
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
  while (left_to_read > 0) {
    num_read = splice(fdin, &in_off, pipefds[1], NULL, len, 0);
    if (-1 == num_read) {
      printf("splice error! error=%d / %s\n", errno, strerror(errno));
      return -1;
    }

    // NB: can the write write less than the read?  If that's the case then
    // we'd need to accunt for that with a loop to drain the pipe to the
    // output.
    num_written = splice(pipefds[0], NULL, fdout, &out_off, num_read, 0);
    if (-1 == num_written) {
      printf("splice error! error=%d / %s\n", errno, strerror(errno));
      return -1;
    }

    if (num_read != num_written) {
      printf("splice error! num_read=%ld != num_written=%ld\n", num_read, num_written);
      return -1;
    }

    left_to_read -= num_read;
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

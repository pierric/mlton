#ifndef _XOPEN_SOURCE_EXTENDED
#define _XOPEN_SOURCE_EXTENDED 1
#endif

#include <fenv.h>
#include <inttypes.h>

#include <unistd.h>

#include <dirent.h>
#include <fcntl.h>
#include <grp.h>
#include <math.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <poll.h>
#include <signal.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/times.h>
#include <sys/un.h>
#include <sys/utsname.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <syslog.h>
#include <termios.h>
#include <utime.h>
#include <sys/types.h>
#include <grp.h>
#include <pwd.h>

#include "setenv.h"

#define HAS_FEROUND TRUE
#define HAS_MSG_DONTWAIT FALSE
#define HAS_REMAP FALSE
#define HAS_SIGALTSTACK TRUE
#define HAS_SPAWN TRUEE
#define HAS_TIME_PROFILING TRUE
#define MLton_Platform_OS_host "openmvs"
extern char **environ;

#define NSIG 40
/* This should not conflict with existing flags. */
#define MSG_DONTWAIT 0x1000000


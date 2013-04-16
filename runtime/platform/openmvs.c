#include "platform.h"

#include "diskBack.openmvs.c"
#include "mmap-protect.c"
#include "nonwin.c"
#include "use-mmap.c"

static void catcher (__attribute__ ((unused)) int signo,
                     __attribute__ ((unused)) siginfo_t* info,
                     void* context) {
        ucontext_t* ucp = (ucontext_t*)context;
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext.psw.addr);
}

void GC_setSigProfHandler (struct sigaction *sa) {
        sa->sa_flags = SA_ONSTACK | SA_RESTART | SA_SIGINFO;
        sa->sa_sigaction = (void (*)(int, siginfo_t*, void*))catcher;
}

size_t GC_pageSize (void) {
        long int pageSize;

        pageSize = sysconf (_SC_PAGESIZE);
        if (pageSize < 0)
                diee ("GC_pageSize error: sysconf (_SC_PAGESIZE) failed");

        return (size_t)pageSize;
}

uintmax_t GC_physMem (void) {
        return (uintmax_t)SIZE_MAX;
}

void GC_displayMem (void) {
}

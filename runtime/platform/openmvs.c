#include "platform.h"

#include "diskBack.openmvs.c"
#include "nonwin.c"

#ifndef __XPLINK__
#define RESUME(mcontext) ((int*)&(mcontext))[14]
#else
#define RESUME(mcontext) ((int*)((char*)&(mcontext))+80)[7]
#endif

static void catcher (__attribute__ ((unused)) int signo,
        __attribute__ ((unused)) siginfo_t* info,
        void* context) {
    ucontext_t* ucp = (ucontext_t*)context;
    GC_handleSigProf ((code_pointer) RESUME(ucp->uc_mcontext));
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
    struct rlimit rlim;
    getrlimit(RLIMIT_AS, &rlim);
    return (uintmax_t)rlim.rlim_max;
}

void GC_displayMem (void) {
}


void *GC_mmapAnon (void *start, size_t length) {
    void *ptr = malloc(length);
    if (NULL == ptr) {
        return (void*) -1;
    }
    return ptr;
}
void GC_release (void *base, size_t length) {
    assert (base != NULL);
    if (0 == length)
        return;
    free (base);
}
void *GC_mmapAnon_safe (void *p, size_t length) {
    void *result;

    result = GC_mmapAnon (p, length);
    if ((void *)-1 == result) {
        die ("Out of memory.  Unable to allocate %s bytes.\n",
                uintmaxToCommaString(length));
    }
    return result;
}
void *GC_mmapAnon_safe_protect (void *start, size_t length,
        size_t dead_low, size_t dead_high) {
    return GC_mmapAnon_safe (start, length);
}


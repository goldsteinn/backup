#include <errno.h>
#include <stdint.h>

#include <asm/unistd.h>
#include <linux/perf_event.h>

#include <asm/unistd.h>
#include <assert.h>
#include <fcntl.h>
#include <linux/perf_event.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <unistd.h>

// echo 2 > /sys/devices/cpu/rdpmc

static long
perf_event_open(struct perf_event_attr * hw_event,
                pid_t                    pid,
                int                      cpu,
                int                      group_fd,
                unsigned long            flags) {
    int ret;

    ret = syscall(__NR_perf_event_open, hw_event, pid, cpu, group_fd, flags);
    return ret;
}

// fixed events
#define NEVENTS 7
static constexpr uint32_t perf_ev_codes[NEVENTS] = {
    /* fixed */
    0x3c,   // instructions
    0xc0,   // true cycles
    0x300,  // ref cycles
    /* prg */
    0x01a1,  // p0 uops
    0x02a1,  // p1 uops
    0x20a1,  // p5 uops
    0x40a1   // p6 uops
};

static constexpr uint32_t perf_bias[NEVENTS] = { 0 };

/* fixed events */
static constexpr uint32_t ev_codes0 = (1 << 30);
static constexpr uint32_t ev_codes1 = (1 << 30) + 1;
static constexpr uint32_t ev_codes2 = (1 << 30) + 2;

/* prg events */
static constexpr uint32_t ev_codes3 = 0x0;
static constexpr uint32_t ev_codes4 = 0x1;
static constexpr uint32_t ev_codes5 = 0x2;
static constexpr uint32_t ev_codes6 = 0x3;


void
perf_counters() {
    struct perf_event_attr pe[NEVENTS];
    long long              count;
    int                    fd;

    __builtin_memset(pe, 0, sizeof(pe));

    int group_fd = -1;
    for (uint32_t i = 0; i < NEVENTS; ++i) {
        pe[i].type           = PERF_TYPE_RAW;
        pe[i].size           = sizeof(struct perf_event_attr);
        pe[i].config         = perf_ev_codes[i];
        pe[i].disabled       = (group_fd == (-1)) ? 1 : 0;
        pe[i].exclude_kernel = 1;
        pe[i].exclude_hv     = 1;

        fd = perf_event_open(pe + i, 0, -1, group_fd, 0);
        if (fd == -1) {
            fprintf(stderr, "Error opening leader %llx\n", pe[i].config);
            fprintf(stderr, "%d: %s\n", errno, strerror(errno));
            exit(EXIT_FAILURE);
        }
        if (group_fd == (-1)) {
            group_fd = fd;
        }
    }
    ioctl(group_fd, PERF_EVENT_IOC_RESET, 0);
    ioctl(group_fd, PERF_EVENT_IOC_ENABLE, 0);

    uint8_t  buf[32];
    uint32_t starts[NEVENTS] __attribute__((aligned(32))) = { 0 };
    uint32_t ends[NEVENTS] __attribute__((aligned(32)))   = { 0 };
    starts[0]                                             = 0;
    ends[0]                                               = 0;

    uint64_t ret = 0, tmp = 0, tmp2 = 0, cnt = 0;
    asm volatile(
        "movl %[ev_code6], %%ecx\n\t"
        "rdpmc\n\t"
        "movl %%eax, 24(%[starts])\n\t"
        "movl %[ev_code5], %%ecx\n\t"
        "rdpmc\n\t"
        "movl %%eax, 20(%[starts])\n\t"
        "movl %[ev_code4], %%ecx\n\t"
        "rdpmc\n\t"
        "movl %%eax, 16(%[starts])\n\t"
        "movl %[ev_code3], %%ecx\n\t"
        "rdpmc\n\t"
        "movl %%eax, 12(%[starts])\n\t"
        "movl %[ev_code2], %%ecx\n\t"
        "rdpmc\n\t"
        "movl %%eax, 8(%[starts])\n\t"
        "movl %[ev_code1], %%ecx\n\t"
        "rdpmc\n\t"
        "movl %%eax, 4(%[starts])\n\t"
        "movl %[ev_code0], %%ecx\n\t"
        "rdpmc\n\t"
        "movl %%eax, 0(%[starts])\n\t"

        /* critical section start */
        "lfence\n\t"
        "movl $1000000, %%edx\n\t"
        "movq %[ends], %%rdi\n\t"
        "xorl %%ecx, %%ecx\n\t"
        ".p2align 4\n\t"
        "1:\n\t"
        "decl %%edx\n\t"
        "movl 8(%%rdi, %%rcx), %%esi\n\t"
        "nop\n\t"
        "nop\n\t"
        "testl %%edx, %%edx\n\t"
        "jnz 1b\n\t"
        "lfence\n\t"
        /* critical section end */


        "movl %[ev_code0], %%ecx\n\t"
        "rdpmc\n\t"
        "movl %%eax, 0(%[ends])\n\t"
        "movl %[ev_code1], %%ecx\n\t"
        "rdpmc\n\t"
        "movl %%eax, 4(%[ends])\n\t"
        "movl %[ev_code2], %%ecx\n\t"
        "rdpmc\n\t"
        "movl %%eax, 8(%[ends])\n\t"

        "movl %[ev_code3], %%ecx\n\t"
        "rdpmc\n\t"
        "movl %%eax, 12(%[ends])\n\t"
        "movl %[ev_code4], %%ecx\n\t"
        "rdpmc\n\t"
        "movl %%eax, 16(%[ends])\n\t"
        "movl %[ev_code5], %%ecx\n\t"
        "rdpmc\n\t"
        "movl %%eax, 20(%[ends])\n\t"
        "movl %[ev_code6], %%ecx\n\t"
        "rdpmc\n\t"
        "movl %%eax, 24(%[ends])\n\t"
        : [ starts_clobber ] "=m"((*(uint32_t(*)[7])starts)),
          [ ends_clobber ] "=m"((*(uint32_t(*)[7])ends)), [ tmp ] "+r"(tmp),
          [ tmp2 ] "+r"(tmp2), [ ret ] "+r"(ret), [ cnt ] "+r"(cnt)
        : [ ev_code0 ] "i"(ev_codes0), [ ev_code1 ] "i"(ev_codes1),
          [ ev_code2 ] "i"(ev_codes2), [ ev_code3 ] "i"(ev_codes3),
          [ ev_code4 ] "i"(ev_codes4), [ ev_code5 ] "i"(ev_codes5),
          [ ev_code6 ] "i"(ev_codes6), [ starts ] "g"(starts),
          [ ends ] "g"(ends), [ buf ] "g"(buf)
        : "rax", "rdx", "rcx");

    for (uint32_t i = 0; i < NEVENTS; ++i) {
        fprintf(stderr, "(%10u - %10u), %10u\n", ends[i], starts[i],
                (ends[i] - starts[i]) - perf_bias[i]);
    }
}

#include <pthread.h>
#include <sched.h>
void
pin_self(int cpu) {
    assert(cpu >= 0 && cpu < sysconf(_SC_NPROCESSORS_ONLN));
    cpu_set_t cset;
    CPU_ZERO(&cset);
    CPU_SET(cpu, &cset);
    assert(sched_setaffinity(0, sizeof(cpu_set_t), &cset) == 0);

    int failure_count = 10;
    while (sched_getcpu() != cpu && (--failure_count)) {
        sched_yield();
    }
    assert(sched_getcpu() == cpu);
}

double
flops() {
    double d0 = 1.1;
    double d1 = 1.1;
    double d2 = 1.1;
    double d3 = 1.1;

    for (int j = 0; j < 10; ++j) {
        for (int i = 0; i < 10000000; ++i) {
            d0 *= d0;
            d1 *= d1;
            d2 *= d2;
            d3 *= d3;
            d0 *= d0;
            d1 *= d1;
            d2 *= d2;
            d3 *= d3;
            d0 *= d0;
            d1 *= d1;
            d2 *= d2;
            d3 *= d3;
            d0 *= d0;
            d1 *= d1;
            d2 *= d2;
            d3 *= d3;
            d0 *= d0;
            d1 *= d1;
            d2 *= d2;
            d3 *= d3;
            d0 *= d0;
            d1 *= d1;
            d2 *= d2;
            d3 *= d3;
            d0 *= d0;
            d1 *= d1;
            d2 *= d2;
            d3 *= d3;
            d0 *= d0;
            d1 *= d1;
            d2 *= d2;
            d3 *= d3;
        }
    }

    return d0 * d1 * d2 * d3;
}

void *
msr_counter(void * arg) {
    int                cpu                     = sched_getcpu();
    constexpr uint64_t CORE_PERF_LIMIT_ICELAKE = 0x64f;
    constexpr uint64_t CORE_PERF_LIMIT_SKYLAKE = 0x690;
    constexpr uint64_t THERM_STATUS            = 0x19c;
    uint64_t           off                     = CORE_PERF_LIMIT_ICELAKE;
    uint64_t           data;
    char               buf[32] = { 0 };
    sprintf(buf, "/dev/cpu/%d/msr", cpu);
    int msr_fd = open(buf, O_RDONLY);
    assert(msr_fd >= 0);

    volatile double sink = flops();
    assert(pread(msr_fd, &data, 8, off) == 8);

    fprintf(stderr, "Data: %016lx\n", data);
    return NULL;
}

#define NTHREADS 128
int
main(int argc, char ** argv) {
    perf_counters();
}

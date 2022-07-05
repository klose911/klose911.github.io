#define _GNU_SOURCE         /* See feature_test_macros(7) */

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/syscall.h>


const int NUM_THREADS = 5;


void *thread_main(void *threadid)
{

        /* 把自己加入cgroup中（syscall(SYS_gettid)为得到线程的系统tid） */
        char cmd[128];
        sprintf(cmd, "echo %ld >> /sys/fs/cgroup/cpu/klose/tasks", syscall(SYS_gettid));
        system(cmd);
        sprintf(cmd, "echo %ld >> /sys/fs/cgroup/cpuset/klose/tasks", syscall(SYS_gettid));
        system(cmd);
        
        long tid;
        tid = (long)threadid;

        printf("Hello World! It's me, thread #%ld, pid #%ld!\n", tid, syscall(SYS_gettid));


        int a=0;
        
        while(1) {
                a++;
        }

        pthread_exit(NULL);

}

int main (int argc, char *argv[])
{

        int num_threads;

        if (argc > 1){
                num_threads = atoi(argv[1]);
        }

        if (num_threads<=0 || num_threads>=100){
                num_threads = NUM_THREADS;
        }
        
        /* 设置CPU利用率为50% */
        mkdir("/sys/fs/cgroup/cpu/klose", 755);
        system("echo 50000 > /sys/fs/cgroup/cpu/klose/cpu.cfs_quota_us");
        mkdir("/sys/fs/cgroup/cpuset/klose", 755);

        /* 限制CPU只能使用#2核和#3核 */
        system("echo \"2,3\" > /sys/fs/cgroup/cpuset/klose/cpuset.cpus");
        
        pthread_t* threads = (pthread_t*) malloc (sizeof(pthread_t)*num_threads);
        int rc;
        long t;

        for(t=0; t<num_threads; t++){

                printf("In main: creating thread %ld\n", t);
                rc = pthread_create(&threads[t], NULL, thread_main, (void *)t);
                if (rc){
                        printf("ERROR; return code from pthread_create() is %d\n", rc);
                        exit(-1);
                }

        }


        /* Last thing that main() should do */
        pthread_exit(NULL);
        free(threads);

}

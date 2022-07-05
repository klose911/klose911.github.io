#define _GNU_SOURCE
#include <sys/types.h>
#include <sys/wait.h>
#include <stdio.h>
#include <sched.h>
#include <signal.h>
#include <unistd.h>
 
/* ����һ���� clone �õ�ջ��ջ��С1M */
#define STACK_SIZE (1024 * 1024)
static char container_stack[STACK_SIZE];
 
char* const container_args[] = {
        "/bin/bash",
        NULL
};
 
int container_main(void* arg)
{
        printf("Container - inside the container!\n");
        /* ֱ��ִ��һ��shell���Ա����ǹ۲�������̿ռ������Դ�Ƿ񱻸����� */
        execv(container_args[0], container_args); 
        printf("Something's wrong!\n");
        return 1;
}
 
int main()
{
        printf("Parent - start a container!\n");
        /* ����clone���������д���һ������������һ��ջ�ռ�ģ�Ϊʲô��βָ�룬��Ϊջ�Ƿ��ŵģ� */
        int container_pid = clone(container_main, container_stack+STACK_SIZE, SIGCHLD, NULL);
        /* �ȴ��ӽ��̽��� */
        waitpid(container_pid, NULL, 0);
        printf("Parent - container stopped!\n");
        return 0;
}

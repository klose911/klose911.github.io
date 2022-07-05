/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.executor;

import java.util.concurrent.Executor;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.Semaphore;

public class BoundedExecutor {

    private final Executor exec;
    private final Semaphore semaphore;

    public BoundedExecutor(Executor exec, int bound) {
        this.exec = exec;
        // 设定信号量permit的上限
        this.semaphore = new Semaphore(bound);
    }

    public void submitTask(final Runnable command) throws InterruptedException {
        // 提交task前先申请permit, 如果无法申请到permit, 调用submitTask的线程将被阻塞, 直到有permit可用
        semaphore.acquire();
        try {
            exec.execute(() -> {
                try {
                    command.run();
                } finally {
                    // 提交成功了, 运行task后释放permit
                    semaphore.release();
                }
            });
        } catch (RejectedExecutionException e) {
            // 如果没有提交成功, 也需要释放permit
            semaphore.release();
        }
    }
}

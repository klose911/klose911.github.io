/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.cancel.timeRun;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import static org.klose.concurrency.synchronizer.Preloader.launderThrowable;

/**
 *
 * @author klose
 */
public class TimerRun {

    private static final ScheduledExecutorService CANCEL_EXEC = Executors.newSingleThreadScheduledExecutor();
    
    public static void timedRun(final Runnable r,
            long timeout, TimeUnit unit) throws InterruptedException {

        class ReThrowableTask implements Runnable {
            //在当前线程和taskThread线程共享异常
            private volatile Throwable throwable;

            @Override
            public void run() {
                try {
                    r.run();
                } catch (Throwable t) {
                    throwable = t;
                }
            }

            void rethrow() {
                if (throwable != null) {
                    throw launderThrowable(throwable);
                }
            }
        }

        ReThrowableTask task = new ReThrowableTask();
        final Thread taskThread = new Thread(task);
        taskThread.start();

        CANCEL_EXEC.schedule(() -> {
            taskThread.interrupt();
        }, timeout, unit);

        // 停止当前进程，让taskThread运行限时时间
        // 如果超过限时，则让cancelExec线程池的线程对taskThread发起中断请求
        taskThread.join(unit.toMillis(timeout));

        //如果taskThread线程内捕获异常，重新抛出
        task.rethrow();
    }
}

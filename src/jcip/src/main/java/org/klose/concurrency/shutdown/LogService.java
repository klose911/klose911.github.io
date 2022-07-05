/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.shutdown;

import java.io.PrintWriter;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.TimeUnit;


public class LogService {

    private final ExecutorService exec = Executors.newSingleThreadExecutor();
    private final PrintWriter writer;

    public LogService(PrintWriter writer) {
        this.writer = writer;
    }

    public void shutdown() throws InterruptedException {
        final long TIMEOUT = 10L; 
        try {
            // 关闭ExecutorService后再调用其awaitTermination将导致当前线程阻塞, 直到所有已提交的任务执行完毕, 或者发生超时  
            exec.shutdown();
            exec.awaitTermination(TIMEOUT, TimeUnit.SECONDS);
        } finally {
            writer.close();
        }
    }

    public void log(String msg) {
        try {
            // 线程池关闭后再调用其execute方法将抛出RejectedExecutionException异常  
            exec.execute(new WriteTask(msg));
        } catch (RejectedExecutionException ignored) {
        }
    }

    private final class WriteTask implements Runnable {
        private final String msg;
        public WriteTask(String msg) {
            this.msg = msg;
        }

        @Override
        public void run() {
            writer.println(msg);
        }
    }
}

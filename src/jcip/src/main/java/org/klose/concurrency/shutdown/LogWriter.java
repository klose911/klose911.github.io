package org.klose.concurrency.shutdown;

import java.io.PrintWriter;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

public class LogWriter {
    private final BlockingQueue<String> queue;
    private final LoggerThread loggerThread;
    /**
     * 表示是否关闭Service
     */
    private boolean isShutdown;
    /**
     * 队列中待处理数据的数量
     */
    private int reservations;

    public LogWriter(PrintWriter writer) {
        this.queue = new LinkedBlockingQueue<>(100);
        this.loggerThread = new LoggerThread(writer);
    }

    public void start() {
        loggerThread.start();
    }

    public void shutDown() {
        synchronized (this) {
            isShutdown = true;
        }
        loggerThread.interrupt();
    }

    public void log(String msg) throws InterruptedException {
        synchronized (this) {
            // service已关闭后调用log方法直接抛出异常  
            if (isShutdown) {
                throw new IllegalStateException("Service has been shut down");
            }
            ++reservations;
        }
        // BlockingQueue本身就是线程安全的, put方法的调用不在同步代码块中  
        // 我们只需要保证isShutdown和reservations是线程安全的即可  
        queue.put(msg);
    }

    private class LoggerThread extends Thread {
        private final PrintWriter writer;
        private LoggerThread(PrintWriter writer) {
            this.writer = writer;
        }

        public void run() {
            try {
                while (true) {
                    try {
                        synchronized (this) {
                            // 当service已关闭且处理完队列中的所有数据时才跳出while循环  
                            if (isShutdown && reservations == 0) {
                                break;
                            }
                        }
                        String msg = queue.take();
                        synchronized (this) {
                            --reservations;
                        }
                        writer.println(msg);
                    } catch (InterruptedException e) {
                        // 发生InterruptedException异常时不应该立刻跳出while循环  
                        // 而应该继续输出log, 直到处理完队列中的所有数据  
                    }
                }
            } finally {
                writer.close();
            }
        }
    }
}

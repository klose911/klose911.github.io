/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.shutdown;

import java.io.PrintWriter;
import java.io.Writer;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

public class BrokenLogWriter {

    private final BlockingQueue<String> queue;
    private final LoggerThread logger;
    private final int CAPACITY = 100;

    public BrokenLogWriter(PrintWriter writer) {
        this.queue = new LinkedBlockingQueue<>(CAPACITY);
        this.logger = new LoggerThread(writer);
    }

    public void start() {
        logger.start();
    }

    /**
     * 需要打印数据的线程调用该方法, 将待打印数据加入阻塞队列
     *
     * @param msg
     * @throws java.lang.InterruptedException
     */
    public void log(String msg) throws InterruptedException {
        queue.put(msg);
    }

    /**
     * 负责从阻塞队列中取出数据输出的线程
     */
    private class LoggerThread extends Thread {

        private final PrintWriter writer;

        private LoggerThread(PrintWriter writer) {
            this.writer = writer;
        }

        @Override
        public void run() {
            try {
                while (true) {
                    writer.println(queue.take());
                }
            } catch (InterruptedException ignored) {
            } finally {
                writer.close();
            }
        }
    }

    /**
     * 该方法用于停止LoggerThread线程
     */
    public void shutDown() {
        logger.interrupt();
    }
}

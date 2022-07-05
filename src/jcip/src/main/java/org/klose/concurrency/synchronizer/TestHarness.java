/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.synchronizer;

import java.util.concurrent.CountDownLatch;

/**
 *
 * @author klose
 */
public class TestHarness {

    public long timeTasks(int nThreads, final Runnable task)
            throws InterruptedException {
        // startGate用来控制子线程，当所有的子线程准备就绪时候，主线程执行startGate的countDown操作，让子线程一起运行
        final CountDownLatch startGate = new CountDownLatch(1);
        //endGate用来控制主线程，每个子线程运行完毕后，对endGate执行countDown操作，当所有的子线程结束后，主线程恢复运行
        final CountDownLatch endGate = new CountDownLatch(nThreads);
        for (int i = 0; i < nThreads; i++) {
            Thread t = new Thread() {
                @Override
                public void run() {
                    try {
                        //n个线程通过start gate处于等待状态
                        startGate.await();
                        try {
                            task.run();
                        } finally {
                            endGate.countDown();
                        }
                    } catch (InterruptedException ignored) {
                    }
                }
            };
            t.start();
        }
        long start = System.nanoTime();
        //start gate开启，n个线程同时开始运行
        startGate.countDown();
        
        endGate.await();
        long end = System.nanoTime();
        return end - start;
    }
}

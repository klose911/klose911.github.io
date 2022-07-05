/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.cancel.interruption;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

public class InterruptedExceptionContinueHandler implements Runnable {

    private final BlockingQueue<Integer> queue;

    public InterruptedExceptionContinueHandler(BlockingQueue<Integer> queue) {
        this.queue = queue;
    }

    @Override
    public void run() {
        while (!Thread.currentThread().isInterrupted()) {
            dosomething();
        }
        System.out.println(queue.size());
    }

    private void dosomething() {
        // cancelled变量用于表明线程是否发生过中断
        boolean cancelled = false;
        for (int i = 0; i < 10000; i++) {
            try {
                queue.put(i);
            } catch (InterruptedException e) {
                // 就算发生了InterruptedException, 循环也希望继续运行下去, 此时将cancelled设置为true, 以表明遍历过程中发生了中断
                System.out.println("InterruptedException happened when i = " + i);
                cancelled = true;
            }
        }
        if (cancelled) {
            // 如果当前线程曾经发生过中断, 就将其中断标记设置为true, 以通知dosomething方法的上层调用栈
            Thread.currentThread().interrupt();
        }
    }

    public static void main(String[] args) throws InterruptedException {
        Thread t = new Thread(new InterruptedExceptionContinueHandler(new LinkedBlockingQueue<>()));
        t.start();

        // 启动线程2ms后设置其中断标记为true
        Thread.sleep(2);
        t.interrupt();
    }
}

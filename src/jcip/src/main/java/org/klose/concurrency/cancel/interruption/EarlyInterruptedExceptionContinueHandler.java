/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.cancel.interruption;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

public class EarlyInterruptedExceptionContinueHandler implements Runnable {

    private final BlockingQueue<Integer> queue;

    public EarlyInterruptedExceptionContinueHandler(BlockingQueue<Integer> queue) {
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
        for (int i = 0; i < 10000; i++) {
            try {
                queue.put(i);
            } catch (InterruptedException e) {
                System.out.println("InterruptedException happened when i = " + i);
                //过早重置中断状态为true会导致put方法又抛出InterruptedException异常, 如此往复直到循环结束.
                Thread.currentThread().interrupt();
            }
        }
    }

    public static void main(String[] args) throws InterruptedException {
        Thread t = new Thread(new EarlyInterruptedExceptionContinueHandler(new LinkedBlockingQueue<>()));
        t.start();

        // 启动线程2ms后设置其中断标记为true
        Thread.sleep(2);
        t.interrupt();
    }
}

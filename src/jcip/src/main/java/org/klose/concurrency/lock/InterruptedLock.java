/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.lock;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class InterruptedLock extends Thread {

    private static final Lock LOCK = new ReentrantLock();

    @Override
    public void run() {
        try {
            // 可中断申请, 在申请锁的过程中如果当前线程被中断, 将抛出InterruptedException异常  
            LOCK.lockInterruptibly();
        } catch (InterruptedException e) {
            System.out.println("interruption happened");
            return;
        }

        // 如果运行到这里, 说明已经申请到锁, 且没有发生异常  
        try {
            System.out.println("run is holding the lock");
        } finally {
            LOCK.unlock();
        }
    }

    public static void main(String[] args) throws InterruptedException {
        try {
            LOCK.lock();
            System.out.println("main is holding the lock.");
            Thread thread = new InterruptedLock();
            thread.start();
            // 1s后中断thread线程, 该线程此时应该阻塞在lockInterruptibly方法上  
            Thread.sleep(1000);
            // 中断thread线程将导致其抛出InterruptedException异常.  
            thread.interrupt();
            Thread.sleep(1000);
        } finally {
            LOCK.unlock();
        }
    }
}

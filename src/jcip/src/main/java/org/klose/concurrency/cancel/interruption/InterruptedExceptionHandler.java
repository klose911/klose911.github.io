/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.cancel.interruption;

/**
 *
 * @author klose
 */
public class InterruptedExceptionHandler implements Runnable{

    private final Object lock = new Object();

    @Override
    public void run() {
        while (!Thread.currentThread().isInterrupted()) {
            dosomething();
        }
    }

    private void dosomething() {
        try {
            // Object.wait是一个可中断的阻塞方法
            // 如果在其阻塞期间检查到当前线程的中断标记为true, 会重置中断标记后从阻塞状态返回, 并抛出InterruptedException异常  
            synchronized (lock) {
                lock.wait();  // 会重置中断标记后从阻塞状态返回, 并抛出InterruptedException异常 
            }
        } catch (InterruptedException e) {
            System.out.println("InterruptedException happened");
            // catch住InterruptedException后设置当前线程的中断标记为true, 以供调用栈上层进行相应的处理  
            // 在此例中, dosomething方法的调用栈上层是run方法.  
            Thread.currentThread().interrupt();
        }
    }

    public static void main(String[] args) throws InterruptedException {
        Thread t = new Thread(new InterruptedExceptionHandler());
        t.start();
        Thread.sleep(1000);
        // 启动线程1s后设置其中断标记为true
        t.interrupt();
    }
}

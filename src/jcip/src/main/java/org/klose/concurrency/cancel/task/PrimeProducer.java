/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.cancel.task;

import java.math.BigInteger;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

class PrimeProducer extends Thread {

    private final BlockingQueue<BigInteger> queue;

    PrimeProducer(BlockingQueue<BigInteger> queue) {
        this.queue = queue;
    }

    public void run() {
        try {
            BigInteger p = BigInteger.ONE;
            // 每次循环前检查当前线程的中断标记, 如果中断标记为设定为true, 则循环结束  
            // 就算当前线程阻塞在put方法上, 在阻塞期间也会周期性检查中断标记, 
            //一旦发现中断标记为true, 就会从阻塞状态中返回, 并抛出InterruptedException异常  
            while (!Thread.currentThread().isInterrupted()) {
                queue.put(p = p.nextProbablePrime());
            }
        } catch (InterruptedException consumed) {
            System.out.println("InterruptedException happened");
        }
    }

    public void cancel() {
        // interrupt方法会将当前线程的中断标记设置为true  
        interrupt();
    }

    public static void main(String[] args) {
        // 设置队列的最大容量为10  
        BlockingQueue<BigInteger> primes = new LinkedBlockingQueue<>(10);
        PrimeProducer producer = new PrimeProducer(primes);
        producer.start();

        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
        }
        //
        producer.cancel();
    }
}

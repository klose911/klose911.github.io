/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.cancel.task;

import java.math.BigInteger;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

/**
 *
 * @author klose
 */
public class BrokenPrimeProducer extends Thread {

    private final BlockingQueue<BigInteger> queue;
    private volatile boolean cancelled = false;

    BrokenPrimeProducer(BlockingQueue<BigInteger> queue) {
        this.queue = queue;
    }

    @Override
    public void run() {
        try {
            BigInteger p = BigInteger.ONE;
            while (!cancelled) {
                // 当队列已满时, put方法将会阻塞. 一旦put方法阻塞, 且没有其他线程从队列中取数据时, 阻塞将一直持续下去  
                queue.put(p = p.nextProbablePrime());
            }
        } catch (InterruptedException consumed) {
//            Thread.currentThread().interrupt();
        }
    }

    public void cancel() {
        cancelled = true;
    }

    public static void main(String[] args) {
        BlockingQueue<BigInteger> primes;
        primes = new LinkedBlockingQueue<>(10);
        BrokenPrimeProducer producer = new BrokenPrimeProducer(primes);
        producer.start();
        try {
            Thread.sleep(1000);
        } catch (InterruptedException ex) {
            //Logger.getLogger(BrokenPrimeProducer.class.getName()).log(Level.SEVERE, null, ex);
        } finally {
            producer.cancel();
        }
    }
}

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.cancel.task;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import net.jcip.annotations.GuardedBy;

/**
 *
 * @author klose
 */
public class PrimeGenerator implements Runnable {

    @GuardedBy("this")
    private final List<BigInteger> primes
            = new ArrayList<BigInteger>();

    //自定义的flag, 为保证线程可见性, 将其声明为volatile 
    private volatile boolean cancelled;

    @Override
    public void run() {
        BigInteger p = BigInteger.ONE;
        // 每次循环之前检查cancelled标记的值, 如果cancelled为true, 循环终止, 线程也就运行结束了  
        while (!cancelled) {
            p = p.nextProbablePrime();
            synchronized (this) {
                primes.add(p);
            }
        }
    }

    public void cancel() {
        cancelled = true;
    }

    public synchronized List<BigInteger> get() {
        return new ArrayList<>(primes);
    }

    public static void main(String[] args) {
        PrimeGenerator generator = new PrimeGenerator();
        Thread t = new Thread(generator);
        t.start();
        
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            //除非明确知道主线程应该终止，不然通常情况下应该重新抛出InterruptedException或者恢复被中断的线程
        }
        // 通过调用cancel方法, 将自定义的cancelled标记设置为true, 从而使得线程t运行终止  
        generator.cancel();
        System.out.println(generator.get().size());
    }
}

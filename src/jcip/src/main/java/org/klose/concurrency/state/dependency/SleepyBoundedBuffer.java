/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.state.dependency;

import net.jcip.annotations.ThreadSafe;

@ThreadSafe
public class SleepyBoundedBuffer<V> extends BaseBoundedBuffer<V> {

    private static final long SLEEP_GRANULARITY = 1000L;

    public SleepyBoundedBuffer(int size) {
        super(size);
    }

    public void put(V v) throws InterruptedException {
        while (true) {
            synchronized (this) {
                if (!isFull()) {
                    doPut(v);
                    return;
                }
            }
            // 释放锁后sleep一段时间再进行重试  
            Thread.sleep(SLEEP_GRANULARITY);
        }
    }

    public V take() throws InterruptedException {
        while (true) {
            synchronized (this) {
                if (!isEmpty()) {
                    return doTake();
                }
            }
            // 释放锁后sleep一段时间再进行重试  
            Thread.sleep(SLEEP_GRANULARITY);
        }
    }
}

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.state.dependency;

/**
 *
 * @author klose
 * @param <V>
 */
public class BoundedBuffer<V> extends BaseBoundedBuffer<V> {

    public BoundedBuffer(int capacity) {
        super(capacity);
    }

    public synchronized void put(V v) throws InterruptedException {
        // 当缓冲区已满时将线程挂起, 等待其他线程唤醒  
        // 不给唤醒之后再次判断缓冲区是否已满         
        while (isFull())
            wait();
        doPut(v);
        // 操作完成后唤醒其他线程  
        notifyAll();
    }

    public synchronized V take() throws InterruptedException {
        // 当缓冲区为空时将线程挂起, 等待其他线程唤醒  
        // 被唤醒之后再次判断缓冲区是否为空  
        while (isEmpty())
            wait();
        V v = doTake();
        // 操作完成后唤醒其他线程  

        notifyAll();
        return v;
    }
}

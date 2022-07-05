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
public abstract class BaseBoundedBuffer<V> {  
    private final V[] buf;  
    private int tail;  
    private int head;  
    private int count;  
  
    @SuppressWarnings("unchecked")  
    protected BaseBoundedBuffer(int capacity) {  
        this.buf = (V[]) new Object[capacity];  
    }  
  
    protected synchronized final void doPut(V v) {  
        buf[tail] = v;  
        if (++tail == buf.length) {  
            tail = 0;  
        }  
        ++count;  
    }  
  
    protected synchronized final V doTake() {  
        V v = buf[head];  
        buf[head] = null;  
        if (++head == buf.length) {  
            head = 0;  
        }  
        --count;  
        return v;  
    }  
  
    public synchronized final boolean isFull() {  
        return count == buf.length;  
    }  
  
    public synchronized final boolean isEmpty() {  
        return count == 0;  
    }  
        
}   

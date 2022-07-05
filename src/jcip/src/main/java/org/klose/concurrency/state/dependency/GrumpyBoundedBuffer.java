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
public class GrumpyBoundedBuffer<V> extends BaseBoundedBuffer<V> {

    public GrumpyBoundedBuffer(int capacity) {
        super(capacity);
    }
    
    public synchronized void put(V v) throws BufferFullException {
        if (isFull()) {
            throw new BufferFullException();
        }
        doPut(v);
    }

    public synchronized V take() throws BufferEmptyException {
        if (isEmpty()) {
            throw new BufferEmptyException();
        }
        return doTake();
    }
}

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.synchronizer;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.Semaphore;

public class BoundedHashSet<T> {

    private final Set<T> set;
    private final Semaphore sem;

    public BoundedHashSet(int bound) {
        this.set = Collections.synchronizedSet(new HashSet<>());
        // 设定Semaphore对象中的permit的最大个数  
        sem = new Semaphore(bound);
    }

    public boolean add(T o) throws InterruptedException {
        // 每次add时就向semaphore对象申请一个permit  
        sem.acquire();
        boolean wasAdded = false;
        try {
            wasAdded = set.add(o);
            return wasAdded;
        } finally {
            if (!wasAdded) {
                // set里已经存在元素时候release permit  
                sem.release();
            }
        }
    }

    public boolean remove(T o) {
        boolean wasRemoved = set.remove(o);
        if (wasRemoved) {
            // 成功移除后将release permit 
            sem.release();
        }
        return wasRemoved;
    }
}

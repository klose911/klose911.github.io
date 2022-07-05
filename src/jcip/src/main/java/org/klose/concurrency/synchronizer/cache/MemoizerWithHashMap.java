/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.synchronizer.cache;

import java.util.HashMap;
import java.util.Map;
import net.jcip.annotations.GuardedBy;

public class MemoizerWithHashMap<A, V> implements Computable<A, V> {

    @GuardedBy("this")
    private final Map<A, V> cache;
    private final Computable<A, V> c;

    public MemoizerWithHashMap(Computable<A, V> c) {
        this.cache = new HashMap<>();
        this.c = c;
    }

    @Override
    public synchronized V compute(A arg) throws InterruptedException {
        V result = cache.get(arg);
        if (result == null) {
            result = c.compute(arg);
            cache.put(arg, result);
        }
        return result;
    }
}

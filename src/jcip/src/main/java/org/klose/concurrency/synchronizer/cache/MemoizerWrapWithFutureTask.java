/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.synchronizer.cache;

import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.FutureTask;
import static org.klose.concurrency.synchronizer.Preloader.launderThrowable;

public class MemoizerWrapWithFutureTask<A, V> implements Computable<A, V> {

    private final Map<A, Future<V>> cache;
    private final Computable<A, V> c;

    public MemoizerWrapWithFutureTask(Computable<A, V> c) {
        this.cache = new ConcurrentHashMap<>();
        this.c = c;
    }

    /**
     *
     * @param arg
     * @return
     * @throws InterruptedException
     */
    @Override
    public V compute(final A arg) throws InterruptedException {
        Future<V> f = cache.get(arg);
        if (f == null) {
            Callable<V> eval = () -> c.compute(arg);
            FutureTask<V> ft = new FutureTask<>(eval);
            f = ft;
            cache.put(arg, ft);
            ft.run(); // call to c.compute happens here
        }
        try {
            return f.get();
        } catch (ExecutionException e) {
            throw launderThrowable(e.getCause());
        }
    }
}

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.shutdown;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.AbstractExecutorService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

public class TrackingExecutor extends AbstractExecutorService {

    private final ExecutorService exec;
    private final Set<Runnable> tasksCancelledAtShutdown
            = Collections.synchronizedSet(new HashSet<>());

    public TrackingExecutor(ExecutorService exec) {
        this.exec = exec;
    }

    public List<Runnable> getCancelledTasks() {
        if (!exec.isTerminated()) {
            throw new IllegalStateException("illegal task status");
        }
        return new ArrayList<>(tasksCancelledAtShutdown);
    }

    @Override
    public void execute(final Runnable runnable) {
        exec.execute(() -> {
            try {
                runnable.run();
            } finally {
                if (isShutdown()
                        && Thread.currentThread().isInterrupted()) {
                    tasksCancelledAtShutdown.add(runnable);
                }
            }
        });
    }
// delegate other ExecutorService methods to exec

    @Override
    public void shutdown() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public List<Runnable> shutdownNow() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public boolean isShutdown() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public boolean isTerminated() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public boolean awaitTermination(long timeout, TimeUnit unit) throws InterruptedException {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}

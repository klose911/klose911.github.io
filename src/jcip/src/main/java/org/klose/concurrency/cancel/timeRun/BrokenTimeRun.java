/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.cancel.timeRun;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/**
 *
 * @author klose
 */
public class BrokenTimeRun {

    private static final ScheduledExecutorService CANCEL_EXEC = Executors.newSingleThreadScheduledExecutor();

    /**
     *
     * @param r
     * @param timeout
     * @param unit
     */
    public static void timedRun(Runnable r, long timeout, TimeUnit unit) {
        final Thread taskThread = Thread.currentThread();
        CANCEL_EXEC.schedule(() -> {
            taskThread.interrupt();
        }, timeout, unit);
        r.run();
    }
}

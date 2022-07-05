/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.executor.threadFactory;

import java.util.concurrent.atomic.AtomicInteger;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author klose
 */
public class MyAppThread extends Thread {
    public static final String DEFAULT_NAME = "MyAppThread";
    private static final AtomicInteger CREATED = new AtomicInteger();
    private static final AtomicInteger ALIVE = new AtomicInteger();
    private static final Logger LOGGER = LoggerFactory.getLogger(MyAppThread.class);
    private static volatile boolean debugLifecycle = false;

    public MyAppThread(Runnable r) {
        this(r, DEFAULT_NAME);
    }

    public MyAppThread(Runnable runnable, String name) {
        // 为自定义的Thread类指定线程名称
        super(runnable, name + "-" + CREATED.incrementAndGet());
        // 设置UncaughtExceptionHandler. UncaughtExceptionHandler的uncaughtException方法将在线程运行中抛出未捕获异常时由系统调用
        setUncaughtExceptionHandler();
    }

    private void setUncaughtExceptionHandler() {
        setUncaughtExceptionHandler((Thread t, Throwable e) -> {
            LOGGER.error("UNCAUGHT in thread " + t.getName(), e);
        });
    }

    public static int getThreadsCreated() {
        return CREATED.get();
    }

    public static int getThreadsAlive() {
        return ALIVE.get();
    }

    public static boolean getDebug() {
        return debugLifecycle;
    }

    public static void setDebug(boolean b) {
        debugLifecycle = b;
    }

    @Override
    public void run() {
        // Copy debug flag to ensure consistent value throughout.
        boolean debug = debugLifecycle;
        if (debug) 
            LOGGER.info("Created {}", getName());
        
        try {
            ALIVE.incrementAndGet();
            super.start();
        } finally {
            ALIVE.decrementAndGet();
            if (debug) {
                LOGGER.info("Exiting {}", getName());
            }
        }
    }
}

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.shutdown;

import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 *
 * @author klose
 */
public class CheckMailService {

    public boolean checkMail(Set<String> hosts, long timeout, TimeUnit unit)
            throws InterruptedException {
        ExecutorService exec = Executors.newCachedThreadPool();
        //因为final无法使用volatile, 所以使用原子变量来保证同步
        final AtomicBoolean hasNewMail = new AtomicBoolean(false);
        try {
            hosts.forEach((host) -> {
                exec.execute(() -> {
                    if (checkMail(host)) {
                        hasNewMail.set(true);
                    }
                });
            });
        } finally {
            exec.shutdown();
            exec.awaitTermination(timeout, unit);
        }
        return hasNewMail.get();
    }

    private boolean checkMail(String host) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}

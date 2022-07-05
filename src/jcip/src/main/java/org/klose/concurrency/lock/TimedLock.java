/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.lock;

import java.util.concurrent.TimeUnit;

/**
 *
 * @author klose
 */
public class TimedLock {

    public boolean transferMoney(Account fromAcct, Account toAcct, DollarAmount amount,
            long timeout, TimeUnit unit)  throws InsufficientFundsException, InterruptedException {
        // 截止时间  
        long stopTime = System.nanoTime() + unit.toNanos(timeout);

        while (true) {
            if (fromAcct.getLock().tryLock()) {
                try {
                    if (toAcct.getLock().tryLock()) {
                        try {
                            if (fromAcct.getBalance().compareTo(amount) < 0) {
                                throw new InsufficientFundsException();
                            } else {
                                fromAcct.debit(amount);
                                toAcct.credit(amount);
                                return true;
                            }
                        } finally {
                            // 成功申请到锁时才需要释放锁  
                            toAcct.getLock().unlock();
                        }
                    }
                } finally {
                    // 成功申请到锁时才需要释放锁  
                    fromAcct.getLock().unlock();
                }
            }
            // 如果已经超过截止时间直接返回false, 说明转账没有成功. 否则进行下次尝试.  
            if (System.nanoTime() < stopTime) {
                return false;
            }
            Thread.sleep(1000L); 
        }
    }
 
    private  class InsufficientFundsException extends Exception {

        public InsufficientFundsException() {
        }
    }
}

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.lock;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Account {

    private final Lock lock = new ReentrantLock();
    
    private volatile DollarAmount balance;

    public Account(DollarAmount balance) {
        this.balance = balance;
    }

    public DollarAmount getBalance() {
        return balance;
    }

    public void debit(DollarAmount val) {
        this.balance = new DollarAmount((this.balance.getAmount().subtract(val.getAmount())));
    }

    public void credit(DollarAmount val) {
        this.balance = new DollarAmount(this.balance.getAmount().add(val.getAmount()));
    }

    public Lock getLock() {
        return lock;
    }
    
    
}

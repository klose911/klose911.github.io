/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.lock;

import java.math.BigDecimal;

/**
 *
 * @author klose
 */
public class DollarAmount {

    private final BigDecimal amount;

    public DollarAmount(final BigDecimal amount) {
        this.amount = amount;
    }

    public int compareTo(DollarAmount d) {
        return this.amount.compareTo(d.amount);
    }

    public BigDecimal getAmount() {
        return amount;
    }

}

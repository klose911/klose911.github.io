/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.synchronizer.cache;

import java.math.BigInteger;

public class ExpensiveFunction
        implements Computable<String, BigInteger> {

    @Override
    public BigInteger compute(String arg) {
        // after deep thought...
        return new BigInteger(arg);
    }
}

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.synchronizer.cache;

public interface Computable<A, V> {

    V compute(A arg) throws InterruptedException;
}

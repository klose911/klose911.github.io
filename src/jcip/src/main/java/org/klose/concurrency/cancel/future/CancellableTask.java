/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.cancel.future;

import java.util.concurrent.Callable;
import java.util.concurrent.RunnableFuture;

public interface CancellableTask<T> extends Callable<T> {

    void cancel();

    RunnableFuture<T> newTask();
}

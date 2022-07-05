/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.synchronizer;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.FutureTask;

public class Preloader {

    private final FutureTask<ProductInfo> future;
    // FutureTask实现了Runnable接口, 因此可以将future对象放进thread中执行  
    private final Thread thread;

    public Preloader() {
        // loadProductInfo方法用于加载产品信息, 这是一个耗时操作
        future = new FutureTask<>(new Callable<ProductInfo>() {
            @Override
            public ProductInfo call() throws Exception {
                return loadProductInfo();
            }
        });
        thread = new Thread(future);
    }

    // 提供start方法启动线程, 而不是在构造方法中直接启动是为了防止this逃逸  
    public void start() {
        // 启动thread, 执行future  
        thread.start();
    }

    // 当需要计算结果时, 就调用get方法获得产品信息的加载结果.   
    private ProductInfo get()
            throws DataLoadException, InterruptedException {
        try {
            // get方法将阻塞, 直到产品信息加载完成, 或者抛出异常  
            return future.get();
        } catch (ExecutionException e) {
            Throwable cause = e.getCause();
            if (cause instanceof DataLoadException) {
                throw (DataLoadException) cause;
            } else {
                throw launderThrowable(cause);
            }
        }
    }

    /**
     * If the Throwable is an Error, throw it; if it is a RuntimeException
     * return it, otherwise throw IllegalStateException
     */
    public static RuntimeException launderThrowable(Throwable t) {
        if (t instanceof RuntimeException) {
            return (RuntimeException) t;
        } else if (t instanceof Error) {
            throw (Error) t;
        } else {
            throw new IllegalStateException("Not unchecked", t);
        }
    }

    private static class DataLoadException extends Exception {

        public DataLoadException() {
        }
    }

    private ProductInfo loadProductInfo() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    private static class ProductInfo {
    }

}

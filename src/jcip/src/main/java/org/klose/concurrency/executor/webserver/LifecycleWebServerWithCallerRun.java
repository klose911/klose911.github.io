/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.executor.webserver;

import org.klose.concurrency.executor.webserver.LifecycleWebServer;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author klose
 */
public class LifecycleWebServerWithCallerRun {
    // MAX_THREAD_COUNT和MAX_QUEUE_COUNT的值根据系统的实际情况确定  

    private static final int MAX_THREAD_COUNT = 100;
    private static final int MAX_QUEUE_COUNT = 1000;
    private static final Logger LOGGER = LoggerFactory.getLogger(LifecycleWebServerWithCallerRun.class);

    // 使用有界队列作为task队列, 当有界队列满时, 将触发饱和策略  
    private final ThreadPoolExecutor exec = new ThreadPoolExecutor(0, MAX_THREAD_COUNT, 60L, TimeUnit.SECONDS,
            new ArrayBlockingQueue<>(MAX_QUEUE_COUNT));

    public void start() throws IOException {
        // 设置饱和策略为CallerRunsPolicy  
        exec.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());
        ServerSocket socket = new ServerSocket(80);
        while (!exec.isShutdown()) {
            try {
                final Socket conn = socket.accept();
                exec.execute(() -> {
                    handleRequest(conn);
                });
            } catch (RejectedExecutionException e) {
                if (!exec.isShutdown()) {
                    LOGGER.error("task submission rejected", e);
                }
            }
        }
    }

    public void stop() {
        exec.shutdown();
    }

    void handleRequest(Socket connection) {
        Request req = readRequest(connection);
        if (isShutdownRequest(req)) {
            stop();
        } else {
            dispatchRequest(req);
        }
    }

    public static void main(String[] args) {
        LifecycleWebServer server = new LifecycleWebServer();
        try {
            // 在main线程中启动server  
            server.start();
        } catch (IOException e) {
            LOGGER.error("io exception occurs", e);
        }
    }

    private Request readRequest(Socket connection) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    private boolean isShutdownRequest(Request req) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    private void dispatchRequest(Request req) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    private static class Request {
    }
}

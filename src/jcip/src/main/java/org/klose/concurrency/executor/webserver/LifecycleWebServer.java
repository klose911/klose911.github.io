/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.executor.webserver;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class LifecycleWebServer {
    
    private final static Logger LOGGER = LoggerFactory.getLogger(LifecycleWebServer.class); 
    
    private static final int NTHREADS = 100;
    // 创建线程池  
    private static final ExecutorService EXEC
            = Executors.newFixedThreadPool(NTHREADS);
   
    public void start() throws IOException {
        ServerSocket socket = new ServerSocket(80);
        while (!EXEC.isShutdown()) {
            try {
                final Socket conn = socket.accept();
                EXEC.execute(() -> {
                    handleRequest(conn);
                });
            } catch (RejectedExecutionException e) {
                // 线程池关闭后提交任务将抛出RejectedExecutionException异常  
                if (!EXEC.isShutdown()) {
                    LOGGER.error("task submission rejected", e);
                }
            }
        }
    }

    public void stop() {
        EXEC.shutdown();
    }

    void handleRequest(Socket connection) {
        Request req = readRequest(connection);
        // 如果是关闭请求, 就关闭线程池, 否则分发该请求  
        if (isShutdownRequest(req)) {
            stop();
        } else {
            dispatchRequest(req);
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

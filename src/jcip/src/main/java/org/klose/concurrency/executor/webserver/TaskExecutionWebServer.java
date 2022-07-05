/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.executor.webserver;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

class TaskExecutionWebServer {

    private static final int NTHREADS = 100;
    // 创建线程池  
    private static final Executor EXEC
            = Executors.newFixedThreadPool(NTHREADS);

    public static void main(String[] args) throws IOException {
        ServerSocket socket = new ServerSocket(80);
        while (true) {
            final Socket connection = socket.accept();
            Runnable task = () -> {
                handleRequest(connection);
            };
            // 将任务提交给线程池执行 
            EXEC.execute(task);
        }
    }

    private static void handleRequest(Socket connection) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}

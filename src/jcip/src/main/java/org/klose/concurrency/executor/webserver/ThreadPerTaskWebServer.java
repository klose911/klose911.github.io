/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.executor.webserver;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

class ThreadPerTaskWebServer {

    public static void main(String[] args) throws IOException {
        ServerSocket socket = new ServerSocket(80);
        while (true) {
            final Socket connection = socket.accept();
            Runnable task = () -> {
                handleRequest(connection);
            };
            new Thread(task).start();
        }
    }

    private static void handleRequest(Socket connection) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}

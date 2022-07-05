/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.cancel.interruption;

import java.io.IOException;
import java.io.InputStream;
import java.net.Socket;

public class ReaderThread extends Thread {
    private final Socket socket;
    private final InputStream in;
    private final static int BUFSZ = 8192;

    public ReaderThread(Socket socket) throws IOException {
        this.socket = socket;
        this.in = socket.getInputStream();
    }


    @Override
    public void interrupt() {
        try {
            // 加入关闭socket的代码
            // 如果发生中断时, 线程阻塞在read方法上, socket的关闭会导致read方法抛出SocketException
            // 然后run方法运行完毕 
            socket.close();
        } catch (IOException ignored) {
        } finally {
            //如果是在一个可响应阻塞的方法，继续抛出异常
            super.interrupt();
        }
    }

    @Override
    public void run() {
        try {
            byte[] buf = new byte[BUFSZ];
            while (true) {
                int count = in.read(buf);
                if (count < 0) {
                    break;
                } else if (count > 0) {
                    processBuffer(buf, count);
                }
            }
        } catch (IOException e) {
            /* Allow thread to exit */ 
        }
    }

    private void processBuffer(byte[] buf, int count) {
        // do something ...
    }
}

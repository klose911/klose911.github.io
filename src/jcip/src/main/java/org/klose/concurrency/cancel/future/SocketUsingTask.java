/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.cancel.future;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.net.Socket;
import java.util.concurrent.FutureTask;
import java.util.concurrent.RunnableFuture;
import net.jcip.annotations.GuardedBy;

public class SocketUsingTask<T>
        implements CancellableTask<T> {

    @GuardedBy("this")
    private Socket socket;

    protected synchronized void setSocket(Socket s) {
        socket = s;
    }

    @Override
    public synchronized void cancel() {
        try {
            if (socket != null) {
                socket.close();
            }
        } catch (IOException ignored) {
        }
    }

    @Override
    public RunnableFuture<T> newTask() {
        return new FutureTask<T>(this) {
            @Override
            // 定义FutureTask的匿名内部类, 并覆盖cancel方法, 向其中加入关闭socket的操作  
            public boolean cancel(boolean mayInterruptIfRunning) {
                try {
                    SocketUsingTask.this.cancel();
                } finally {
                    return super.cancel(mayInterruptIfRunning);
                }
            }
        };
    }

    @Override
    public T call() throws Exception {
        Reader reader = new InputStreamReader(socket.getInputStream());
        Writer writer = new OutputStreamWriter(socket.getOutputStream());
        try {
            writer.write("Hello Server.");
            writer.flush();
            //写完以后进行读操作
            char chars[] = new char[64];
            int len;
            StringBuffer sb = new StringBuffer();
            sb.append("from server: ");
            while ((len = reader.read(chars)) != -1) {
                sb.append(len);
            }
            System.out.println("from server: " + sb);
            return (T) sb.toString();
        } finally {
            reader.close();
            writer.close();
        }
    }
}

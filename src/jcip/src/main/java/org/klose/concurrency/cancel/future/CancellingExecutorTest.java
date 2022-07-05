/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.cancel.future;

import java.io.IOException;
import java.net.Socket;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.logging.Level;
import java.util.logging.Logger;
import static org.klose.concurrency.synchronizer.Preloader.launderThrowable;

/**
 *
 * @author klose
 */
public class CancellingExecutorTest {

    public static void main(String[] args) throws IOException {
        CancellingExecutor executor = new CancellingExecutor();
        SocketUsingTask<String> task = new SocketUsingTask<>();
        task.setSocket(new Socket("www.baidu.com", 80));
        Future<String> future = executor.submit(task);
        try {
            future.get(1000L, TimeUnit.MILLISECONDS);
        } catch (TimeoutException | InterruptedException ex) {
        } catch (ExecutionException ex) {
            throw launderThrowable(ex.getCause());
        } finally {
            future.cancel(true); // interrupt if running
            executor.shutdown();
        }
    }
}

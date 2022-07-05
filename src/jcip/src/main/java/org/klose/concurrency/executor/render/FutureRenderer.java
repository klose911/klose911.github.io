/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.executor.render;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import static org.klose.concurrency.synchronizer.Preloader.launderThrowable;

public class FutureRenderer implements Render {

    private static final int NTHREADS = 100;

    private final ExecutorService executor = Executors.newFixedThreadPool(NTHREADS);

    @Override
    public void renderPage(CharSequence source) {
        final List<ImageInfo> imageInfos = scanForImageInfo(source);
        Callable<List<ImageData>> task;
        task = () -> {
            List<ImageData> result
                    = new ArrayList<>();
            imageInfos.forEach((imageInfo) -> {
                result.add(imageInfo.downloadImage());
            });
            return result;
        };
        Future<List<ImageData>> future = executor.submit(task);
        // 渲染文本  
        renderText(source);
        try {
            // get方法将阻塞, 直到task完成下载  
            List<ImageData> imageData = future.get();
            imageData.forEach((data) -> {
                // 渲染图片  
                renderImage(data);
            });
        } catch (InterruptedException e) {
            // Re-assert the thread’s interrupted status
            Thread.currentThread().interrupt();
            // We don’t need the result, so cancel the task too
            future.cancel(true);
        } catch (ExecutionException e) {
            throw launderThrowable(e.getCause());
        }
    }
}

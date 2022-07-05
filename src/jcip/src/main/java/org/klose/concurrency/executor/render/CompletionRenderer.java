/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.executor.render;

import java.util.List;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import static org.klose.concurrency.synchronizer.Preloader.launderThrowable;

public class CompletionRenderer implements Render {

    private final ExecutorService executor = Executors.newCachedThreadPool();

    @Override
    public void renderPage(CharSequence source) {
        List<ImageInfo> info = scanForImageInfo(source);
        // 将图片下载拆分为多个任务  
        CompletionService<ImageData> completionService
                = new ExecutorCompletionService<>(executor);
        info.forEach((imageInfo) -> {
            completionService.submit(() -> imageInfo.downloadImage());
        });
        renderText(source);
        try {
            for (int t = 0, n = info.size(); t < n; t++) {
                // take方法可能阻塞: 当已完成队列中为空时  
                Future<ImageData> f = completionService.take();
                // get方法不会阻塞, 因为从take方法返回的Future对象肯定是已完成的  
                ImageData imageData = f.get();
                renderImage(imageData);
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        } catch (ExecutionException e) {
            throw launderThrowable(e.getCause());
        }
    }
}

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.shutdown;

import java.net.URL;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import net.jcip.annotations.GuardedBy;

public abstract class WebCrawler {

    private volatile TrackingExecutor exec;
    @GuardedBy("this")
    private final Set<URL> urlsToCrawl = new HashSet<URL>();

    public synchronized void start() {
        exec = new TrackingExecutor(
                Executors.newCachedThreadPool());
        urlsToCrawl.forEach((url) -> {
            submitCrawlTask(url);
        });
        urlsToCrawl.clear();
    }

    public synchronized void stop() throws InterruptedException {
        try {
            //保存从未开始运行的任务
            saveUncrawled(exec.shutdownNow());
            if (exec.awaitTermination(10, TimeUnit.SECONDS)) {
                //保存运行但被cancel的任务
                saveUncrawled(exec.getCancelledTasks());
            }
        } finally {
            exec = null;
        }
    }

    protected abstract List<URL> processPage(URL url);

    private void saveUncrawled(List<Runnable> uncrawled) {
        uncrawled.forEach((task) -> {
            urlsToCrawl.add(((CrawlTask) task).getPage());
        });
    }

    private void submitCrawlTask(URL u) {
        exec.execute(new CrawlTask(u));
    }

    private class CrawlTask implements Runnable {

        private final URL url;

        private CrawlTask(URL u) {
            url = u;
        }

        @Override
        public void run() {
            for (URL link : processPage(url)) {
                if (Thread.currentThread().isInterrupted()) {
                    return;
                }
                submitCrawlTask(link);
            }
        }

        public URL getPage() {
            return url;
        }
    }
}

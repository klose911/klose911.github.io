package org.klose.concurrency.synchronizer.blockingQueue;


import java.io.File;
import java.io.FileFilter;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

public class FileIndexerTest {


    public static void startIndexing(File[] roots) {
        final int BOUND = 1000;
        final int N_CONSUMERS = 10;
        BlockingQueue<File> queue = new LinkedBlockingQueue<>(BOUND);
        FileFilter filter = (File file) -> true;
        for (File root : roots)
            new Thread(new FileCrawler(queue, filter, root)).start();
        for (int i = 0; i < N_CONSUMERS; i++)
            new Thread(new Indexer(queue)).start();
    }

    public static void main(String[] args) {
        File dir = new File("/home/klose/Movie");
        startIndexing(dir.listFiles((File pathname) -> {
            if (pathname.isDirectory()) {
                return true;
            }
            return false;
        }));
    }
}

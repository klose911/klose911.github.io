package org.klose.concurrency.synchronizer.blockingQueue;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.concurrent.BlockingQueue;

public class Indexer implements Runnable {
    private final BlockingQueue<File> queue;
    private final static Logger logger = LoggerFactory.getLogger(Indexer.class);

    public Indexer(BlockingQueue<File> queue) {
        this.queue = queue;
    }

    public void run() {
        try {
            while (true)
                indexFile(queue.take());
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    private void indexFile(File take) {
        logger.info(take + ": 已进行索引 by " + Thread.currentThread().getName());
    }
}
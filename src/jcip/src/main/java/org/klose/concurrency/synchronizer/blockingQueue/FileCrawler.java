package org.klose.concurrency.synchronizer.blockingQueue;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileFilter;
import java.util.concurrent.BlockingQueue;

public class FileCrawler implements Runnable {
    private final BlockingQueue<File> fileQueue;
    private final FileFilter fileFilter;
    private final File root;
    private final static Logger logger = LoggerFactory.getLogger(FileCrawler.class);

    public FileCrawler(BlockingQueue<File> fileQueue, FileFilter fileFilter, File root) {
        this.fileQueue = fileQueue;
        this.fileFilter = fileFilter;
        this.root = root;
    }

    public void run() {
        try {
            crawl(root);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    private void crawl(File root) throws InterruptedException {
        File[] entries = root.listFiles(fileFilter);
        if (entries != null) {
            for (File entry : entries)
                if (entry.isDirectory())
                    crawl(entry);
                else if (!alreadyIndexed(entry)) {
                    // 向队列中添加文件, 如果队列是BOUND的, 且队列已满, 则put方法将阻塞, 直到队列不满
                    logger.info(entry + ": 等待进行索引 by " + Thread.currentThread().getName());
                    fileQueue.put(entry);
                }

        }
    }

    private boolean alreadyIndexed(File entry) {
        return false;
    }
}
package org.klose.concurrency.visibility.thisEscape;

import java.util.ArrayList;
import java.util.List;


public class EventSource<T> {
    private final List<T> eventListeners;

    public EventSource() {
        eventListeners = new ArrayList<T>();
    }

    public synchronized void registerListener(T eventListener) {
        this.eventListeners.add(eventListener);
        this.notifyAll();
    }

    public synchronized List<T> retrieveListeners() throws InterruptedException {
        List<T> dest = null;
        if (eventListeners.size() <= 0) {
            this.wait();
        }
        dest = new ArrayList<T>(eventListeners.size());
        dest.addAll(eventListeners);
        return dest;
    }
}

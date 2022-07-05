package org.klose.concurrency.visibility.thisEscape;

import java.util.List;


public class ListenerRunnable implements Runnable {

    private EventSource<EventListener> source;

    public ListenerRunnable(EventSource<EventListener> source) {
        this.source = source;
    }

    public void run() {
        List<EventListener> listeners = null;

        try {
            listeners = this.source.retrieveListeners();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        for (EventListener listener : listeners) {
            listener.onEvent(new Object());
        }
    }

}

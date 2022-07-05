package org.klose.concurrency.raceCondition;

import net.jcip.annotations.NotThreadSafe;

@NotThreadSafe
public class LazyInitRace {
    private LazyInitRace instance = null;

    public LazyInitRace getInstance() {
        if (instance == null)
            instance = new LazyInitRace();
        return instance;
    }
}

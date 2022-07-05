package org.klose.concurrency.composing.delegation;


import java.util.concurrent.atomic.AtomicInteger;

public class NumberRange {
    //对于多个状态变量参与的不变式，即使每个变量都是原子，仍无法保证不变式恒等
    // INVARIANT: lower <= upper
    private final AtomicInteger lower = new AtomicInteger(0);
    private final AtomicInteger upper = new AtomicInteger(0);

    public void setLower(int i) {
        // Warning -- unsafe check-then-act
        if (i > upper.get())
            throw new IllegalArgumentException(
                    "can’t set lower to " + i + " > upper");
        lower.set(i);
    }

    public void setUpper(int i) {
        // Warning -- unsafe check-then-act
        if (i < lower.get())
            throw new IllegalArgumentException(
                    "can’t set upper to " + i + " < lower");
        upper.set(i);
    }


    public boolean isInRange(int i) {
        return (i >= lower.get() && i <= upper.get());
    }
}

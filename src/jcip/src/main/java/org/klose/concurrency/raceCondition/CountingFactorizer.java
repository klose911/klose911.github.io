package org.klose.concurrency.raceCondition;

import net.jcip.annotations.ThreadSafe;

import javax.servlet.*;
import java.math.BigInteger;
import java.util.concurrent.atomic.AtomicLong;

@ThreadSafe
public abstract class CountingFactorizer implements Servlet {

    private final AtomicLong count = new AtomicLong(0);

    public long getCount() {
        return count.get();
    }

    public void service(ServletRequest req, ServletResponse resp) {
        BigInteger i = extractFromRequest(req);
        BigInteger[] factors = factor(i);
        count.incrementAndGet();
        encodeIntoResponse(resp, factors);
    }


    private void encodeIntoResponse(ServletResponse resp, BigInteger[] factors) {
    }

    private BigInteger[] factor(BigInteger i) {
        return null;
    }

    private BigInteger extractFromRequest(ServletRequest req) {
        return new BigInteger(req.getParameter("number"));
    }

}
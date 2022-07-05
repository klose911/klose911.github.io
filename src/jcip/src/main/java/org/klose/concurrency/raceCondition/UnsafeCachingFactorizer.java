package org.klose.concurrency.raceCondition;


import net.jcip.annotations.NotThreadSafe;

import javax.servlet.Servlet;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import java.math.BigInteger;
import java.util.concurrent.atomic.AtomicReference;

@NotThreadSafe
public abstract class UnsafeCachingFactorizer implements Servlet {
    private final AtomicReference<BigInteger> lastNumber
            = new AtomicReference<BigInteger>();

    private final AtomicReference<BigInteger[]> lastFactors
            = new AtomicReference<BigInteger[]>();

    public void service(ServletRequest req, ServletResponse resp) {
        BigInteger i = extractFromRequest(req);
        if (i.equals(lastNumber.get()))
            encodeIntoResponse(resp, lastFactors.get());
        else {
            BigInteger[] factors = factor(i);
            lastNumber.set(i);
            lastFactors.set(factors);
            encodeIntoResponse(resp, factors);
        }
    }

    protected abstract BigInteger[] factor(BigInteger i);

    protected abstract void encodeIntoResponse(ServletResponse resp, BigInteger[] bigIntegers);

    protected abstract BigInteger extractFromRequest(ServletRequest req);
}
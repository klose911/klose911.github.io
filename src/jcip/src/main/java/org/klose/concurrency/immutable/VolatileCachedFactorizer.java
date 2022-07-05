package org.klose.concurrency.immutable;

import net.jcip.annotations.Immutable;
import net.jcip.annotations.ThreadSafe;

import javax.servlet.Servlet;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import java.math.BigInteger;
import java.util.Arrays;

@ThreadSafe
public abstract class VolatileCachedFactorizer implements Servlet {
    private volatile OneValueCache cache =
            new OneValueCache(null, null);

    public void service(ServletRequest req, ServletResponse resp) {
        BigInteger i = extractFromRequest(req);
        BigInteger[] factors = cache.getFactors(i);
        if (factors == null) {
            factors = factor(i);
            cache = new OneValueCache(i, factors);
        }
        encodeIntoResponse(resp, factors);
    }

    protected abstract void encodeIntoResponse(ServletResponse resp, BigInteger[] factors);

    protected abstract BigInteger[] factor(BigInteger i);

    protected abstract BigInteger extractFromRequest(ServletRequest req);
}

@Immutable
class OneValueCache {
    private final BigInteger lastNumber;
    private final BigInteger[] lastFactors;

    public OneValueCache(BigInteger i,
                         BigInteger[] factors) {
        lastNumber = i;
        lastFactors = Arrays.copyOf(factors, factors.length);
    }

    public BigInteger[] getFactors(BigInteger i) {
        if (lastNumber == null || !lastNumber.equals(i))
            return null;
        else
            return Arrays.copyOf(lastFactors, lastFactors.length);
    }
}
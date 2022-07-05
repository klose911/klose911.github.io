/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.synchronizer.cache;

import java.math.BigInteger;
import javax.servlet.Servlet;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import net.jcip.annotations.ThreadSafe;

@ThreadSafe
public abstract class Factorizer implements Servlet {

    private final Computable<BigInteger, BigInteger[]> c;
    private final Computable<BigInteger, BigInteger[]> cache;

    public Factorizer() {
        c = (BigInteger arg) -> factor(arg);
        cache = new Memoizer<>(c);
    }

    private BigInteger[] factor(BigInteger arg) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void service(ServletRequest req,
            ServletResponse resp) {
        try {
            BigInteger i = extractFromRequest(req);
            encodeIntoResponse(resp, cache.compute(i));
        } catch (InterruptedException e) {
            encodeError(resp, "factorization interrupted");
        }
    }

    private BigInteger extractFromRequest(ServletRequest req) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    private void encodeIntoResponse(ServletResponse resp, BigInteger[] compute) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    private void encodeError(ServletResponse resp, String factorization_interrupted) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}

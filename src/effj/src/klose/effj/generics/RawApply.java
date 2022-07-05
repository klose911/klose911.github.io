/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.generics;

import java.util.List;

/**
 *
 * @author klose
 */
public class RawApply {

    /**
     *
     * @param list
     * @param f
     * @param initVal
     * @return
     */
    // Reduction without generics, and with concurrency flaw!
    public static Object reduce(List list, RawFunction f, Object initVal) {
        synchronized (list) {
            Object result = initVal;
            for (Object o : list) {
                result = f.apply(result, o);
            }
            return result;
        }
    }
}

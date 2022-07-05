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
public class BadGenericApply {

    // Reduction without generics or concurrency flaw
    public static Object rawReduce(List list, GenericFunction f, Object initVal) {
        // Locks list internally
        Object[] snapshot = list.toArray();
        Object result = initVal;
        for (Object o : list) {
            result = f.apply(result, o);
        }
        return result;
    }

    public static <E> E uglyReduce(List<E> list, GenericFunction<E> f, E initVal) {
        // Naive generic version of reduction - won't compile!
        //E[] snapshot = list.toArray();
        E[] snapshot = (E[]) list.toArray(); // Locks list
        E result = initVal;
        for (E e : snapshot) {
            result = f.apply(result, e);
        }
        return result;
    }
}

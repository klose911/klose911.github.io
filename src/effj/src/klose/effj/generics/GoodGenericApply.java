/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.generics;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author klose
 */
public class GoodGenericApply {

    public static <E> E reduce(List<E> list, GenericFunction<E> f, E initVal) {
        // List-based generic reduction
        List<E> snapshot;
        synchronized (list) {
            snapshot = new ArrayList<>(list);
        }
        E result = initVal;
        for (E e : snapshot) {
            result = f.apply(result, e);
        }
        return result;
    }
}

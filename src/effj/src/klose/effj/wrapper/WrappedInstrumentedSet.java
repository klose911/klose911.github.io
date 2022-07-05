/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.wrapper;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

/**
 *
 * @param <E>
 */
public class WrappedInstrumentedSet<E> {

    private final Set<E> s;
    private int addCount = 0;

    public WrappedInstrumentedSet(Set<E> s) {
        this.s = s;
    }

    public boolean add(E e) {
        addCount++;
        return s.add(e);
    }

    public boolean addAll(Collection<? extends E> c) {
        addCount += c.size();
        return s.addAll(c);
    }

    public int getAddCount() {
        return addCount;
    }

    public static void main(String[] args) {
        WrappedInstrumentedSet<String> s
                = new WrappedInstrumentedSet<>(new HashSet<>());
        s.addAll(Arrays.asList("Snap", "Crackle", "Pop"));
        //the element number of instrumented set is 3
        System.out.println(
                String.format("the element number of wrapped instrumented set is %d", s.getAddCount()));
    }
}

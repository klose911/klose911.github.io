/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.wrapper;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;

// Broken - Inappropriate use of inheritance!
public class InstrumentedHashSet<E> extends HashSet<E> {
// The number of attempted element insertions

    private int addCount = 0;

    public InstrumentedHashSet() {
    }

    public InstrumentedHashSet(int initCap, float loadFactor) {
        super(initCap, loadFactor);
    }

    @Override
    public boolean add(E e) {
        addCount++;
        return super.add(e);
    }

    @Override
    public boolean addAll(Collection<? extends E> c) {
        addCount += c.size();
        return super.addAll(c);
    }

    public int getAddCount() {
        return addCount;
    }

    public static void main(String[] args) {
        InstrumentedHashSet<String> s
                = new InstrumentedHashSet<>();
        s.addAll(Arrays.asList("Snap", "Crackle", "Pop"));
        //it should be 3, but is 6 !
        //the InstrumentedHashSet.addAll -> HashSet.add -> InstrumentedHashSet.add 
        System.out.println(
                String.format("the element number of instrumented hash set is %d", s.getAddCount()));
    }
}

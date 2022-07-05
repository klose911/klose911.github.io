/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.concurrency.observer;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 *
 * @author klose
 * @param <E>
 */
public class ConcurrentObservableSet<E> extends ForwardingSet<E> {

    public ConcurrentObservableSet(Set<E> s) {
        super(s);
    }

    private final List<SetObserver<E>> observers
            = new CopyOnWriteArrayList<>();

    public void addObserver(SetObserver<E> observer) {
            observers.add(observer);
    }

    public boolean removeObserver(SetObserver<E> observer) {
            return observers.remove(observer);
    }

    private void notifyElementAdded(E element) {
        observers.forEach((observer) -> {
            observer.added(this, element);
        });
    }

    @Override
    public boolean add(E element) {
        boolean added = super.add(element);
        if (added) {
            notifyElementAdded(element);
        }
        return added;
    }

    @Override
    public boolean addAll(Collection<? extends E> c) {
        boolean result = false;
        // calls notifyElementAdded
        for (E element : c) {
            result |= add(element);
        }
        return result;
    }
}

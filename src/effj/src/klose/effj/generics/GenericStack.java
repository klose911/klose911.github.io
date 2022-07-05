/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.generics;

import java.util.Arrays;
import java.util.Collection;
import java.util.EmptyStackException;

/**
 *
 * @author klose
 * @param <E>
 */
public class GenericStack<E> {

    private Object[] elements;
    private int size = 0;
    private static final int DEFAULT_INITIAL_CAPACITY = 16;

    public GenericStack() {
        elements = new Object[DEFAULT_INITIAL_CAPACITY];
    }

    public void push(E e) {
        ensureCapacity();
        elements[size++] = e;
    }

    public E pop() {
        if (size == 0) {
            throw new EmptyStackException();
        }
        E result = (E) elements[--size];
        elements[size] = null; // Eliminate obsolete reference
        return result;
    }

    private void ensureCapacity() {
        if (elements.length == size) {
            elements = Arrays.copyOf(elements, 2 * size + 1);
        }
    }

    // Wildcard type for parameter that serves as an E producer
    public void pushAll(Iterable<? extends E> src) {
        for (E e : src) {
            push(e);
        }
    }

    // Wildcard type for parameter that serves as an E consumer
    public void popAll(Collection<? super E> dst) {
        while (elements.length > 0) {
            dst.add(pop());
        }
    }
}

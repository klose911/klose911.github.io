/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.clone;

import java.util.Arrays;
import java.util.EmptyStackException;

public class Stack implements Cloneable {

    private Object[] elements;
    private int size = 0;
    private static final int DEFAULT_INITIAL_CAPACITY = 16;

    public Stack() {
        this.elements = new Object[DEFAULT_INITIAL_CAPACITY];
    }

    public void push(Object e) {
        ensureCapacity();
        elements[size++] = e;
    }

    public Object pop() {
        if (size == 0) {
            throw new EmptyStackException();
        }
        Object result = elements[--size];
        elements[size] = null; // Eliminate obsolete reference
        return result;
    }
// Ensure space for at least one more element.

    private void ensureCapacity() {
        if (elements.length == size) {
            elements = Arrays.copyOf(elements, 2 * size + 1);
        }
    }

    @Override
    public Stack clone() throws CloneNotSupportedException {
        try {
            return (Stack) super.clone();
        } catch (CloneNotSupportedException ex) {
            throw new AssertionError(); // Can't happen
        }
    }
    
    public static void main(String [] args) throws CloneNotSupportedException {
        Stack s = new Stack(); 
        s.push(100);
        s.push(200);
        s.push(300); 
        
        Stack s1 = s.clone(); 
        s1.pop();
        for(Object o: s.elements) 
            System.out.println(o);
        
        if(s.elements == s1.elements) 
            System.err.println("pity");
    }
}

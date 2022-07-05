/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.generics.heterogeneous;

/**
 *
 * @author klose
 * @param <T>
 */
public class Column<T> {

    private final T valClass;

    @SuppressWarnings("unchecked")
    public Column(Class<T> valClass) {
        this.valClass = (T) valClass;
    }

    @SuppressWarnings("unchecked")
    public T cast(Object obj) {
        return obj == null ? null : ((Class<T>) valClass).cast(obj);
    }
}

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.concurrency.observer;

/**
 *
 * @author klose
 * @param <E>
 */
public interface SetObserver<E> {

    // Invoked when an element is added to the observable set
    void added(
            ObservableSet<E> set, E element);

    // Invoked when an element is added to the Broken observable set
    void added(
            BrokenObservableSet<E> set, E element);

    // Invoked when an element is added to the concurrent observable set
    void added(
            ConcurrentObservableSet<E> set, E element);
}

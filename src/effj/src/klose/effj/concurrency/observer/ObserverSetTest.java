/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.concurrency.observer;

import java.util.HashSet;

/**
 *
 * @author klose
 */
public class ObserverSetTest {

    public static void main(String[] args) {
        ObservableSet<Integer> set
                = new ObservableSet<>(new HashSet<>());

        set.addObserver(new SetObserver<Integer>() {
            @Override
            public void added(ObservableSet<Integer> s, Integer e) {
                System.out.println(e);
                if (e == 23) {
                    s.removeObserver(this);
                }
            }
            
            @Override
            public void added(BrokenObservableSet<Integer> set, Integer element) {
                throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
            }

            @Override
            public void added(ConcurrentObservableSet<Integer> set, Integer element) {
                throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
            }
        });

        for (int i = 0; i < 100; i++) {
            set.add(i);
        }
    }
}

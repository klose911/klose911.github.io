/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.capability;


public class ThreadLocalWithStaticClass {

    // Noninstantiable
    // (Capability)
    private ThreadLocalWithStaticClass() {
    }

    public static class Key {

        Key() {
        }
    }

    // Generates a unique, unforgeable key
    public static Key getKey() {
        return new Key();
    }

    public static void set(Key key, Object value) {

    }

    public static Object get(Key key) {
        throw new UnsupportedOperationException();
    }
}

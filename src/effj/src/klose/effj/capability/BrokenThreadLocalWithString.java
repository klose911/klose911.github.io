/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.capability;

public class BrokenThreadLocalWithString {

    private BrokenThreadLocalWithString() {
    } // Noninstantiable

    // Sets the current thread's value for the named variable.
    public static void set(String key, Object value) {

    }

    // Returns the current thread's value for the named variable.
    public static Object get(String key) {
        throw new UnsupportedOperationException();
    }
}

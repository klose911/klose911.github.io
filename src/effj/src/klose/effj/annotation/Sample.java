/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.annotation;

// Program containing marker annotations
public class Sample {

    @Test
    public static void m1() {
        // Test should pass
    }

    public static void m2() {
    }

    @Test
    public static void m3() {
        throw new RuntimeException("Boom"); //test should fail 
    }

    public static void m4() {
    }

    @Test
    public void m5() {
        // INVALID USE: nonstatic method
    }

    public static void m6() {
    }

    @Test
    public static void m7() {
        // Test should fail 
        throw new RuntimeException("Crash");
    }

    public static void m8() {
    }
}

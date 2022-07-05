/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.annotation;

// Program containing annotations with a parameter
public class SampleWithException {

    @ExceptionTest(ArithmeticException.class)
    // Test should pass
    public static void m1() { 
        int i = 0;
        i = i / i;
    }

    @ExceptionTest(ArithmeticException.class)
    // Should fail (wrong exception)
    public static void m2() { 
        int[] a = new int[0];
        int i = a[1];
        
    }

    @ExceptionTest(ArithmeticException.class)
    // Should fail (no exception)
    public static void m3() {
    } 
}

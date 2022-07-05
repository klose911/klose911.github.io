/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.annotation;

import java.util.ArrayList;
import java.util.List;

public class SampleWithExceptions {
    // Code containing an annotation with an array parameter
    @ExceptionsTest({IndexOutOfBoundsException.class, 
        NullPointerException.class})
    public static void doublyBad() {
        List<String> list = new ArrayList<String>();
        // The spec permits this method to throw either
        // IndexOutOfBoundsException or NullPointerException
        list.addAll(5, null);
    }
}
    
/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.strategy;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Comparator;

/**
 *
 * @author klose
 */
public class StaticMemeberStrLengComparatorTest {

    private static class StrLenCmp
            implements Comparator<String>, Serializable {

        @Override
        public int compare(String s1, String s2) {
            return s1.length() - s2.length();
        }
    }

    // Returned comparator is serializable
    public static final Comparator<String> STRING_LENGTH_COMPARATOR = new StrLenCmp();

    public static void main(String[] args) {
        String[] stringArray = {"david", "john", "michael", "frank"};
        Arrays.sort(stringArray, STRING_LENGTH_COMPARATOR);
        Arrays.asList(stringArray).forEach(n -> System.out.println(n));
    }
}

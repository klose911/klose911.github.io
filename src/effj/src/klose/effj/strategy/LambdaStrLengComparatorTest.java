/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.strategy;

import java.util.Arrays;

/**
 *
 * @author klose
 */
public class LambdaStrLengComparatorTest {

    public static void main(String[] args) {
        String[] stringArray = {"david", "john", "michael", "frank"};
        Arrays.sort(stringArray, (String o1, String o2) -> {
            return o1.length() - o2.length();
        });

        Arrays.asList(stringArray).forEach(n -> System.out.println(n));
    }
}

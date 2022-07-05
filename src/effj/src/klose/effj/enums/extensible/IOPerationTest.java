/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.enums.extensible;

import java.util.Arrays;
import java.util.Collection;

/**
 *
 * @author klose
 */
public class IOPerationTest {

    private static <T extends Enum<T> & IOperation> void test(
            Class<T> opSet, double x, double y) {
        for (IOperation op : opSet.getEnumConstants()) {
            System.out.printf("%f %s %f = %f%n",
                    x, op, y, op.apply(x, y));
        }
    }

    private static void test(Collection<? extends IOperation> opSet,
            double x, double y) {
        opSet.forEach((op) -> {
            System.out.printf("%f %s %f = %f%n",
                    x, op, y, op.apply(x, y));
        });
    }

    public static void main(String[] args) {
        double x = 5.0;
        double y = 3.0;
        //5.000000 ^ 3.000000 = 125.000000
        //5.000000 % 3.000000 = 2.000000
        test(ExtendedOperation.class, x, y); 
         //5.000000 ^ 3.000000 = 125.000000
        //5.000000 % 3.000000 = 2.000000       
        test(Arrays.asList(ExtendedOperation.values()), x, y);
    }

}

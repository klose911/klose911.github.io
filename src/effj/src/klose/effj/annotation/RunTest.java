/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.annotation;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 *
 * @author klose
 */
public class RunTest {

    public static void main(String[] args) throws Exception {
        int tests = 0;
        int passed = 0;
         //        Class testClass = Class.forName(args[0]);
        Class testClass = Class.forName("klose.effj.annotation.Sample");
        for (Method m : testClass.getDeclaredMethods()) {
            //通过反射获取＠Test的方法
            if (m.isAnnotationPresent(Test.class)) {
                tests++;
                try {
                    //调用测试方法
                    m.invoke(null);
                    passed++;
                } catch (InvocationTargetException wrappedExc) {
                    Throwable exc = wrappedExc.getCause();
                    System.out.println(m + " failed: " + exc);
                } catch (Exception exc) {
                    System.out.println("INVALID @Test: " + m);
                }
            }
        }
        System.out.printf("Passed: %d, Failed: %d%n",
                passed, tests - passed);
    }
}

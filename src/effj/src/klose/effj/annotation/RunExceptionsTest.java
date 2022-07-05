/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.annotation;

import java.lang.reflect.Method;

public class RunExceptionsTest {

    public static void main(String[] args) throws Exception {
        int tests = 0;
        int passed = 0;
        //Class testClass = Class.forName(args[0]);
        Class testClass = Class.forName("klose.effj.annotation.SampleWithExceptions");
        for (Method m : testClass.getDeclaredMethods()) {
            if (m.isAnnotationPresent(ExceptionsTest.class)) {
                tests++;
                try {
                    m.invoke(null);
                    System.out.printf("Test %s failed: no exception%n", m);
                } catch (Throwable wrappedExc) {
                    Throwable exc = wrappedExc.getCause();
                    Class<? extends Exception>[] excTypes
                            = m.getAnnotation(ExceptionsTest.class).value();
                    int oldPassed = passed;
                    for (Class<? extends Exception> excType : excTypes) {
                        if (excType.isInstance(exc)) {
                            passed++;
                            break;
                        }
                    }
                    if (passed == oldPassed) {
                        System.out.printf("Test %s failed: %s %n", m, exc);
                    }
                }
            }
        }
        System.out.printf("Passed: %d, Failed: %d%n",
                passed, tests - passed);
    }
}

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
public class RunExceptionTest {

    public static void main(String[] args) throws Exception {
        int tests = 0;
        int passed = 0;
        //Class testClass = Class.forName(args[0]);
        Class testClass = Class.forName("klose.effj.annotation.SampleWithException");
        for (Method m : testClass.getDeclaredMethods()) {
            if (m.isAnnotationPresent(ExceptionTest.class)) {
                tests++;
                try {
                    m.invoke(null);
                    System.out.printf("Test %s failed: no exception%n", m);
                } catch (InvocationTargetException wrappedEx) {
                    Throwable exc = wrappedEx.getCause();
                    //获取注解中的value值，也就是想要捕捉的异常的class类型，以此与实际捕捉异常的Throwable做比较
                    Class<? extends Exception> excType
                            = m.getAnnotation(ExceptionTest.class).value();
                    if (excType.isInstance(exc)) {
                        passed++;
                    } else {
                        System.out.printf(
                                "Test %s failed: expected %s, got %s%n",
                                m, excType.getName(), exc);
                    }
                } catch (Exception exc) {
                    System.out.println("INVALID @Test: " + m);
                }
            }
        }
        System.out.printf("Passed: %d, Failed: %d%n",
                passed, tests - passed);
    }
}

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.generics;

/**
 *
 * @author klose
 */
public final class IdentityFunction {

    private static interface UnaryFunction<T> {

        T apply(T arg);
    }

    private IdentityFunction() {
    }

    // Generic singleton factory pattern
    private static final UnaryFunction<Object> IDENTITY_FUNCTION
            = new UnaryFunction<Object>() {
        @Override
        public Object apply(Object arg) {
            return arg;
        }
    };

    @SuppressWarnings("unchecked")
    public static <T> UnaryFunction<T> identityFunction() {
        // IDENTITY_FUNCTION is stateless and its type parameter is
        // unbounded so it's safe to share one instance across all types.
        return (UnaryFunction<T>) IDENTITY_FUNCTION;
    }

    public static void main(String[] args) {
        String[] strings = {"jute", "hemp", "nylon"};
        UnaryFunction<String> sameString = identityFunction();
        for (String s : strings) {
            System.out.println(sameString.apply(s));
        }
        Number[] numbers = {1, 2.0, 3L};
        UnaryFunction<Number> sameNumber = identityFunction();
        for (Number n : numbers) {
            System.out.println(sameNumber.apply(n));
        }
    }
}

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.concurrency.lazy;

public class StaticFieldLazyInitializer {

    private static class FieldHolder {

        private static final FieldType FIELD = computeFieldValue();
    }

    // Lazy initialization holder class idiom for static fields
    public static FieldType getField3() {
        return FieldHolder.FIELD;
    }

    private static FieldType computeFieldValue() {
        return new FieldType();
    }
}

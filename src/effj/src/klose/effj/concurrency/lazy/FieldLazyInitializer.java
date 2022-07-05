/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.concurrency.lazy;

public class FieldLazyInitializer {
    // Lazy initialization of instance field - synchronized accessor
    private FieldType field2;

    public synchronized FieldType getField2() {
        if (field2 == null) {
            field2 = computeFieldValue();
        }
        return field2;
    }

    private static FieldType computeFieldValue() {
        return new FieldType();
    }
}

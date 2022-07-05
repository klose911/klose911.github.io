/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.concurrency.lazy;

public class DoubleCheckFieldLazyInitializer {

    // Double-check idiom for lazy initialization of instance fields
    private volatile FieldType field4;

    public FieldType getField4() {
        FieldType result = field4;
        if (result == null) { // First check (no locking)
            synchronized (this) {
                result = field4;
                if (result == null) { // Second check (with locking)
                    field4 = result = computeFieldValue();
                }
            }
        }
        return result;
    }

    private static FieldType computeFieldValue() {
        return new FieldType();
    }
}

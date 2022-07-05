/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.enums.extensible;

/**
 *
 * @author klose
 */
// Emulated extension enum
public enum ExtendedOperation implements IOperation {
    EXP("^") {
        @Override
        public double apply(double x, double y) {
            return Math.pow(x, y);
        }
    },
    REMAINDER("%") {
        @Override
        public double apply(double x, double y) {
            return x % y;
        }
    };
    private final String symbol;

    private ExtendedOperation(String symbol) {
        this.symbol = symbol;
    }

    @Override
    public String toString() {
        return symbol;
    }
}

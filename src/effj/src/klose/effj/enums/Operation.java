/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.enums;

import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author klose
 */
public enum Operation {

    PLUS("+") {
        @Override
        double apply(double x, double y) {
            return x + y;
        }
    },
    MINUS("-") {
        @Override
        double apply(double x, double y) {
            return x - y;
        }
    },
    TIMES("*") {
        @Override
        double apply(double x, double y) {
            return x * y;
        }
    },
    DIVIDE("/") {
        @Override
        double apply(double x, double y) {
            return x / y;
        }
    };

    private final String symbol;

    abstract double apply(double x, double y);

    private static final Map<String, Operation> stringToEnum = 
            new HashMap<String, Operation>();

    static {
        for (Operation op : values()) {
            stringToEnum.put(op.toString(), op);
        }
    }

    public static Operation fromString(String symbol) {
        return stringToEnum.get(symbol);
    }

    private Operation(String symbol) {
        this.symbol = symbol;
    }

    @Override
    public String toString() {
        return symbol;
    }

    public static void main(String[] args) {
        String[] arg = {"2", "4"};
        //2.000000 + 4.000000 = 6.000000
        //2.000000 - 4.000000 = -2.000000
        //2.000000 * 4.000000 = 8.000000
        //2.000000 / 4.000000 = 0.500000
        double x = Double.parseDouble(arg[0]);
        double y = Double.parseDouble(arg[1]);
        for (Operation op : Operation.values()) {
            System.out.printf("%f %s %f = %f%n", x, op, y, op.apply(x, y));
        }
    }
}

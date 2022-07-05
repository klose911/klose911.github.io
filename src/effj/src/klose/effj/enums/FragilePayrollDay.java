/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.enums;

/**
 *
 * @author klose
 */
public enum FragilePayrollDay {

    MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY,
        SATURDAY, SUNDAY;

    private static final int HOURS_PER_SHIFT = 8;

    double pay(double hourseWorked, double payRate) {
        double basePay = hourseWorked * payRate;
        double overtimePay;
        switch (this) {
            case SATURDAY:
            case SUNDAY:
                overtimePay = hourseWorked * payRate / 2;
                break;
            default:
                overtimePay = hourseWorked <= HOURS_PER_SHIFT ? 
                        0 : (hourseWorked - HOURS_PER_SHIFT) * payRate / 2;
                break;
        }
        return basePay + overtimePay;
    }
}

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
public enum UglyPayrollDay {
    MONDAY() {
        @Override
        double overtimePay(double hoursWorked, double payRate) {
            return weekdayPay(hoursWorked, payRate);
        }
    },
    TUESDAY {
        @Override
        double overtimePay(double hoursWorked, double payRate) {
            return weekdayPay(hoursWorked, payRate);
        }
    },
    WEDNESDAY {
        @Override
        double overtimePay(double hoursWorked, double payRate) {
            return weekdayPay(hoursWorked, payRate);
        }
    },
    THURSDAY {
        @Override
        double overtimePay(double hoursWorked, double payRate) {
            return weekdayPay(hoursWorked, payRate);
        }
    },
    FRIDAY {
        @Override
        double overtimePay(double hoursWorked, double payRate) {
            return weekdayPay(hoursWorked, payRate);
        }
    },
    SATURDAY {
        @Override
        double overtimePay(double hoursWorked, double payRate) {
            return weekendPay(hoursWorked, payRate);
        }
    },
    SUNDAY {
        @Override
        double overtimePay(double hoursWorked, double payRate) {
            return weekendPay(hoursWorked, payRate);
        }
    };
    private static final int HOURS_PER_SHIFT = 8;//正常工作时数

    //抽象出加班工资计算
    abstract double overtimePay(double hoursWorked, double payRate);

    //计算工资
    double pay(double hoursWorked, double payRate) {
        double basePay = hoursWorked * payRate;//公用
        return basePay + overtimePay(hoursWorked, payRate);
    }

    //双休日加班工资算法
    double weekendPay(double hoursWorked, double payRate) {
        return hoursWorked * payRate / 2;
    }

    //正常工作日加班工资
    double weekdayPay(double hoursWorked, double payRate) {
        return hoursWorked <= HOURS_PER_SHIFT ? 0
                : (hoursWorked - HOURS_PER_SHIFT) * payRate / 2;
    }
}

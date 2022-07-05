/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.defensive.copy;

import java.util.Date;

/**
 *
 * @author klose
 */
public final class Period {

    private final Date start;
    private final Date end;

    /**
     * @param start the beginning of the period
     * @param end the end of the period; must not precede start
     * @throws IllegalArgumentException if start is after end
     * @throws NullPointerException if start or end is null
     */
    public Period(Date start, Date end) {
        Date startVal = new Date(start.getTime());
        Date endVal = new Date(end.getTime());

        if (startVal.compareTo(endVal) > 0) {
            throw new IllegalArgumentException(
                    startVal + " after " + endVal);
        }
        this.start = startVal;
        this.end = endVal;
    }

    public Date start() {
        return new Date(start.getTime());
    }

    public Date end() {
        return new Date(end.getTime());
    }

    public static void main(String[] args) {
        Date start = new Date();
        Date end = new Date();
        Period p = new Period(start, end);
        System.out.println(p.end());
        end.setYear(78);
        p.end().setYear(78);
        System.out.println(end);
        System.out.println(p);
    }

    @Override
    public String toString() {
        return "Period{" + "start=" + start + ", end=" + end + '}';
    }
    
    
}

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.defensive.copy;

import java.util.Date;

// Broken "immutable" time period class
public class BrokenPeriod {

    private final Date start;
    private final Date end;

    /**
     * @param start the beginning of the period
     * @param end the end of the period; must not precede start
     * @throws IllegalArgumentException if start is after end
     * @throws NullPointerException if start or end is null
     */
    public BrokenPeriod(Date start, Date end) {
        if (start.compareTo(end) > 0) {
            throw new IllegalArgumentException(
                    start + " after " + end);
        }
        this.start = start;
        this.end = end;
    }

    public Date start() {
        return start;
    }

    public Date end() {
        return end;
    }

    public static void main(String[] args) {
        // Attack the internals of a Period instance
        Date start = new Date();
        Date end = new Date();
        BrokenPeriod p = new BrokenPeriod(start, end);
        System.out.println(p.end());
        
        end.setYear(78); // Modifies internals of p!
        System.out.println(p.end());
    }
}

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.currency;

import java.math.BigDecimal;

/**
 *
 * @author klose
 */
public class CurrencyCalWithBigDecimal {

    public static void main(String[] args) {
        final BigDecimal TEN_CENTS = new BigDecimal(".10");
        int itemsBought = 0;
        BigDecimal funds = new BigDecimal("1.00");
        for (BigDecimal price = TEN_CENTS;
                funds.compareTo(price) >= 0;
                price = price.add(TEN_CENTS)) {
            itemsBought++;
            funds = funds.subtract(price);
        }
        
        //4 items bought.
        //Money left: $0.00
        System.out.println(itemsBought + " items bought.");
        System.out.println("Money left over: $" + funds);
    }
}

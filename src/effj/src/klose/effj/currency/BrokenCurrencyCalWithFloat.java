/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.currency;

public class BrokenCurrencyCalWithFloat {

    // Broken - uses floating point for monetary calculation!
    public static void main(String[] args) {
        //0.610000001
        System.out.println(1.03 - .42);
        //0.0999999998
        System.out.println(1.00 - 9 * .10);

        double funds = 1.00;
        int itemsBought = 0;
        for (double price = .10; funds >= price; price += .10) {
            funds -= price;
            itemsBought++;
        }
        //3 items bought.
        //Change: $0.39999999
        System.out.println(itemsBought + " items bought.");
        System.out.println("Change: $" + funds);
    }
}

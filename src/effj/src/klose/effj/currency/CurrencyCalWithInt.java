/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.currency;

public class CurrencyCalWithInt {

    public static void main(String[] args) {
        int itemsBought = 0;
        int funds = 100;
        for (int price = 10; funds >= price; price += 10) {
            itemsBought++;
            funds -= price;
        }
        //4 items bought. 
        //Money left over: 0 cents
        System.out.println(itemsBought + " items bought.");
        System.out.println("Money left over: " + funds + " cents");
    }

}

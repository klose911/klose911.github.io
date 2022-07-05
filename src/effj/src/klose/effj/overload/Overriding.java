/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.overload;

public class Overriding {

    public static void main(String[] args) {
        Wine[] wines = {
            new Wine(), new SparklingWine(), new Champagne()
        };
        
        //wine
        //sparkling wine
        //champagne
        for (Wine wine : wines) {
            System.out.println(wine.name());
        }
    }
}

class Wine {

    String name() {
        return "wine";
    }
}

class SparklingWine extends Wine {

    @Override
    String name() {
        return "sparkling wine";
    }
}

class Champagne extends SparklingWine {

    @Override
    String name() {
        return "champagne";
    }
}

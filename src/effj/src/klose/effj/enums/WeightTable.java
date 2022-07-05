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
public class WeightTable {

    public static void main(String[] args) {
        double earthWeight = Double.parseDouble("175");
        double mass = earthWeight / Planet.EARTH.surfaceGravity();
        //Weight on MERCURY is 66.133672
        //Weight on VENUS is 158.383926
        //Weight on EARTH is 175.000000
        //Weight on MARS is 66.430699
        //Weight on JUPITER is 442.693902
        //Weight on SATURN is 186.464970
        //Weight on URANUS is 158.349709
        //Weight on NEPTUNE is 198.846116
        for (Planet p : Planet.values()) {
            System.out.printf("Weight on %s is %f%n", p, p.surfaceWeight(mass));
        }
    }
}

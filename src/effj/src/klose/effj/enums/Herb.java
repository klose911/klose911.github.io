/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.enums;

import java.util.EnumMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 *
 * @author klose
 */
public class Herb {

    public enum Type {
        ANNUAL, PERENNIAL, BIENNIAL
    }
    private final String name;
    private final Type type;

    Herb(String name, Type type) {
        this.name = name;
        this.type = type;
    }

    @Override
    public String toString() {
        return name;
    }

    // Using ordinal() to index an array - DON'T DO THIS!
    private static void useOrdinalAsArrayIndex() {
        Herb[] garden = {new Herb("Basil", Type.ANNUAL),
            new Herb("Scallion", Type.PERENNIAL), new Herb("Dill", Type.BIENNIAL)};

        // Indexed by Herb.Type.ordinal()
        Set<Herb>[] herbsByType
                = (Set<Herb>[]) new Set[Herb.Type.values().length];

        for (int i = 0; i < herbsByType.length; i++) {
            herbsByType[i] = new HashSet<>();
        }

        for (Herb h : garden) {
            herbsByType[h.type.ordinal()].add(h);
        }

        //ANNUAL: [Basil]
        //PERENNIAL: [Scallion]
        //BIENNIAL: [Dill]
        for (int i = 0; i < herbsByType.length; i++) {
            System.out.printf("%s: %s%n",
                    Herb.Type.values()[i], herbsByType[i]);
        }
    }

    // Using an EnumMap to associate data with an enum
    private static void useEnumMap() {
        Herb[] garden = {new Herb("Basil", Type.ANNUAL),
            new Herb("Scallion", Type.PERENNIAL), new Herb("Dill", Type.BIENNIAL)};
        Map<Herb.Type, Set<Herb>> herbsByType
                = new EnumMap<>(Herb.Type.class);
        for (Herb.Type t : Herb.Type.values()) {
            herbsByType.put(t, new HashSet<>());
        }
        for (Herb h : garden) {
            herbsByType.get(h.type).add(h);
        }

        System.out.println(herbsByType);
    }

    public static void main(String[] args) {
        useEnumMap();
    }
}

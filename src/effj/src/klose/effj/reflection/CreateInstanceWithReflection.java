/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.reflection;

import java.util.Arrays;
import java.util.Set;

public class CreateInstanceWithReflection {
    // Reflective instantiation with interface access

    public static void main(String[] args) {
        // Translate the class name into a Class object
        Class<?> cl = null;
        try {
            cl = Class.forName(args[0]);
        } catch (ClassNotFoundException e) {
            System.err.println("Class not found.");
            System.exit(1);
        }
        
        // Instantiate the class
        Set<String> s = null;
        try {
            s = (Set<String>) cl.newInstance();
        } catch (IllegalAccessException e) {
            System.err.println("Class not accessible.");
            System.exit(1);
        } catch (InstantiationException e) {
            System.err.println("Class not instantiable.");
            System.exit(1);
        }
        
        // Exercise the set
        s.addAll(Arrays.asList(args).subList(1, args.length));
        System.out.println(s);
    }
}

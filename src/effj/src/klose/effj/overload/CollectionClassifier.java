/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.overload;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import static klose.effj.overload.BrokenCollectionClassifier.classify;

/**
 *
 * @author klose
 */
public class CollectionClassifier {

    public static String classify(Collection<?> c) {
        return c instanceof Set ? "Set"
                : c instanceof List ? "List" : "Unknown Collection";
    }
    
        public static void main(String[] args) {
        Collection<?>[] collections = {
            new HashSet<String>(),
            new ArrayList<BigInteger>(),
            new HashMap<String, String>().values()
        };

        //Set
        //List
        //Unknown Collection        
        for (Collection<?> c : collections) {
            System.out.println(classify(c));
        }
    }
}

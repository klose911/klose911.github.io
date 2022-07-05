/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.generics;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

/**
 *
 * @author klose
 */
public class SimpleGenericMethod {
      public static <E> Set<E> union(Set<E> s1, Set<E> s2) {
          Set<E> result = new HashSet<>(s1);
          result.addAll(s2);
          return result;
  }

  // Simple program to exercise generic method
  public static void main(String[] args) {
          Set<String> guys = new HashSet<>(
                  Arrays.asList("Tom", "Dick", "Harry"));
          Set<String> stooges = new HashSet<>(
                  Arrays.asList("Larry", "Moe", "Curly"));
          Set<String> aflCio = union(guys, stooges);
          System.out.println(aflCio);
  }
}

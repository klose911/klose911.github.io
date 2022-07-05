/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.generics;

import java.util.Iterator;
import java.util.List;

/**
 *
 * @author klose
 */
public class RecursiveBound {
      // Returns the maximum value in a list - uses recursive type bound
  public static <T extends Comparable<T>> T max(List<T> list) {
          Iterator<T> i = list.iterator();
          T result = i.next();
          while (i.hasNext()) {
                  T t = i.next();
                  if (t.compareTo(result) > 0)
                          result = t;
          }
          return result;
  }
}

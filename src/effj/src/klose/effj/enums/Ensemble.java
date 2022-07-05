/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.enums;

  // Enum with integer data stored in an instance field
  public enum Ensemble {
          SOLO(1), DUET(2), TRIO(3), QUARTET(4), QUINTET(5),
          SEXTET(6), SEPTET(7), OCTET(8), DOUBLE_QUARTET(8),
          NONET(9), DECTET(10), TRIPLE_QUARTET(12);

          private final int numberOfMusicians;

          private Ensemble(int size) {
                  this.numberOfMusicians = size;
          }

          public int numberOfMusicians() {
                  return numberOfMusicians;
          }
  }

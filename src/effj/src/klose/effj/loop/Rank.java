/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.loop;

enum Rank {
    ACE, DEUCE, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT,
    NINE, TEN, JACK, QUEEN, KING;

    @Override
    public String toString() {
        return "Rank{" + this.name() + '}';
    }
}

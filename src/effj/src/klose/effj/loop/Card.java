/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.loop;

final class Card {

    private final Suit suit;
    private final Rank rank;

    Card(Suit suit, Rank rank) {
        this.suit = suit;
        this.rank = rank;
    }

    @Override
    public String toString() {
        return "Card{" + "suit=" + suit + ", rank=" + rank + '}';
    }

}

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.loop;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

public class FunctionalCardLoop {

    public static void main(String[] args) {
        Collection<Suit> suits = Arrays.asList(Suit.values());
        Collection<Rank> ranks = Arrays.asList(Rank.values());
        List<Card> decks = new ArrayList<>();

        suits.forEach((suit) -> {
            ranks.forEach((rank) -> {
                decks.add(new Card(suit, rank));
            });
        });

        decks.forEach((c) -> {
            System.out.println(c);
        });
    }
}

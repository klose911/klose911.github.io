/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.loop;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

public class BrokenCardLoop {
    // Can you spot the bug?
    public static void main(String[] args) {
        Collection<Suit> suits = Arrays.asList(Suit.values());
        Collection<Rank> ranks = Arrays.asList(Rank.values());
        List<Card> deck = new ArrayList<>();
        //Exception in thread "main" java.util.NoSuchElementException
        for (Iterator<Suit> i = suits.iterator();
                i.hasNext();) {
//            Suit s = i.next();
            for (Iterator<Rank> j = ranks.iterator();
                    j.hasNext();) {
                deck.add(new Card(i.next(), j.next()));
//                deck.add(new Card(s, j.next()));

            }
        }
//        deck.forEach((c) -> {
//            System.out.println(c);
//        });
    }
}

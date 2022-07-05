/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.enums;

import java.util.EnumSet;
import java.util.Set;

/**
 *
 * @author klose
 */
// EnumSet - a modern replacement for bit fields
public class Text {

    public static enum Style {
        BOLD, ITALIC, UNDERLINE, STRIKETHROUGH
    }

    // Any Set could be passed in, but EnumSet is clearly best
    public void applyStyles(Set<Style> styles) {
        //... 
    }

    public static void main(String[] args) {
        Text text = new Text();
        text.applyStyles(EnumSet.of(Style.BOLD, Style.ITALIC));
    }
}

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.executor.render;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author klose
 */
public interface Render {
    /**
     * 
     * @param source 
     */
    void renderPage(CharSequence source);
    
    /**
     * 
     * @param source 
     */
    default void renderText(CharSequence source) {
    }

    /**
     * 
     * @param source
     * @return 
     */
    default List<ImageInfo> scanForImageInfo(CharSequence source) {
        return new ArrayList<>();
    }

    /**
     * 
     * @param data 
     */
    default void renderImage(ImageData data) {

    }

}

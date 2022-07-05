/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.executor.render;

import java.util.ArrayList;
import java.util.List;

public class SingleThreadRenderer implements Render {


    @Override
    public void renderPage(CharSequence source) {
        renderText(source);
        List<ImageData> imageData = new ArrayList<>();
        scanForImageInfo(source).forEach((imageInfo) -> {
            imageData.add(imageInfo.downloadImage());
        });
        imageData.forEach((data) -> this.renderImage(data));
    }
}

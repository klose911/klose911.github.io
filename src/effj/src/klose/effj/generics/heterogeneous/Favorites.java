/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.generics.heterogeneous;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 *
 * @author klose
 */
public class Favorites {

    //使用map的key来做范型，value的class类型就是key
    private final Map<Class<?>, Object> favorites
            = new ConcurrentHashMap<>();

    public <T> void putFavorite(Class<T> type, T instance) {
        if (type == null) {
            throw new NullPointerException("Type is null");
        }
        favorites.put(type, instance);
    }

    public <T> T getFavorite(Class<T> type) {
        return type.cast(favorites.get(type));
    }

    // Typesafe heterogeneous container pattern - client
    public static void main(String[] args) {
        Favorites f = new Favorites();
        f.putFavorite(String.class, "Java");
        f.putFavorite(Integer.class, 0xcafebabe);
        f.putFavorite(Class.class, Favorites.class);
        String favoriteString = f.getFavorite(String.class);
        int favoriteInteger = f.getFavorite(Integer.class);
        Class<?> favoriteClass = f.getFavorite(Class.class);
        //Java cafebabe klose.effj.generics.Favorites
        System.out.printf("%s %x %s%n", favoriteString,
                favoriteInteger, favoriteClass.getName());
    }
    
}

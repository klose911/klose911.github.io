package org.klose.concurrency.composing.delegation;

import java.util.Collections;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class DelegatingVehicleTracker {
    //因为MutablePoint是线程不安全的，所以concurrentMap只能保证point对象的引用是原子修改，无法保证point对象内容是原子修改
    private final ConcurrentMap<String, MutablePoint> locations;
    private final Map<String, MutablePoint> unmodifiableMap;

    //委托ConcurrentMap保证线程安全
    public DelegatingVehicleTracker(Map<String, MutablePoint> points) {
        locations = new ConcurrentHashMap<>(points);
        unmodifiableMap = Collections.unmodifiableMap(locations);
    }

    public Map<String, MutablePoint> getLocations() {
        //另外一个线程修改了locations，依旧是可见的！！！
        return unmodifiableMap;
    }

    public MutablePoint getLocation(String id) {
        return locations.get(id);
    }

    public void setLocation(String id, int x, int y) {
        if (locations.replace(id, new MutablePoint(x, y)) == null)
            throw new IllegalArgumentException(
                    "invalid vehicle name: " + id);
    }
}

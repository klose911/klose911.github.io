package org.klose.concurrency.composing.delegation;

import java.util.Collections;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class PublishingVehicleTracker {
    //使用线程安全的SafePoint类可以保证对SafePoint的修改是原子性
    private final Map<String, SafePoint> locations;
    private final Map<String, SafePoint> unmodifiableMap;

    public PublishingVehicleTracker(
            Map<String, SafePoint> locations) {
        this.locations
                = new ConcurrentHashMap<>(locations);
        this.unmodifiableMap
                = Collections.unmodifiableMap(locations);
    }

    public Map<String, SafePoint> getLocations() {
        return unmodifiableMap;
    }

    public SafePoint getLocation(String id) {
        return locations.get(id);
    }

    public void setLocation(String id, int x, int y) {
        if (!locations.containsKey(id))
            throw new IllegalArgumentException(
                    "invalid vehicle name: " + id);
        locations.get(id).set(x, y);
    }
}

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package klose.effj.generics.heterogeneous;

import java.util.HashMap;
import java.util.Map;

public class DatabaseRow {
    
    private final Map<Column<?>, Object> row = new HashMap<>();

    /**
     *
     * @param <T>
     * @param type
     * @param instance
     */
    public <T> void putColumn(Column<T> type, T instance) {
        if (type == null) {
            throw new NullPointerException("Type is null");
        }
        row.put(type, instance);
    }

    public <T> T getColumn(Column<T> type) {
        return type.cast(row.get(type));
    }

    public static void main(String[] args) {
        DatabaseRow db = new DatabaseRow();

        Column<Integer> colInt = new Column<>(Integer.class);
        Column<Double> colDouble = new Column<>(Double.class);
        Column<Float> colFloat = new Column<>(Float.class);

        db.putColumn(colInt, 1);
        db.putColumn(colDouble, 10.0);
        db.putColumn(colFloat, 12.3f);

        System.out.println(colInt.getClass() + " " + colDouble.getClass());
        System.out.println(db.getColumn(colInt) + " " + db.getColumn(colDouble));
    }
}

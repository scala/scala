package ch.epfl.lamp.util;

import java.util.*;

/**
 * A stack of hash maps.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class StackedHashMap extends AbstractMap implements Map {
    protected LinkedList/*<HashMap>*/ maps = new LinkedList();
    protected HashMap topmostMap = new HashMap();

    public void push() {
        maps.addFirst(topmostMap);
        topmostMap = new HashMap(topmostMap);
    }

    public void push(Map map) {
        push();
        topmostMap.putAll(map);
    }

    public void pop() {
        topmostMap = (HashMap)maps.removeFirst();
    }

    public Object put(Object key, Object value) {
        return topmostMap.put(key, value);
    }

    public void putAll(Object[] keys, Object[] values) {
        assert keys.length == values.length;
        for (int i = 0; i < keys.length; ++i)
            topmostMap.put(keys[i], values[i]);
    }

    public Object get(Object key) {
        return topmostMap.get(key);
    }

    public Set entrySet() {
        return topmostMap.entrySet();
    }
}

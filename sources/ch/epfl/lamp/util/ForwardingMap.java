package ch.epfl.lamp.util;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

/**
 * This class implements the interface Map by forwarding all its
 * operations to an underlying instance of Map.
 */
public class ForwardingMap implements Map {

    //########################################################################
    // Protected Fields

    protected final Map delegate;

    //########################################################################
    // Public Constructors

    public ForwardingMap(Map delegate) {
        this.delegate = delegate;
    }

    //########################################################################
    // Public Methods - Query operations

    public int size() {
        return delegate.size();
    }

    public boolean isEmpty() {
        return delegate.isEmpty();
    }

    public boolean containsKey(Object key) {
        return delegate.containsKey(key);
    }

    public boolean containsValue(Object value) {
        return delegate.containsValue(value);
    }

    public Object get(Object key) {
        return delegate.get(key);
    }

    //########################################################################
    // Public Methods - Modification operations

    public Object put(Object key, Object value) {
        return delegate.put(key, value);
    }

    public Object remove(Object key) {
        return delegate.remove(key);
    }

    //########################################################################
    // Public Methods - Bulk operations

    public void putAll(Map map) {
        delegate.putAll(map);
    }

    public void clear() {
        delegate.clear();
    }

    //########################################################################
    // Public Methods - Views

    public Set keySet() {
        return delegate.keySet();
    }

    public Collection values() {
        return delegate.values();
    }

    public Set entrySet() {
        return delegate.entrySet();
    }

    //########################################################################
    // Public Methods - Comparison and hashing

    public boolean equals(Object that) {
        return delegate.equals(that);
    }

    public int hashCode() {
        return delegate.hashCode();
    }

    //########################################################################
}

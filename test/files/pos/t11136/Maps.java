interface Map<K, V> {
    default V getOrDefault(Object key, V defaultValue) {
        return null;
    }
}

interface ConcurrentMap<K, V> extends Map<K, V> {
    @Override
    default V getOrDefault(Object key, V defaultValue) {
        return null;
    }
}

// class ConcurrentMapWrapper<K, V> implements Map<K, V>, ConcurrentMap<K, V> {  }

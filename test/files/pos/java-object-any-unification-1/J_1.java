package p1;

public class J_1 {
    public static class Map<A, B> {}

    public static class A<K, V> {
        public synchronized void putAll(Map<? extends K, ? extends V> t) {            
        }
    }

    public static class B extends A<Object, Object> {
        @Override
        public synchronized void putAll(Map<?, ?> t) {
        }
    }
}

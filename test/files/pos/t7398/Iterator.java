public interface Iterator<E> {
    boolean hasNext();
    E next();
    default void remove() {
        throw new UnsupportedOperationException("remove");
    }
    default void forEachRemaining(java.util.function.Consumer<? super E> action) {
        throw new UnsupportedOperationException("forEachRemaining");
    }
}

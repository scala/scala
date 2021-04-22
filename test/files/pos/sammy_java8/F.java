public interface F<T, U> {
    U apply(T t);
    default void yadayada() {
        throw new UnsupportedOperationException("yadayada");
    }
}

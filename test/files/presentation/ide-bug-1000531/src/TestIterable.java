public abstract class TestIterable<T> {
    public abstract TestIterator<T> iterator();
    public static abstract class TestIterator<T> {
    	public abstract T next();
    	public abstract boolean hasNext();
    }
}

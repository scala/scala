public class Java {
}

class Partial {
	public <E extends java.lang.Exception> long waitFor(long l, Waiter.Predicate<E> pred) throws E {
		return 0L;
	}
}

class Waiter {
	interface Predicate<E> {}
}

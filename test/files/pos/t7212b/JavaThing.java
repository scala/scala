
public interface JavaThing {
	default void remove() { throw new UnsupportedOperationException(); }
}

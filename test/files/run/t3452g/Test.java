
public class Test {
	public static void main(String[] args) {
		// To get better types here, we would need to
		// add bridge during mixin so we can expose
		// a generic return type of Traversable<A>, because the erasure
		// of this (Traversable) differs from the erasure of the mixed
		// method (erasure(Repr) = Object)

		Object lsSharp = O.tail();

		Object lsSharp2 = new C<String>().tail();
	}
}

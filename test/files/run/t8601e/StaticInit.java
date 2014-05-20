public class StaticInit {
	static {
		if ("".isEmpty()) {
			throw new RuntimeException();
		}
	}
	public static int fld = 42;
}

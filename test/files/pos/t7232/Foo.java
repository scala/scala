package pack;

import java.util.List;

public class Foo {
	public static java.util.List okay() { throw new Error(); }

	public static List wrong() { throw new Error(); }
}

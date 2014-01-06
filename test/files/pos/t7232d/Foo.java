package pack;

import java.util.Map.Entry;

public class Foo {
	public static Entry mapEntry() { throw new Error(); }
	public static void javaTest() { mapEntry().getKey(); }
}

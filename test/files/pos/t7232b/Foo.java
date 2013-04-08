package pack;

import java.util.*;

public class Foo {
	// should be pack.List.
	public static List list() { throw new Error(); }
}

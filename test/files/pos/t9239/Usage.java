/**
 * Used to fail with:
 *
 * Usage.java:5: error: incompatible types: Baz<String> cannot be converted to Foo<String>
 * 	  foo(f);
 *            ^
 */
public class Usage {
  public Usage() {
	  Baz<String> f = null;
	  foo(f);
  }

  public void foo(Foo<String> f) { };
}

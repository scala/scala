
/** Test that call-by-name parameters are set to null if
 *  they are used only to initialize a lazy value, after the
 *  value has been initialized.
 */

class Foo(param1: => Object, param2: => String) {
	lazy val field1 = param1
	lazy val field2 = try param2 finally println("")
}

object Test extends App {
	val foo = new Foo(new Object, "abc")

	foo.field1
	foo.field2

	for (f <- foo.getClass.getDeclaredFields) {
		f.setAccessible(true)
		if (f.getName.startsWith("param")) {
			println("%s: %s".format(f.getName, f.get(foo)))
		}
	}

	// test that try-finally does not generated a liftedTry
	// helper. This would already fail the first part of the test,
	// but this check will help diagnose it (if the single access to a
	// private field does not happen directly in the lazy val, it won't
	// be nulled).
	for (f <- foo.getClass.getDeclaredMethods) {
		f.setAccessible(true)
		if (f.getName.startsWith("lifted")) {
			println("not expected: %s".format(f))
		}
	}
}

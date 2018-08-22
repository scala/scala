import language.implicitConversions

object Test {
	implicit def foo(a: Int)(b: Int, c: Int): String = "" + a + b;
	implicitly[Int => (Int, Int) => String].apply(1).apply(2, 3)
}

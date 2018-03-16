object Test {

  def foo(i: Int)(implicit s: String): String = ""

  val t1 = foo(1) _     // error: no implicit string
  val t2 = foo(1)("") _ // error: _ must follow method
  val t3 = foo _        // error: no implicit string
  val t4 = { implicit val s = ""; foo _ } // eta-expansion over the non-implicit parameter list
  val t4a: Int => String = t4             // ok
  val t5 = { implicit val s = ""; foo(1) _ }       // compiles as Predef.wrapString(foo(1)(s))
  val t5a: collection.immutable.WrappedString = t5 // don't ask me why

  def bar(i: Int)(implicit j: Int): Int = 0
  val t6 = { implicit val i = 0; bar(0) _ } // error: type mismatch, found Int, required: ? => ?

  def fooSimple(implicit x: Int): Int = x
  val barSimple = fooSimple _ // error: no implicit int
}

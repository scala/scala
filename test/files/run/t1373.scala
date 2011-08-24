// Testing whether case class params come back in the right order.
object Test extends App {
  case class Foo(private val a: String, b: String, private val c: String, d: String, private val e: String)
  val x = Foo("a", "b", "c", "d", "e")
  assert(x.toString == """Foo(a,b,c,d,e)""")
}

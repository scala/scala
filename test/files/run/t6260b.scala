class C[A](val a: A) extends AnyVal

class DD {
  def foo(c: C[String]) = ()
  def bar[A <: String](c: C[A]) = ()
  def baz[A](c: C[A]) = ()
}

object Test extends App {
  classOf[DD].getMethod("foo", classOf[String])
  classOf[DD].getMethod("bar", classOf[String])
  classOf[DD].getMethod("baz", classOf[Object])
}

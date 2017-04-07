trait T {
  @org.junit.Test def foo = 0
}

class C extends T

object Test extends App {
  def check(c: Class[_], e: String) = {
    val s = c.getDeclaredMethods.sortBy(_.getName).map(m => s"${m.getName} - ${m.getDeclaredAnnotations.mkString(", ")}").mkString(";")
    assert(s == e, s"found: $s\nexpected: $e")
  }
  check(classOf[C], "foo - @org.junit.Test()")
  // scala/scala-dev#213, scala/scala#5570: `foo$` should not have the @Test annotation
  check(classOf[T], "$init$ - ;foo - @org.junit.Test();foo$ - ")
}

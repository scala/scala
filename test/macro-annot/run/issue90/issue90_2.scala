import scala.reflect.runtime.{currentMirror => cm}

object Test extends App {
  def assertEquals(a: Any, b: Any): Unit = { assert(a == b, s"$a != $b") }

  // @Test def compileTimeOnlyOnceOnly: Unit =
  {
    assertEquals(cm.staticClass("issue90Class").annotations.length, 1)
  }

  // @Test def compileTimeOnlyOnceOnlyMessage: Unit =
  {
    assertEquals(cm.staticClass("issue90Class").annotations.head.toString, "scala.annotation.compileTimeOnly(\"this is the only annotation\")")
  }
}

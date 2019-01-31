import scala.reflect.runtime.{currentMirror => cm}

object Test extends App {
  def assertEquals(a: Any, b: Any): Unit = { assert(a == b, s"$a != $b") }

  // @Test  def macroAnnotationsMarkedWithMACRO: Unit =
  {
    assertEquals(cm.staticClass("identity").isMacro, true)
  }
}

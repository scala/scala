// javaVersion: 11+

object Test {
  def main(args: Array[String]): Unit = {
    val a = new Array[Object](1)
    val h = java.lang.invoke.MethodHandles.arrayElementVarHandle(a.getClass)
    val r = h.setVolatile(a, 0, "foo") // important: no expected type
  }
}

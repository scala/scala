class Foo
object Foo {
  def bar(a: Int, `_`: String, *** : Long, `unary_!` : Float, ABC: Double) = null
}

object Test extends App {
  val methodParams = classOf[Foo].getDeclaredMethods.head.getParameters

  def printParams(params: Array[java.lang.reflect.Parameter]) = {
    params.foreach { param =>
      println(s"name: ${param.getName}; isNamePresent: ${param.isNamePresent}; isSynthetic: ${param.isSynthetic}")
    }
  }

  printParams(methodParams)

  Foo.bar(a = 1, `_` = "2", *** = 3L, `unary_!` = 4.0f, ABC = 5.0)
}

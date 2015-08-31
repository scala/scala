class Foo(a: Int, `_`: String, *** : Long, `unary_!` : Float, ABC: Double) {
  def bar(a: Int, `_`: String, *** : Long, `unary_!` : Float, ABC: Double) = null
}

object Test extends App {
  val constrParams = classOf[Foo].getConstructors.head.getParameters
  val methodParams = classOf[Foo].getDeclaredMethods.head.getParameters

  def printParams(params: Array[java.lang.reflect.Parameter]) = {
    params.foreach { param =>
      println(s"name: ${param.getName}; isNamePresent: ${param.isNamePresent}; isSynthetic: ${param.isSynthetic}")
    }
  }

  printParams(constrParams)
  printParams(methodParams)

  val foo = new Foo(a = 1, `_` = "2", *** = 3L, `unary_!` = 4.0f, ABC = 5.0)
  foo.bar(a = 1, `_` = "2", *** = 3L, `unary_!` = 4.0f, ABC = 5.0)
}

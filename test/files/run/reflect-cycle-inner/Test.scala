object Test {

  def main(args: Array[String]): Unit = {
    import scala.reflect.runtime.universe._
    val decls = typeOf[example.Example].decls // throws scala.reflect.internal.Symbols$CyclicReference: illegal cyclic reference involving class ExampleClass
    println(decls)
  }
}

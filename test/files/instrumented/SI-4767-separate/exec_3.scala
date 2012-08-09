import scala.tools.partest.instrumented.Instrumentation._
import instrumented._

object Test {
  def main(args: Array[String]) {
    // force predef initialization before profiling
    Predef
    // create instances of Foo and Bar
    val bar = new Bar
    val foo = new Foo {}
    val x1 = 7
    val x2 = new Array[String](3)
    val x3 = new Array[Int](3)
    val x4 = new Gândăcel
    startProfiling()
    // if the inlining took place, you won't see a call to Foo$class.foo, meaning it wasn't inlined
    bar.bar(foo, x1, x2, x3, x4)
    stopProfiling()
    printStatistics()
  }
}


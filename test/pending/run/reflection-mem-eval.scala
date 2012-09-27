import scala.tools.partest.MemoryTest

trait A { type T <: A }
trait B { type T <: B }

object Test extends MemoryTest {
  lazy val tb = {
    import scala.reflect.runtime.universe._
    import scala.reflect.runtime.{currentMirror => cm}
    import scala.tools.reflect.ToolBox
    cm.mkToolBox()
  }

  override def maxDelta = 10
  override def calcsPerIter = 3
  override def calc() {
    var snippet = """
      trait A { type T <: A }
      trait B { type T <: B }
      def foo[T](x: List[T]) = x
      foo(List(new A {}, new B {}))
    """.trim
    snippet = snippet + "\n" + (List.fill(50)(snippet.split("\n").last) mkString "\n")
    tb.eval(tb.parse(snippet))
  }
}
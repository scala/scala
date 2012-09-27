import scala.tools.partest.MemoryTest

trait A { type T <: A }
trait B { type T <: B }

object Test extends MemoryTest {
  override def maxDelta = 10
  override def calcsPerIter = 100000
  override def calc() {
    import scala.reflect.runtime.universe._
    def foo = {
      class A { def x = 2; def y: A = new A }
      weakTypeOf[A { def z: Int }]
    }
    foo
  }
}
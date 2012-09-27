import scala.tools.partest.MemoryTest

trait A { type T <: A }
trait B { type T <: B }

object Test extends MemoryTest {
  override def maxDelta = 10
  override def calcsPerIter = 50000
  override def calc() {
    import scala.reflect.runtime.universe._
    glb(List(typeOf[A], typeOf[B]))
  }
}
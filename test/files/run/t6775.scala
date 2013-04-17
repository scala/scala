import scala.tools.partest._
import java.io.{Console => _, _}

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -Xprint:specialize"

  override def code = 
    """
trait Counter[@specialized(Int) +T] {
  def apply(): T
}

class IntCounter(var state: Int) extends Counter[Int] {
  def apply(): Int = synchronized { // not properly synchronized
    val n = state
    state += 1
    n
  }
}
"""

  override def show(): Unit = {
    Console.withErr(System.out) {
      compile()
    }
  }
}

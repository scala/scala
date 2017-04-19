import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval
 
object O {
  type A = {
    type B = {
      type C
    }
  }
}
 
object Test {
  val x: O.A = new {
    type B = O.A#B
  }
 
  val y: x.B = new {
    type C = x.B#C
  }
 
  def expr = reify {
    val b: x.B = y
  }
 
  def main(args: Array[String]): Unit = {
    expr.eval
  }
}

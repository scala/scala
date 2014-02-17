import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  val reified = reify {
    val ru = scala.reflect.runtime.universe
    val tpe1: ru.Type = ru.typeOf[List[Int]]
    println(tpe1)
    val tpe2: ru.Type = ru.typeOf(List(1, 2, 3))
    println(tpe2)
  }
  println(reified)
  println(reified.eval)
}
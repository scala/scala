
import scala.language.implicitConversions
import scala.language.reflectiveCalls

object Test extends App {

  abstract class Value {
  }

  case class Num(value: Int) extends Value {
    override def toString = value.toString;
  }

  implicit def conversions(x: Value) = new {
    def toInt =
      x match {
        case Num(n) => n
        case _ => throw new RuntimeException
      }
  }

  def eval(v: Value): Value = {
    println("hey")
    Num(1)
  }

  eval(Num(1)).toInt
}

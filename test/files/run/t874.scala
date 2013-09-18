
import scala.language.{ reflectiveCalls }
object Test {
  abstract class Base {
    val U: {
      def apply[A](x1: A)(x2: Int): Any
    }
    U("xyz")(2)
  }
  class Mix extends Base {
    case class U[A](x1: A)(x2: Int) {
      Console.println("U created with "+x1+" and "+x2)
    }
  }
  def main(args : Array[String]) : Unit = {
    val obvious: Base = new Mix;
    obvious.U("abc")(1)
  }
}

import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.tools.reflect.Eval

object Test extends App {
  {
    type T = Int
    implicit val tt = implicitly[TypeTag[String]].asInstanceOf[TypeTag[T]] // this "mistake" is made for a reason!
    val code = reify {
      List[T](2)
    }
    println(code.eval)
  }
}
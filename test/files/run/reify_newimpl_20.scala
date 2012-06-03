import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.tools.reflect.Eval

object Test extends App {
  class C {
    type T
    implicit val tt: TypeTag[T] = implicitly[TypeTag[Int]].asInstanceOf[TypeTag[T]]
    val code = reify {
      List[T](2.asInstanceOf[T])
    }
    println(code.eval)
  }

  new C { type T = String } // this "mistake" is made for a reason!
}
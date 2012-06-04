import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.tools.reflect.Eval

object Test extends App {
  trait C {
    type T
    implicit val tt: TypeTag[T]
    lazy val code = reify {
      List[T](2.asInstanceOf[T])
    }
  }

  class D extends C {
    type T = String // this "mistake" is made for a reason!
    override val tt: TypeTag[T] = implicitly[TypeTag[Int]].asInstanceOf[TypeTag[T]]
  }

  println((new D).code.eval)
}
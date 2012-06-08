import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.tools.reflect.Eval

object Test extends App {
  {
    class C[T: TypeTag] {
      val code = reify {
        List[T](2.asInstanceOf[T])
      }
      println(code.eval)
    }

    new C[Int]
  }
}
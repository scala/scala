import scala.reflect.runtime.universe._
import scala.tools.reflect.{ ToolBox, ToolBoxError }
import scala.tools.reflect.Eval

object Test extends App {
  {
    class C {
      type T = Int
      val code = reify {
        List[C#T](2)
      }
      try { println(code.eval) }
      catch { case e: ToolBoxError => println(e.getMessage) }
    }

    new C
  }
}

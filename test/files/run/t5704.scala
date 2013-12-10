import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  class MyQuerycollection{
    def findUserByName( name:String ) = {
      val tree = reify{ "test" == name }.tree
      val toolbox = cm.mkToolBox()
      toolbox.typecheck(tree) match{
        case Apply(Select(lhs,op),rhs::Nil) =>
          println(rhs.tpe)
      }
    }
  }
  val qc = new MyQuerycollection
  qc.findUserByName("some value")
}
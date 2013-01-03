import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test {
  def main(args: Array[String]): Unit = {
    def z(x: Int) = x
    reify(z _)
//    cm.mkToolBox().eval(reify(z _).tree) fails at runtime due to
//    https://issues.scala-lang.org/browse/SI-6693 with the following error:
//    scala.tools.reflect.ToolBoxError: reflective compilation has failed: 
//    
//    missing parameter type for expanded function ((x) => z$value()(x))
//    	at scala.tools.reflect.ToolBoxFactory$ToolBoxImpl$ToolBoxGlobal.throwIfErrors(ToolBoxFactory.scala:323)
//    	at scala.tools.reflect.ToolBoxFactory$ToolBoxImpl$ToolBoxGlobal.compile(ToolBoxFactory.scala:256)
//    	at scala.tools.reflect.ToolBoxFactory$ToolBoxImpl.compile(ToolBoxFactory.scala:420)
//    	at scala.tools.reflect.ToolBoxFactory$ToolBoxImpl.eval(ToolBoxFactory.scala:423)
//    	[...]
  }
}
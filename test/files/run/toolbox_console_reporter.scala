import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.{ToolBox, mkConsoleFrontEnd}

object Test extends App {
  val oldErr = Console.err;
  val baos = new java.io.ByteArrayOutputStream();
  Console.setErr(new java.io.PrintStream(baos));
  try {
    val toolbox = cm.mkToolBox(frontEnd = mkConsoleFrontEnd(), options = "-deprecation")
    toolbox.eval(reify{
      object Utils {
        @deprecated("test", "2.10.0")
        def foo { println("hello") }
      }

      Utils.foo
    }.tree)
    println("============compiler console=============")
    println(baos.toString);
    println("=========================================")
    println("============compiler messages============")
    toolbox.frontEnd.infos.foreach(println(_))
    println("=========================================")
  } finally {
    Console.setErr(oldErr);
  }
}
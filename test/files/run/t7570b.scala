import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.{ToolBox, ToolBoxError}
import definitions._
import Flag._

object Test extends App {
  val tb = cm.mkToolBox()
  val msg = build.newFreeTerm("msg", "C")
  build.setTypeSignature(msg, typeOf[String])
  try {
    val csym = tb.define(q"""class C { override def toString = $msg }""")
    println(tb.eval(q"new $csym"))
  } catch {
    case ToolBoxError(message, _) => println(s"compilation failed: $message")
  }
}
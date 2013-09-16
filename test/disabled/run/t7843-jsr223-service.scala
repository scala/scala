/*DISABLED:
    For Paul, it steals focus when it runs.

    For me, it fails with some platform specific extra output:

    -ScriptEngineManager providers.next(): javax.script.ScriptEngineFactory: Provider apple.applescript.AppleScriptEngin
     n: Object = 10
     12345678910
*/
import javax.script._
import scala.tools.nsc.interpreter.IMain

object Test extends App {
  val engine = new ScriptEngineManager getEngineByName "scala"
  engine.asInstanceOf[IMain].settings.usejavacp.value = true
  engine put ("n", 10)
  engine eval "1 to n.asInstanceOf[Int] foreach print"
}

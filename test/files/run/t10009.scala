import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox

object Test {
  def test(code: String, log: Boolean = false) {
    val tb = currentMirror.mkToolBox()
    val tree = tb.parse(code)
    val typed = tb.typecheck(tree)
    if (log) {
      println("=" * 80)
      println(typed)
    }
    val untyped = tb.untypecheck(typed)
    if (log) println(untyped)
    val retyped = tb.typecheck(untyped)
    if (log) println(retyped)
  }
  def main(args: Array[String]): Unit = {
    test("{ class a { val x = 42 }; new a }") // failed
    test("{ trait a { val x = 42 }; new a {} }") // worked
    test("{ abstract class a { val x: Int } }") // worked
    test("{ abstract class a { val x: Int }; new a { val x = 42 } }") // failed
    test("{ class a { private val x = 42 }; new a }") // failed
    test("{ class a { protected val x = 42 }; new a { x } }") // failed
    test("{ class a { protected[a] val x = 42 }; new a }") // failed
  }
}
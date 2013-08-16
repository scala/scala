import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
:silent
class A { override def toString() = { println("!"); "A" } }
val a = new A
var b: A = new A
b = new A
new A
:silent
res0
"""
}

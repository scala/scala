// TODO: fix AME when this runs in REPL
import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
trait B { val y = "a" }
trait T extends B { val x: y.type = y }
println((new T{}).x)
"""
}

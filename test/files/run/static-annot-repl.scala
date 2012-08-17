


import scala.tools.partest.ReplTest



object Test extends ReplTest {
  def code = """
import annotation.static
@static var x1 = 42
@static val x2 = 43
@static def x3 = 44
x1
x2
x3
class Test {
  @static def x = 42
}
"""

}

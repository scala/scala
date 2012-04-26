import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
var s = 0
for (i <- 1 to 10) {s += i}
for (i <- 1 to 10) {s += i}
for (i <- 1 to 10) {s += i}
println(s)
  """
}

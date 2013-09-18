object Test extends App {
  import scala.reflect.internal.util.StringContextStripMarginOps
  def check(actual: Any, expected: Any) = if (actual != expected) sys.error(s"\nexpected:\n$expected\n\nactual:\n$actual")

  val bar = "|\n ||"

  check(
    sm"""|ab
         |de
         |${bar} | ${1}""",
      "ab\nde\n|\n || | 1")

  check(
    sm"|",
      "")

  check(
    sm"${0}",
      "0")

  check(
    sm"${0}",
    "0")

  check(
    sm"""${0}|${1}
     |""",
      "0|1\n")

  check(
    sm"""   ||""",
      "|")

  check(
    sm""" ${" "} ||""",
      "   ||")

  check(
      sm"\n",
     raw"\n".stripMargin)
}

object Test extends App {
	import scala.reflect.internal.util.StringContextStripMarginOps
  def assertEqual(expected: Any, actual: Any) = if (actual != expected) sys.error(s"expected: [$expected], actual: [$actual])")

  val bar = "|\n ||"

	assertEqual(
		sm"""|ab  
         |de
         |${bar} | ${1}""",
      "ab  \nde\n|\n || | 1")

	assertEqual(
		sm"|",
      "")

	assertEqual(
		sm"${0}",
      "0")

	assertEqual(
		sm"${0}",
    "0")

	assertEqual(
		sm"""${0}|${1}
     |""",
      "0|1\n")

	assertEqual(
		sm"""   ||""",
      "|")

	assertEqual(
		sm""" ${" "} ||""",
      "   ||")
}

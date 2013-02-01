import scala.tools.partest.ReplTest

// Check that the fragments of code generated in
// in the REPL correctly escape values added to
// literal strings.
//
// Before, we saw:
// scala> m("").x = 77
// <console>:10: error: ')' expected but string literal found.
//  + "m("").x: Int = " + `$ires8` + "\n"
object Test extends ReplTest {
  def code = """
    |case class `X"`(var xxx: Any)
    |val m = Map(("": Any) -> `X"`("\""), ('s: Any) -> `X"`("\""))
    |m("")
    |m("").xxx
    |m("").xxx = 0
    |m("").xxx = "\""
    |m('s).xxx = 's
    |val `"` = 0
  """.stripMargin
}

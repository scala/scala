import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def tripleQuote(s: String) = "\"\"\"" + s + "\"\"\""

  def code = s"""
:power
// guarding against "error: reference to global is ambiguous"
global.emptyValDef  // "it is imported twice in the same scope by ..."
val tp = ArrayClass[scala.util.Random]    // magic with tags
tp.memberType(Array_apply)                // evidence
val m = LIT(10)                           // treedsl
typed(m).tpe                              // typed is in scope
${tripleQuote("escaping is hard, m'kah")}
  """.trim
}

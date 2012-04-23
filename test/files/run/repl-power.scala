import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
:power
// guarding against "error: reference to global is ambiguous"
global.emptyValDef  // "it is imported twice in the same scope by ..."
val tp = ArrayClass[scala.util.Random]    // magic with tags
tp.memberType(Array_apply)                // evidence
val m = LIT(10) MATCH (CASE(LIT(5)) ==> FALSE, DEFAULT ==> TRUE) // treedsl
typed(m).tpe                              // typed is in scope
  """.trim
}

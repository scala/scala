import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
:power
// guarding against "error: reference to global is ambiguous"
global.emptyValDef  // "it is imported twice in the same scope by ..."
  """.trim
}


import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
Map(1 -> "eis").values    // no warn
:setting -Xmigration:none
Map(1 -> "eis").values    // no warn
:setting -Xmigration:any
Map(1 -> "eis").values    // warn
:setting -Xmigration:2.8
Map(1 -> "eis").values    // no warn
:setting -Xmigration:2.7
Map(1 -> "eis").values    // warn
:setting -Xmigration:2.11
Map(1 -> "eis").values    // no warn
:setting -Xmigration      // same as :any
Map(1 -> "eis").values    // warn
  """
}

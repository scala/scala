
import scala.tools.partest.SessionTest

object Test extends SessionTest {
  def session =
s"""|
    |scala> :paste -raw $pastie
    |Pasting file $pastie...
    |$pastie:3: error: expected class or object definition
    |val nope = 42
    |^
    |There were compilation errors!
    |
    |scala> :quit"""
  def pastie = testPath changeExtension "pastie"
}

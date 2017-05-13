
import scala.tools.partest.SessionTest

object Test extends SessionTest {
  override def session =
s"""|
    |scala> :paste -raw $pastie
    |Pasting file $pastie...
    |
    |       val nope = 42
    |       ^
    |$pastie:3: error: expected class or object definition
    |There were compilation errors!
    |
    |scala> :quit"""
  def pastie = testPath changeExtension "pastie"

  override def stripMargins: Boolean = true

  override def show() = checkSession()
}


import scala.tools.partest.SessionTest

object Test extends SessionTest {
  override def session =
s"""|
    |scala> :paste $pastie
    |Pasting file $pastie...
    |class Foo
    |object Foo
    |
    |scala> Foo(new Foo)
    |val res0: Int = 7
    |
    |scala> :quit"""
  def pastie = testPath changeExtension "pastie"

  override def stripMargins: Boolean = true

  override def show() = checkSession()
}


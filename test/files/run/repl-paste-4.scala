
import scala.tools.partest.SessionTest

object Test extends SessionTest {
  def session =
s"""|
    |scala> :paste $pastie
    |Pasting file $pastie...
    |defined class Foo
    |defined object Foo
    |
    |scala> Foo(new Foo)
    |res0: Int = 7
    |
    |scala> :quit"""
  def pastie = testPath changeExtension "pastie"
}



import scala.tools.partest.SessionTest

object Test extends SessionTest {
  def session =
s"""|
    |scala> :paste -raw $pastie
    |Pasting file $pastie...
    |
    |scala> val favoriteThing = brown_paper.Gift(true)
    |favoriteThing: brown_paper.Gift = Gift(true)
    |
    |scala> favoriteThing.hasString
    |res0: Boolean = true
    |
    |scala> :quit"""
  def pastie = testPath changeExtension "pastie"
}

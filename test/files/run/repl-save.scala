import scala.tools.partest.SessionTest

object Test extends SessionTest {
  def session =
s"""|Type in expressions to have them evaluated.
    |Type :help for more information.
    |
    |scala> val i = 7
    |i: Int = 7
    |
    |scala> val j = 8
    |j: Int = 8
    |
    |scala> i * j
    |res0: Int = 56
    |
    |scala> :save $saveto
    |
    |scala> """
  def saveto = testOutput / "session.repl"
  override def show() = {
    super.show()
    Console print saveto.toFile.slurp
  }
}

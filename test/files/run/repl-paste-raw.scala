
import scala.tools.partest.ReplTest
import scala.tools.partest.nest.FileUtil


// arrives in partest in PR# 2701
abstract class SessionTest extends ReplTest with FileUtil {
  def session: String
  override final def code = expected filter (_.startsWith(prompt)) map (_.drop(prompt.length)) mkString "\n"
  def expected = session.stripMargin.lines.toList
  final def prompt = "scala> "
  override def show() = {
    val out = eval().toList
    if (out.size != expected.size) Console println s"Expected ${expected.size} lines, got ${out.size}"
    if (out != expected) Console print compareContents(expected, out, "expected", "actual")
  }
}

object Test extends SessionTest {
  /* old ReplTest
  def code = s"""
  |:paste -raw $pastie
  |val favoriteThing = brown_paper.Gift(true)
  |favoriteThing.hasString
  |""".stripMargin.trim
  */
  def pastie = testPath changeExtension "pastie"

  def session =
s"""|Type in expressions to have them evaluated.
    |Type :help for more information.
    |
    |scala> :paste -raw $pastie
    |Pasting file $pastie...
    |
    |scala> val favoriteThing = brown_paper.Gift(true)
    |favoriteThing: brown_paper.Gift = Gift(true)
    |
    |scala> favoriteThing.hasString
    |res0: Boolean = true
    |
    |scala> """
}

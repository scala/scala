import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code =
    """abstract class A { def f: Object }
      |object B extends A { @annotation.inheritSignature def f: String = " b "; def g: String = f }
      |B.f.trim
      |B.g.trim
      |""".stripMargin
}

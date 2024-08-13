import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    |class ann extends annotation.StaticAnnotation
    |def a: Int => (Int @ann) = ???
    |def b: Int => Int @ann = ???
    |def c: (Int => Int) @ann = ???
    |def d: Int => (Int => Int) @ann = ???
    |def e: (Int => Int => Int) @ann = ???
    |""".stripMargin
}

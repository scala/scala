package scala.tools.nsc.parser

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.BytecodeTesting

@RunWith(classOf[JUnit4])
class ParserTest extends BytecodeTesting{
  override def compilerArgs: String = "-Ystop-after:parser -Yvalidate-pos:parser -Yrangepos"
  @Test
  def crlfRangePositionXml_t10321(): Unit = {
    val code =
      """
        |object Test {
        |  Nil.map { _ =>
        |    <X />
        |    <Y />
        |  }
        |}
      """.stripMargin
    val crlfCode = code.replaceAllLiterally("\n", "\r\n")
    val lfCode = code
    assert(crlfCode != lfCode)
    import compiler._, global._
    val run = new Run
    run.compileSources(newSourceFile(lfCode) :: Nil)
    assert(!reporter.hasErrors)
    run.compileSources(newSourceFile(crlfCode) :: Nil)
  }
}

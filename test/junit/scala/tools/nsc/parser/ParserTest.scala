package scala.tools.nsc.parser

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Assert._

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
    val crlfCode = code.linesIterator.map(_ + "\r\n").mkString
    val lfCode = code.linesIterator.map(_ + "\n").mkString
    assert(crlfCode != lfCode)
    import compiler._, global._
    val run = new Run
    run.compileSources(newSourceFile(lfCode) :: Nil)
    assert(!reporter.hasErrors)
    run.compileSources(newSourceFile(crlfCode) :: Nil)
  }

  @Test
  def rangePosOfDefaultInitializer_t12213(): Unit = {
    val code =
      """object Other { var x: Int = _; var y: Int = 42 }"""
    import compiler._, global._
    val run = new Run
    run.compileSources(newSourceFile(code) :: Nil)
    assert(!reporter.hasErrors)
    val unit = run.units.toList.head
    def codeOf(pos: Position) = new String(pos.source.content.slice(pos.start, pos.end))
    val List(x, y) = unit.body.collect { case vd : ValDef => vd }.takeRight(2)
    assertEquals("var y: Int = 42", codeOf(y.pos))
    assertEquals("var x: Int = _", codeOf(x.pos))
  }
}

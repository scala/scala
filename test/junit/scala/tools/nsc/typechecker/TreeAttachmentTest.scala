package scala.tools.nsc.typechecker

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testkit.BytecodeTesting

@RunWith(classOf[JUnit4])
class TreeAttachmentTest extends BytecodeTesting {

  import compiler.global._

  override def compilerArgs: String = "-Ystop-after:typer"

  @Test
  def namedApplyInfoAttachmentPreserved(): Unit = {
    def compile(testMethodCode: String, call: String): Option[analyzer.NamedApplyInfo] = {
      val code =
        s"""
          |class C {
          |  $testMethodCode
          |  def s = ""
          |  def useTest = $call
          |}
          |""".stripMargin

      compiler.compileClasses(code)
      val run = compiler.newRun()
      run.compileUnits(newCompilationUnit(code) :: Nil, run.parserPhase)
      val tree : Tree = run.units.next().body
      val block: Tree = tree.collect { case b: Block => b }.last
      block.attachments.get[analyzer.NamedApplyInfo]
    }


    def vargss(x: Option[analyzer.NamedApplyInfo]): List[List[String]] = x.map(x => mmap(x.vargss)(_.symbol.name.toString)).getOrElse(Nil)

    assertEquals(None, compile("def test(a: Any, b: Any)(c: Any)(implicit d: DummyImplicit) = 0", "test(s, s)(s)"))

    val expected = List(List("x$2", "x$1"), List("x$3"))
    assertEquals(expected, vargss(compile("def test(a: Any, b: Any)(c:    Any)                            = 0", "test(b = s, a = s)(s)")))
    assertEquals(expected, vargss(compile("def test(a: Any, b: Any)(c:    Any)(implicit d: DummyImplicit) = 0", "test(b = s, a = s)(s)")))
  }
}

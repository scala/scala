
package scala.tools.nsc.typechecker

import org.junit.Test
import org.junit.Assert._

import scala.tools.nsc.reporters.StoreReporter
import scala.tools.testkit.{BytecodeTesting, XMLTesting}, BytecodeTesting.SourceFile, XMLTesting.xml

class XMLTest extends BytecodeTesting {
  override def compilerArgs: String = "-Ystop-after:refchecks -Wnonunit-statement"

  @Test def `building elements does not warn nonunit-statement`: Unit = {
    val code =
      """
        |object Test {
        |  val xml = <xml><elem></elem></xml>
        |}
      """.stripMargin
    val reporter = compiler.global.reporter.asInstanceOf[StoreReporter]
    val run = compiler.newRun()
    run.compileSources(SourceFile(xml.code, code))
    assertFalse(reporter.hasWarnings)
    assertFalse(reporter.hasErrors)
  }
}
class XMLLessTest extends BytecodeTesting {
  override def compilerArgs: String = "-Ystop-after:refchecks"

  @Test def `detect XML lib is absent`: Unit = {
    val code =
      """
        |object Test {
        |  val xml = <xml><elem></elem></xml>
        |}
      """.stripMargin
    val reporter = compiler.global.reporter.asInstanceOf[StoreReporter]
    val run = compiler.newRun()
    run.compileSources(SourceFile(code))
    assertTrue(reporter.hasErrors)
    assertTrue(reporter.infos.forall(_.msg.contains("scala.xml")))
  }
}

import java.nio.file.Files

import org.junit.Assert.assertFalse
import scala.jdk.CollectionConverters._
import scala.reflect.internal.util._
import scala.tools.partest.DirectTest
import scala.tools.testkit.AssertUtil.assertSameElements

import org.junit.Assert._

object Test extends DirectTest {
  lazy val argfile = testOutput.jfile.toPath().resolve("print-args.txt")
  override def extraSettings = s"${super.extraSettings} -Vprint-args ${argfile}"
  def expected =
    sm"""
    |-usejavacp
    |-d
    |${testOutput.jfile.toPath()}
    |-Vprint-args
    |${testOutput.jfile.toPath().resolve(argfile)}
    |newSource1.scala
    """.trim
  def code =
    sm"""
    |class C {
    |  def f: Int = "42"
    |}
    """
  def show() = {
    assertFalse(compile())
    assertSameElements(expected.linesIterator, Files.readAllLines(argfile).asScala)
  }
}


import java.nio.file.Files

import scala.jdk.CollectionConverters._
import scala.reflect.internal.util._
import scala.tools.partest.DirectTest

object Test extends DirectTest {
  lazy val argfile = testOutput.jfile.toPath().resolve("print-args.txt")
  override def extraSettings = s"${super.extraSettings} -Xsource:3 -Vprint-args ${argfile}"
  def expected =
    sm"""
    |-usejavacp
    |-d
    |${testOutput.jfile.toPath()}
    |-Xsource:3.0.0
    |-Vprint-args
    |${testOutput.jfile.toPath().resolve(argfile)}
    |newSource1.scala
    """
  def code =
    sm"""
    |class C {
    |  def f: Int = "42"
    |}
    """
  def show() = {
    assert(!compile())
    assert(expected.linesIterator.toList.tail.init.sameElements(Files.readAllLines(argfile).asScala))
  }
}

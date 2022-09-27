
import java.nio.file.Files

import scala.jdk.CollectionConverters._
import scala.reflect.internal.util._
import scala.tools.nsc.{CompilerCommand, Settings}
import scala.tools.partest.DirectTest
import scala.util.chaining._

object Test extends DirectTest {
  var count = 0
  lazy val argfile = testOutput.jfile.toPath().resolve("print-args.txt")
  lazy val goodarg = testOutput.jfile.toPath().resolve("print-args2.txt")
  override def extraSettings =
    if (count == 0) s"""${super.extraSettings} -Xsource:3 -Vprint-args $argfile "-Wconf:cat=unused-nowarn&msg=does not suppress&site=C:s""""
    else s"@$goodarg"
    
  // Use CompilerCommand for expanding the args file.
  override def newSettings(args: List[String]) = (new Settings).tap { s =>
    val cc = new CompilerCommand(args, s)
    assert(cc.ok)
    assert(cc.files.isEmpty)
  }
  def code =
    sm"""
    |@annotation.nowarn
    |final class C {
    |  def f: Int = "42".toInt
    |}
    """
  def show() = {
    assert(compile())
    // drop "-Vprint-args .../print-args.txt newSource1.scala"
    val args = Files.readAllLines(argfile).asScala.toList.dropRight(3)
    Files.write(goodarg, args.asJava)
    count += 1
    assert(compile())
  }
}

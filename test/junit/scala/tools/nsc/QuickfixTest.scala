package scala.tools.nsc

import org.junit.Assert._
import org.junit.Test

import java.nio.file.Files
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.AbstractFile
import scala.tools.testkit.BytecodeTesting
import scala.tools.testkit.ReleasablePath._
import scala.util.Using

class QuickfixTest extends BytecodeTesting {
  def testQuickfix(a: String, b: String, args: String): Unit = if (!scala.util.Properties.isWin) {
    Using.resource(Files.createTempFile("unitSource", "scala")) { src =>
      Files.write(src, a.getBytes)
      val c = BytecodeTesting.newCompiler(extraArgs = args)
      val r = c.newRun()
      val f = AbstractFile.getFile(src.toFile.getAbsolutePath)
      r.compileSources(List(new BatchSourceFile(f)))
      assertEquals(b, new String(Files.readAllBytes(src)))
    }
  }

  def comp(args: String) = BytecodeTesting.newCompiler(extraArgs = args)

  @Test def multipleRewrites(): Unit = {
    // procedure syntax is a parser warning
    // "val" in for-comprehension is a parser error
    // auto-application is fixed only in 2nd compilation - parser error prevents later phase from running
    val a =
      """class C {
        |  def f { println }
        |  def g(xs: List[String]) = for (val x <- xs) yield x.trim
        |}
        |""".stripMargin
    val b =
      """class C {
        |  def f: Unit = { println }
        |  def g(xs: List[String]) = for (x <- xs) yield x.trim
        |}
        |""".stripMargin
    val c =
      """class C {
        |  def f: Unit = { println() }
        |  def g(xs: List[String]) = for (x <- xs) yield x.trim
        |}
        |""".stripMargin
    testQuickfix(a, b, "-deprecation -quickfix:any")
    testQuickfix(b, c, "-deprecation -quickfix:any")
  }

  @Test def filters(): Unit = {
    val a =
      """class C {
        |  def f { println }
        |  def g(xs: List[String]) = for (val x <- xs) yield x.trim
        |}
        |""".stripMargin
    val b =
      """class C {
        |  def f { println }
        |  def g(xs: List[String]) = for (x <- xs) yield x.trim
        |}
        |""".stripMargin
    val c =
      """class C {
        |  def f { println() }
        |  def g(xs: List[String]) = for (x <- xs) yield x.trim
        |}
        |""".stripMargin
    val d =
      """class C {
        |  def f: Unit = { println() }
        |  def g(xs: List[String]) = for (x <- xs) yield x.trim
        |}
        |""".stripMargin
    testQuickfix(a, b, "-deprecation -quickfix:msg=comprehension")
    testQuickfix(b, b, "-deprecation -quickfix:msg=teddy")
    testQuickfix(b, c, "-deprecation -quickfix:msg=Auto-application")
    testQuickfix(c, c, "-quickfix:cat=deprecation") // without -deprecation, the warning is not shown and not rewritten
    testQuickfix(c, d, "-deprecation -quickfix:cat=deprecation")
  }
}

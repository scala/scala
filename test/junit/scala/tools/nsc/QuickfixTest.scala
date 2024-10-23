package scala.tools.nsc

import org.junit.Assert._
import org.junit.Test

import java.nio.file.Files
import scala.jdk.CollectionConverters._
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.AbstractFile
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.testkit.AssertUtil._
import scala.tools.testkit.BytecodeTesting
import scala.tools.testkit.ReleasablePath._
import scala.util.Using
import scala.util.chaining._

class QuickfixTest extends BytecodeTesting {

  private val allGood: StoreReporter.Info => Boolean = _ => true

  // expected result b is lines (of resulting files) with NL termination (i.e., trailing empty line)
  def testQuickfixes(as: List[String], b: String, args: String, checkInfo: StoreReporter.Info => Boolean = allGood): Unit =
    noWin() {
      Using.resource(Files.createTempDirectory("quickfixTest")) { tmpDir =>
        val srcs = as.zipWithIndex.map {
          case (a, i) => tmpDir.resolve(s"unitSource$i.scala").tap(Files.write(_, a.getBytes))
        }
        val c = BytecodeTesting.newCompiler(extraArgs = args)
        val r = c.newRun()
        val fs = srcs.map(src => AbstractFile.getFile(src.toFile.getAbsolutePath))
        r.compileSources(fs.map(new BatchSourceFile(_)))
        assertEquals(b, srcs.flatMap(src => Files.readAllLines(src).asScala).mkString("", "\n", "\n"))
        if (checkInfo ne allGood)
          for (info <- c.global.reporter.asInstanceOf[StoreReporter].infos)
            assert(checkInfo(info), info)
      }
    }

  def testQuickfix(a: String, b: String, args: String, checkInfo: StoreReporter.Info => Boolean = _ => true): Unit =
    testQuickfixes(List(a), b, args, checkInfo)

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

  @Test def `do not lie about fixing`: Unit = {
    val a = "import foo.bar" // not found: object foo (not quickfixable; no -Wunused:imports)
    testQuickfix(a, a+"\n", "-quickfix:any", !_.msg.contains("[rewritten by -quickfix]"))
  }

  // https://github.com/scala/bug/issues/12941
  @Test def correctSourceInTypeCompleters(): Unit = {
    val a1s = List(
      """trait Trait[A] {
        |  def foo: Option[A]
        |}
        |object Test1 extends Trait[Any] {
        |  def foo = None
        |}
        |""".stripMargin,
      """object Test2 {
        |  def one = Test1.foo
        |}
        |""".stripMargin)
    val b1 =
      """trait Trait[A] {
        |  def foo: Option[A]
        |}
        |object Test1 extends Trait[Any] {
        |  def foo: None.type = None
        |}
        |object Test2 {
        |  def one = Test1.foo
        |}
        |""".stripMargin
    testQuickfixes(a1s, b1, "-Xsource:3 -quickfix:any")

    val a2s = List(
      """object Test2 {
        |  def one = Test1.foo
        |}
        |""".stripMargin,
      """trait Trait[A] {
        |  def foo: Option[A]
        |}
        |object Test1 extends Trait[Any] {
        |  def foo = None
        |}
        |""".stripMargin)
    val b2 =
      """object Test2 {
        |  def one = Test1.foo
        |}
        |trait Trait[A] {
        |  def foo: Option[A]
        |}
        |object Test1 extends Trait[Any] {
        |  def foo: None.type = None
        |}
        |""".stripMargin
    testQuickfixes(a2s, b2, "-Xsource:3 -quickfix:any")
  }

  @Test def `named boolean literal lint has a fix`: Unit = {
    val a =
      sm"""|class C {
           |  def f(hasState: Boolean, isMutable: Boolean): Boolean = hasState && isMutable
           |  def test = f(true, false)
           |}
           |"""
    val b =
      sm"""|class C {
           |  def f(hasState: Boolean, isMutable: Boolean): Boolean = hasState && isMutable
           |  def test = f(hasState = true, isMutable = false)
           |}
           |"""
    testQuickfix(a, b, "-Wunnamed-boolean-literal -quickfix:any")
  }

  @Test def `synthetic case companion used as a function error has a fix`: Unit = {
    val a =
      sm"""|case class C(i: Int)
           |object Test {
           |  def test = List(1, 2, 3).map(C)
           |}
           |"""
    val b =
      sm"""|case class C(i: Int)
           |object Test {
           |  def test = List(1, 2, 3).map(C.apply)
           |}
           |"""
    testQuickfix(a, b, "-Xsource:3 -quickfix:any")
  }

  @Test def `unused import is quickfixable`: Unit = {
    val precedingNL =
      sm"""|import java.lang.Runnable
           |import java.lang.Thread
           |class C extends Runnable { def run() = () }
           |"""
    val resPrecedingNL =
      sm"""|import java.lang.Runnable
           |class C extends Runnable { def run() = () }
           |"""
    val noPrecedingNL =
      sm"""|import java.lang.Thread
           |class C
           |"""
    val resNoPrecedingNL =
      sm"""|class C
           |"""
    val multiplePrefix =
      sm"""|import java.lang.Runnable
           |import java.lang.Thread
           |class C
           |"""
    val multipleSuffix = // competing edits to delete line separator, was AIOOB in insertEdits
      sm"""|class C
           |import java.lang.Runnable
           |import java.lang.Thread"""
    testQuickfix(precedingNL, resPrecedingNL, "-Wunused:imports -quickfix:any")
    testQuickfix(noPrecedingNL, resNoPrecedingNL, "-Wunused:imports -quickfix:any")
    testQuickfix(multiplePrefix, resNoPrecedingNL, "-Wunused:imports -quickfix:any")
    testQuickfix(multipleSuffix, resNoPrecedingNL, "-Wunused:imports -quickfix:any")
  }

  @Test def `unused imports are selectively quickfixable`: Unit = {
    val keepEither =
      sm"""|import java.lang.{Runnable, Thread}
           |class C extends Runnable { def run() = () }
           |"""
    val keptEither =
      sm"""|import java.lang.Runnable
           |class C extends Runnable { def run() = () }
           |"""
    val keepOne =
      sm"""|import java.lang.Runnable, java.lang.Thread
           |class C extends Runnable { def run() = () }
           |"""
    val keptOne =
      sm"""|import java.lang.Runnable
           |class C extends Runnable { def run() = () }
           |"""
    val keepOther =
      sm"""|import java.lang.Thread, java.lang.Runnable
           |class C extends Runnable { def run() = () }
           |"""
    val keepOtherSpacey =
      sm"""|import java.lang.Thread  , java.lang.Runnable
           |class C extends Runnable { def run() = () }
           |"""
    val keepNeither =
      sm"""|import java.lang.Thread, java.lang.Runnable
           |class C
           |"""
    val keepNothing =
      sm"""|import java.lang.{Runnable, Thread}
           |class C
           |"""
    val keptNothing =
      sm"""|class C
           |"""
    val keepTwo =
      sm"""|import java.lang.{Runnable, System, Thread}, System.out
           |class C extends Runnable { def run() = out.println() }
           |"""
    val keptTwo =
      sm"""|import java.lang.{Runnable, System}, System.out
           |class C extends Runnable { def run() = out.println() }
           |"""
    testQuickfix(keepEither, keptEither, "-Wunused:imports -quickfix:any")
    testQuickfix(keepOne, keptOne, "-Wunused:imports -quickfix:any")
    testQuickfix(keepOther, keptOne, "-Wunused:imports -quickfix:any")
    testQuickfix(keepOtherSpacey, keptOne, "-Wunused:imports -quickfix:any")
    testQuickfix(keepNeither, keptNothing, "-Wunused:imports -quickfix:any")
    testQuickfix(keepNothing, keptNothing, "-Wunused:imports -quickfix:any")
    testQuickfix(keepTwo, keptTwo, "-Wunused:imports -quickfix:any")
  }

  @Test def `unused imports respects your weird formatting`: Unit = {
    val keepColumns =
      sm"""|import java.lang.{Runnable,
           |                  Thread,
           |                  System}, System.out
           |class C extends Runnable { def run() = out.println() }
           |"""
    val keptColumns =
      sm"""|import java.lang.{Runnable,
           |                  System}, System.out
           |class C extends Runnable { def run() = out.println() }
           |"""
    val overlapping =
      sm"""|import java.lang.{AutoCloseable,
           |                  Thread,
           |                  Runnable,
           |                  System}, System.out
           |class C extends Runnable { def run() = out.println() }
           |"""
    val lapped =
      sm"""|import java.lang.{
           |                  Runnable,
           |                  System}, System.out
           |class C extends Runnable { def run() = out.println() }
           |"""
    testQuickfix(keepColumns, keptColumns, "-Wunused:imports -quickfix:any")
    testQuickfix(overlapping, lapped, "-Wunused:imports -quickfix:any")
  }
}

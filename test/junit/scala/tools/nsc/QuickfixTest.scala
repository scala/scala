package scala.tools.nsc

import org.junit.Assert._
import org.junit.Test

import java.nio.file.Files
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.AbstractFile
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.testkit.AssertUtil._
import scala.tools.testkit.BytecodeTesting
import scala.tools.testkit.ReleasablePath._
import scala.util.Using

class QuickfixTest extends BytecodeTesting {

  def testQuickfixs(as: List[String], b: String, args: String, checkInfo: StoreReporter.Info => Boolean = _ => true): Unit =
    noWin() {
      Using.resource(Files.createTempDirectory("quickfixTest")) { tmpDir =>
        val srcs = as.indices.map(i => tmpDir.resolve(s"unitSource$i.scala")).toList
        as.lazyZip(srcs).foreach { case (a, src) => Files.write(src, a.getBytes) }
        val c = BytecodeTesting.newCompiler(extraArgs = args)
        val r = c.newRun()
        val fs = srcs.map(src => AbstractFile.getFile(src.toFile.getAbsolutePath))
        r.compileSources(fs.map(new BatchSourceFile(_)))
        assertEquals(b.replaceAll("\n+", "\n"), srcs.map(src => new String(Files.readAllBytes(src))).mkString("\n").replaceAll("\n+", "\n"))
        for (info <- c.global.reporter.asInstanceOf[StoreReporter].infos)
          assert(checkInfo(info), info)
      }
    }

  def testQuickfix(a: String, b: String, args: String, checkInfo: StoreReporter.Info => Boolean = _ => true): Unit =
    testQuickfixs(List(a), b, args, checkInfo)

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
    val a = "import foo.bar"
    testQuickfix(a, a, "-quickfix:any", !_.msg.contains("[rewritten by -quickfix]"))
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
    testQuickfixs(a1s, b1, "-Xsource:3 -quickfix:any")

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
    testQuickfixs(a2s, b2, "-Xsource:3 -quickfix:any")
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
}

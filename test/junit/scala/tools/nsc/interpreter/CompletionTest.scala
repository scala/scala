package scala.tools.nsc.interpreter

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test

import java.io.{PrintWriter, StringWriter}
import scala.reflect.internal.util.{BatchSourceFile, SourceFile}
import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.shell._

class CompletionTest {
  val EmptyString = "" // def string results include the empty string so that JLine won't insert "def ..." at the cursor

  def newIMain(classBased: Boolean = false): IMain = {
    val settings = new Settings()
    settings.Xnojline.value = true
    settings.usejavacp.value = true
    settings.Yreplclassbased.value = classBased

    new IMain(settings, new ReplReporterImpl(settings, new PrintWriter(new StringWriter)))
  }

  private def setup(sources: SourceFile*): Completion = {
    val intp = newIMain()
    intp.compileSources(sources: _*)
    val completer = new ReplCompletion(intp)
    completer
  }

  private def interpretLines(lines: String*): (Completion, Repl, Accumulator) = {
    val intp = newIMain()
    lines.foreach(intp.interpret)
    val acc = new Accumulator
    val completer = new ReplCompletion(intp, acc)
    (completer, intp, acc)
  }

  private def commandInterpretLines(): (Completion, Repl, Accumulator) = {
    val intp = newIMain()
    class CommandMock extends LoopCommands {
      override protected def echo(msg: String): Unit = ???
      override protected def out: PrintWriter = ???
      override def commands: List[LoopCommand] = {
        val default = (string: String) => Result.default
        List(
          LoopCommand.cmd("paste", "[-raw] [path]", "enter paste mode or paste a file", default),
          LoopCommand.cmd("paste", "[-raw] [path]", "enter paste mode or paste a file", default)// Other commands
          )
      }
    }
    val acc             = new Accumulator
    val shellCompletion = new Completion {
      override def complete(buffer: String, cursor: Int, filter: Boolean) =
        if (buffer.startsWith(":")) new CommandMock().colonCompletion(buffer, cursor).complete(buffer, cursor)
        else NoCompletions
    }
    (shellCompletion, intp, acc)
  }

  implicit class BeforeAfterCompletion(completion: Completion) {
    def complete(before: String, after: String = ""): CompletionResult =
      completion.complete(before + after, before.length)
  }

  @Test
  def t4438_arrayCompletion(): Unit = {
    val completer = setup()
    checkExact(completer, "Array(1, 2, 3) rev")("reverse", "reverseIterator", "reverseMap")
  }

  @Test
  def classBased(): Unit = {
    val intp = newIMain()
    val completer = new ReplCompletion(intp)
    checkExact(completer, "object O { def x_y_z = 1 }; import O._; x_y")("x_y_z")
  }

  @Test
  def completions(): Unit = {
    testCompletions(classBased = false)
  }

  @Test
  def completionsReplClassBased(): Unit = {
    testCompletions(classBased = true)
  }

  private def testCompletions(classBased: Boolean): Unit = {
    val intp = newIMain(classBased)
    val completer = new ReplCompletion(intp)

    checkExact(completer, "object O { def x_y_z = 1 }; import O._; x_y")("x_y_z")
    checkExact(completer, "object O { private def x_y_z = 1 }; import O._; x_y")()
    checkExact(completer, "object O { private def x_y_z = 1; x_y", "}")("x_y_z")
    checkExact(completer, "object x_y_z; import x_y")("x_y_z")

    checkExact(completer, "object x_y_z { def a_b_c }; import x_y_z.a_b")("a_b_c")

    checkExact(completer, "object X { private[this] def definition = 0; def")("definition")

    // stable terms are offered in type completion as they might be used as a prefix
    checkExact(completer, """object O { def x_y_z = 0; val x_z_y = ""; type T = x_""")("x_z_y")
    checkExact(completer, """def method { def x_y_z = 0; val x_z_y = ""; type T = x_""")("x_z_y")

    checkExact(completer, "asInstanceO", includeUniversal = false)()
    checkExact(completer, "asInstanceO", "", includeUniversal = true)("asInstanceOf")

    // Output is sorted
    assertEquals(List("prefix_aaa", "prefix_nnn", "prefix_zzz"), completer.complete( """class C { def prefix_nnn = 0; def prefix_zzz = 0; def prefix_aaa = 0; prefix_""").candidates.filter(!_.isUniversal).map(_.name))

    // Enable implicits to check completion enrichment
    checkExact(completer, """'c'.toU""")("toUpper")
    checkExact(completer, """val c = 'c'; c.toU""")("toUpper")

    intp.interpret("object O { def x_y_x = 1; def x_y_z = 2; def getFooBarZot = 3}; ")
    checkExact(new ReplCompletion(intp), """object O2 { val x = O.""")("x_y_x", "x_y_z", "getFooBarZot")
  }

  @Test
  def annotations(): Unit = {
    val completer = setup()
    checkExact(completer, "def foo[@specialize", " A]")("specialized")
    checkExact(completer, "def foo[@specialize")("specialized")
    checkExact(completer, """@deprecatedN""", """ class Foo""")("deprecatedName")
    checkExact(completer, """{@deprecatedN""")("deprecatedName")
  }

  @Test
  def incompleteStringInterpolation(): Unit = {
    val completer = setup()
    checkExact(completer, """val x_y_z = 1; s"${x_""", "}\"")("x_y_z")
    checkExact(completer, """val x_y_z = 1; s"${x_""", "\"")("x_y_z")
  }

  @Test
  def symbolically(): Unit = {
    val completer = setup()
    checkExact(completer, """class C { def +++(a: Any) = 0; def ---(a: Any) = 0; this.++""")("+++")
  }

  @Test
  def previousLineCompletions(): Unit = {
    val (completer, intp, _) = interpretLines(
      "class C { val x_y_z = 42 }",
      "object O { type T = Int }")

    checkExact(completer, "new C().x_y")("x_y_z")
    checkExact(completer, "(1 : O.T).toCha")("toChar")

    intp.interpret("case class X_y_z()")
    val completer1 = new ReplCompletion(intp)
    checkExact(completer1, "new X_y_")("X_y_z")
    checkExact(completer1, "X_y_")("X_y_z")
    checkExact(completer1, "X_y_z.app")("apply")
  }

  @Test
  def previousResultInvocation(): Unit = {
    val (completer, _, _) = interpretLines("1 + 1")

    checkExact(completer, ".toCha")("toChar")
  }

  @Test
  def multiLineInvocation(): Unit = {
    val (completer, _, accumulator) = interpretLines()
    accumulator += "class C {"
    checkExact(completer, "1 + 1.toCha")("toChar")
  }

  @Test
  def defStringConstructor(): Unit = {
    val intp = newIMain()
    val completer = new ReplCompletion(intp)
    // : String to workaround https://github.com/scala/bug/issues/11964
    checkExact(completer, "class Shazam(i: Int); new Shazam",  result = _.declString())("def <init>(i: Int): Shazam" : String)
    checkExact(completer, "class Shazam(i: Int) { def this(x: String) = this(0) }; new Shazam", result = _.declString())("def <init>(i: Int): Shazam", "def <init>(x: String): Shazam": String)
  }

  @Test
  def treePrint(): Unit = {
    val completer = setup()
    checkExact(completer, " 1.toHexString //print")(EmptyString, "scala.Predef.intWrapper(1).toHexString // : String")
  }

  @Test
  def replGeneratedCodeDeepPackages(): Unit = {
    val completer = setup(new BatchSourceFile("<paste>", "package p1.p2.p3; object Ping { object Pong }"))
    checkExact(completer, "p1.p2.p")("p3")
    checkExact(completer, "p1.p2.p3.P")("Ping")
    checkExact(completer, "p1.p2.p3.Ping.Po")("Pong")
  }

  @Test
  def constructor(): Unit = {
    val intp = newIMain()
    val completer = new ReplCompletion(intp)
    checkExact(completer, "class Shazam{}; new Shaz")("Shazam")

    intp.interpret("class Shazam {}")
    checkExact(completer, "new Shaz")("Shazam")
  }

  @Test
  def completionWithComment(): Unit = {
    val completer = setup()

    val withMultilineCommit =
      """|Array(1, 2, 3)
         |  .map(_ + 1) /* then we do reverse */
         |  .rev""".stripMargin
    assertTrue(
      completer.complete(withMultilineCommit).candidates.map(_.name).contains("reverseMap")
    )

    val withInlineCommit =
      """|Array(1, 2, 3)
         |  .map(_ + 1) // then we do reverse
         |  .rev""".stripMargin
    assertTrue(
      completer.complete(withInlineCommit).candidates.map(_.name).contains("reverseMap")
    )
  }

  @Test
  def isDeprecated(): Unit = {
    val (completer, _, _) = interpretLines(
      """object Stale { @deprecated("","") def oldie = ??? }""",
      """object Stuff { @deprecated("","") def `this` = ??? ; @deprecated("","") def `that` = ??? }"""
    )
    val candidates1 = completer.complete("Stale.ol").candidates
    assertEquals(1, candidates1.size)
    assertTrue(candidates1.forall(_.isDeprecated))
    val candidates2 = completer.complete("Stuff.th").candidates
    assertEquals(2, candidates2.size)
    assertTrue(candidates2.forall(_.isDeprecated))
  }

  @Test
  def isDeprecatedOverrideMethod(): Unit = {
    val (completer, _, _) = interpretLines(
      """object Stale { def oldie(i: Int) = ???; @deprecated("","") def oldie = ??? }"""
      )
    val candidates1 = completer.complete("Stale.ol").candidates
    assertEquals(2, candidates1.size)
    // Our JLine Reader is now responsible for only displaying @deprecated if all candidates with the name are
    // deprecated. That isn't covered by this test.
    assertEquals(candidates1.head.isDeprecated, true)
    assertEquals(candidates1.last.isDeprecated, false)
  }

  @Test
  def isDeprecatedOverrideMethodDefString(): Unit = {
    val (completer, _, _) = interpretLines(
      """object Stale { def oldie(i: Int) = ???; @deprecated("","") def oldie = ??? }"""
      )
    val candidates1 = completer.complete("Stale.oldie").candidates
    assertEquals(2, candidates1.size)
    assertEquals(candidates1.filter(_.isDeprecated).map(_.declString().contains("deprecated")).head, true)
    assertEquals(candidates1.last.isDeprecated, false)
  }

  @Test
  def isDeprecatedInMethodDesc(): Unit = {
    val (completer, _, _) = interpretLines(
      """object Stale { @deprecated("","") def oldie = ??? }""",
      """object Stuff { @deprecated("","") def `this` = ??? ; @deprecated("","") def `that` = ??? }"""
      )
    val candidates1 = completer.complete("Stale.oldie").candidates
    assertEquals(1, candidates1.size) // When exactly matched, there is an empty character
    assertTrue(candidates1.filter(_.declString().contains("oldie")).head.declString().contains("deprecated"))
    val candidates2 = completer.complete("Stuff.that").candidates
    assertEquals(1, candidates2.size)
    assertTrue(candidates2.filter(_.declString().contains("that")).head.declString().contains("deprecated"))
  }

  @Test
  def jline3Matcher(): Unit = {
    val (completer, _, _) = commandInterpretLines()
    val candidates1 = completer.complete(":p").candidates
    assertEquals(2, candidates1.size)

    // Save the line to the CompletionResult of the matcher, and select the command to match successfully.
    val completionResult = completer.complete(":p")
    assertEquals(completionResult.line, ":p")
  }

  @Test
  def isNotDeprecated(): Unit = {
    val (completer, _, _) = interpretLines(
      """object Stuff { def `this` = ??? ; def `that` = ??? }"""
    )
    val candidates = completer.complete("Stuff.th").candidates
    assertEquals(2, candidates.size)
    assert(candidates.forall(!_.isDeprecated), "No deprecations")
  }

  @Test
  def importTypesAndTermsBoth(): Unit = {
    val (completer, _, _) = interpretLines(
      """object A { class Type; object Term }"""
    )
    val candidates1 = completer.complete("A.T").candidates
    assertEquals("Term", candidates1.map(_.name).mkString(" "))
    val candidates2 = completer.complete("import A.T").candidates
    assertEquals("Term Type", candidates2.map(_.name).sorted.mkString(" "))
  }

  @Test
  def dependentTypeImplicits_t10353(): Unit = {
    val code =
      """
package test

// tests for autocomplete on repl

object Test {
  trait Conv[In] {
    type Out
    def apply(in: In): Out
  }
  object Conv {
    type Aux[In, Out0] = Conv[In] { type Out = Out0 }
    implicit val int2String = new Conv[Int] {
      type Out = String
      override def apply(i: Int) = i.toString
    }
  }

  // autocomplete works on repl: `test.Test.withParens().<TAB>` shows completions for String
  def withParens[Out]()(implicit conv: Conv.Aux[Int, Out]): Out = "5".asInstanceOf[Out]

  // autocomplete doesn't work on repl: `test.Test.withoutParens.` doesn't suggest anything
  // when saving intermediate result it works though: `val a = test.Test.withoutParens; a.<TAB>`
  def withoutParens[Out](implicit conv: Conv.Aux[Int, Out]): Out = "5".asInstanceOf[Out]
}

// this works fine
object Test2 {
  trait A
  implicit val a: A = ???
  def withParens()(implicit a: A): String = "something"
  def withoutParens(implicit a: A): String = "something"
}
"""
    val completer = setup(new BatchSourceFile("<paste>", code))
    checkExact(completer, "val x = test.Test.withoutParens; x.charA")("charAt")
    checkExact(completer, "test.Test.withoutParens.charA")("charAt")
  }

  def checkExact(completer: Completion, before: String, after: String = "", includeUniversal: Boolean = false,
                 result: CompletionCandidate => String = _.name)(expected: String*): Unit = {
    val candidates  = completer.complete(before, after).candidates
                          .filter(c => includeUniversal || !c.isUniversal)
    val actual = candidates.map(result)
    assertEquals(expected.sorted.mkString(" "), actual.toSeq.distinct.sorted.mkString(" "))
  }

  @Test
  def ignoreAlias(): Unit = {
    val (completer, _, _) = interpretLines(
      """class Foo(i: Int) { def this(s: String) = this(s.toInt) }""",
      """type Bar = Foo"""
      )
    // We not only keep the original `type Bar = Bar`, but also add more detailed candidates
    val candidates       = completer.complete("new Bar").candidates
    //type Bar = Bar
    //def <init>(i: Int): Foo
    //def <init>(s: String): Foo
    assertEquals(3, candidates.size)
    assertEquals("type Bar = Bar", candidates.head.declString.apply())
    assertEquals("def <init>(i: Int): Foo", candidates(1).declString.apply())
    assertEquals("def <init>(s: String): Foo", candidates(2).declString.apply())

    val candidates1       = completer.complete("new Foo").candidates
    assertEquals(2, candidates1.size)
  }

}

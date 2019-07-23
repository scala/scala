package scala.tools.nsc.scaladoc

import java.io.StringWriter

import org.scalacheck.Prop._
import org.scalacheck._

import scala.collection.mutable
import scala.tools.nsc.doc.html.HtmlPage


object HtmlFactoryTest extends Properties("HtmlFactory") {

  final val RESOURCES = "test/scaladoc/resources/"

  import scala.tools.nsc.ScalaDocReporter
  import scala.tools.nsc.doc.html.HtmlFactory
  import scala.tools.nsc.doc.{DocFactory, Settings}

  def createFactory: DocFactory = {
    val settings = new Settings(Console.err.println)
    settings.scaladocQuietRun = true
    settings.nowarn.value = true
    SettingsUtil.configureClassAndSourcePath(settings)
    settings.docAuthor.value = true

    val reporter = new scala.tools.nsc.reporters.ConsoleReporter(settings)
    new DocFactory(reporter, settings)
  }

  import scala.tools.nsc.doc.html.HtmlTags.{textOf, Elem => Node, Elems => NodeSeq}

  def createTemplates(basename: String): collection.Map[String, HtmlPage] = {
    val result = mutable.Map[String, HtmlPage]()

    val path: String = SettingsUtil.checkoutRoot.resolve(RESOURCES).resolve(basename).toAbsolutePath.toString
    createFactory.makeUniverse(Left(List(path))) match {
      case Some(universe) =>
        new HtmlFactory(universe, new ScalaDocReporter(universe.settings)).writeTemplates{ page =>
          result += (page.absoluteLinkTo(page.path) -> page)
        }
      case _ =>
    }

    result
  }


  /**
   * This tests the text without the markup - ex:
   *
   * <h4 class="signature">
   *  <span class="modifier_kind">
   *    <span class="modifier">implicit</span>
   *    <span class="kind">def</span>
   *  </span>
   *  <span class="symbol">
   *    <span class="name">test</span><span class="params">()</span><span class="result">: <span name="scala.Int" class="extype">Int</span></span>
   *  </span>
   *  </h4>
   *
   * becomes:
   *
   *  implicit def test(): Int
   *
   * and is required to contain the text in the given checks
   *
   * NOTE: Comparison is done ignoring all whitespace
   */
  def checkText(scalaFile: String, debug: Boolean = true)(checks: (Option[String], String, Boolean)*): Boolean = {
    val htmlFile = scalaFile.stripSuffix(".scala") + ".html"
    val htmlAllFiles = createTemplates(scalaFile)
    var result = true

    for ((fileHint, check, expected) <- checks) {
      // resolve the file to be checked
      val fileName = fileHint match {
        case Some(file) =>
          if (file endsWith ".html")
            file
          else
            file + ".html"
        case None =>
          htmlFile
      }
      val fileTextPretty = textOf(htmlAllFiles(fileName).body).replace('→',' ').replaceAll("\\s+"," ")
      val fileText = fileTextPretty.replaceAll(" ", "")

      val checkTextPretty = check.replace('→',' ').replaceAll("\\s+"," ")
      val checkText = checkTextPretty.replaceAll(" ", "")

      val checkValue = fileText.contains(checkText) == expected
      if (debug && (!checkValue)) {
        Console.err.println("")
        Console.err.println("HTML Check failed for resource file " + scalaFile + ":")
        Console.err.println("Could not match: \n" + checkTextPretty)
        Console.err.println("In the extracted HTML text: \n" + fileTextPretty)
        Console.err.println("NOTE: The whitespaces are eliminated before matching!")
        Console.err.println("")
      }
      result &&= checkValue
    }

    result
  }

  private def checkTemplate(base: String, file: String)(check: (collection.Map[String, HtmlPage], String) => Boolean): Boolean = {
    val files = createTemplates(base)
    files.get(file).exists { page => check(files, toHtml(page)) }
  }

  private def toHtml(page: HtmlPage) = {
    val sw = new StringWriter()
    page.writeHtml("dummy encoding")(sw) // encoding is not really needed
    sw.toString
  }

  private def checkTemplate(base: String)(check: String => Boolean): Boolean =
    checkTemplate(base, base.stripSuffix(".scala") + ".html") { (_, s) => check(s) }


  property("Trac #3790") = {
    checkTemplate("Trac3790.scala") { _.contains(
      """<p class="shortcomment cmt">A lazy String
        |<p>""".stripMargin) }
    checkTemplate("Trac3790.scala") { _.contains(
      """<p class="shortcomment cmt">A non-lazy String
        |</p>""".stripMargin) }
  }

  property("Trac #4306") = {
    checkTemplate("Trac4306.scala", "com/example/trac4306/foo/package$$Bar.html") { (_, _) => true }
  }

  property("Trac #4366") = {
    checkTemplate("Trac4366.scala") { html =>
      html.contains("""<p class="shortcomment cmt"><strong><code>foo</code>""")
    }
  }


  property("Trac #4358") = {
    checkTemplate("Trac4358.scala") { html => !html.contains("<em>i.</em>") }
  }

  property("Trac #4180") = {
    checkTemplate("Trac4180.scala") { _ => true }
  }

  property("Trac #4372") = {
    checkTemplate("Trac4372.scala") { html =>
      html.contains("""<span class="name" title="gt4s: $plus$colon">+:</span>""") &&
        html.contains("""<span class="name" title="gt4s: $minus$colon">-:</span>""") &&
          html.contains("""<span class="params">(<span name="n">n: <span name="scala.Int" class="extype">Int</span></span>)</span><span class="result">: <span name="scala.Int" class="extype">Int</span></span>""")
    }
  }

  property("Trac #4374 - public") = {
    checkTemplate("Trac4374.scala", "WithPublic.html"){(files, s) =>
      s.contains("""href="WithPublic$.html"""") &&
      files.get("WithPublic$.html").isDefined
    }
  }

  property("Trac #4374 - private") = {
    checkTemplate("Trac4374.scala", "WithPrivate.html"){ (files, s) =>
      !s.contains("""href="WithPrivate$.html"""") &&
      files.get("WithPrivate$.html").isEmpty
    }
  }

  property("Trac #4325 - files") = {
    val files = createTemplates("Trac4325.scala")

    files.get("WithSynthetic.html").isDefined &&
    files.get("WithSynthetic$.html").isEmpty &&
    files.get("WithObject.html").isDefined &&
    files.get("WithObject$.html").isDefined
  }

  property("Trac #4325 - Don't link to syntetic companion") = {
    checkTemplate("Trac4325.scala", "WithSynthetic.html"){ (_, s) =>
        ! s.contains("""href="WithSynthetic$.html"""")
    }
  }

  property("Trac #4325 - Link to companion") = {
    checkTemplate("Trac4325.scala", "WithObject.html") { (_, s) =>
      s.contains("""href="WithObject$.html"""")
    }
  }

  property("Trac #4420 - no whitespace at end of line") = {
    checkTemplate("Trac4420.scala", "TestA.html") { (_, s) =>
      s.contains("""See YYY for more details""")
    }
  }
  //
  // property("Trac #484 - refinements and existentials") = {
  //   val files = createTemplates("Trac484.scala")
  //   val lines = """
  //       |type Bar = AnyRef { type Dingus <: T forSome { type T <: String } }
  //       |type Foo = AnyRef { ... /* 3 definitions in type refinement */ }
  //       |def g(x: T forSome { type T <: String }): String
  //       |def h(x: Float): AnyRef { def quux(x: Int,y: Int): Int }
  //       |def hh(x: Float): AnyRef { def quux(x: Int,y: Int): Int }
  //       |def j(x: Int): Bar
  //       |def k(): AnyRef { type Dingus <: T forSome { type T <: String } }
  //     """.stripMargin.trim.lines map (_.trim)
  //
  //   files("RefinementAndExistentials.html") match {
  //     case node: scala.xml.Node => {
  //       val s = node.text.replaceAll("\\s+", " ")
  //       lines forall (s contains _)
  //     }
  //     case _ => false
  //   }
  // }

  property("Trac #4289") = {
    checkTemplate("Trac4289.scala", "Subclass.html") { (_, s) =>
      s.contains("""<dt>returns</dt><dd class="cmt"><p>123</p></dd>""")
    }
  }

  property("Trac #4409") = {
    checkTemplate("Trac4409.scala") {s => ! s.contains("""<div class="block"><ol>since""") }
  }

  property("Trac #4452") = {
    checkTemplate("Trac4452.scala") { s => ! s.contains(">*") }
  }

  property("scala/bug#4421") = {
    checkTemplate("t4421.scala") { html =>
      html.contains(">Example:") && html.contains(">Note<")
    }
  }

  property("scala/bug#4589") = {
    checkTemplate("t4589.scala") { html =>
      html.contains(">x0123456789: <") &&
        html.contains(">x012345678901234567890123456789: <")
    }
  }

  property("scala/bug#4714: Should decode symbolic type alias name.") = {
    checkTemplate("t4715.scala") { html =>
      html.contains(">:+:<")
    }
  }

  property("scala/bug#4287: Default arguments of synthesized constructor") = {
    checkTemplate("t4287.scala", "ClassWithSugar.html") { (_, s) => s.contains(">123<") }
  }

  property("scala/bug#4507: Default arguments of synthesized constructor") = {
    checkTemplate("t4507.scala") { s =>
      ! s.contains("<li>returns silently when evaluating true and true</li>")
    }
  }

  property("scala/bug#4898: Use cases and links should not crash scaladoc") = {
    checkTemplate("t4898.scala"){ _ => true}
  }

  property("scala/bug#5054: Use cases should override their original members") =
     checkText("t5054_q1.scala")(
       (None,"""def test(): Int""", true)
       //Disabled because the full signature is now displayed
       //(None, """def test(implicit lost: Int): Int""", false)
     )

  property("scala/bug#5054: Use cases should keep their flags - final should not be lost") =
    checkText("t5054_q2.scala")((None, """final def test(): Int""", true))

  property("scala/bug#5054: Use cases should keep their flags - implicit should not be lost") =
    checkText("t5054_q3.scala")((None, """implicit def test(): Int""", true))

  property("scala/bug#5054: Use cases should keep their flags - real abstract should not be lost") =
    checkText("t5054_q4.scala")((None, """abstract def test(): Int""", true))

  property("scala/bug#5054: Use cases should keep their flags - traits should not be affected") =
    checkText("t5054_q5.scala")((None, """def test(): Int""", true))

  property("scala/bug#5054: Use cases should keep their flags - traits should not be affected") =
    checkText("t5054_q6.scala")((None, """abstract def test(): Int""", true))

  property("scala/bug#5054: Use case individual signature test") =
    checkText("t5054_q7.scala")(
      (None, """abstract def test2(explicit: Int): Int [use case] This takes the explicit value passed.""", true),
      (None, """abstract def test1(): Int [use case] This takes the implicit value in scope.""", true)
    )

  property("scala/bug#5287: Display correct \"Definition classes\"") =
    checkText("t5287.scala")(
      (None,
          """def method(): Int
           [use case] The usecase explanation
           [use case] The usecase explanation
           Definition Classes t5287 t5287_B t5287_A""", true)
    )      // the explanation appears twice, as small comment and full comment

  property("Comment inheritance: Correct comment inheritance for overriding") =
    checkText("implicit-inheritance-override.scala")(
      (Some("Base"),
       """def function[T](arg1: T, arg2: String): Double
          The base comment.
          The base comment. And another sentence...
          T the type of the first argument
          arg1 The T term comment
          arg2 The string comment
          returns The return comment
          """, true),
      (Some("DerivedA"),
       """def function[T](arg1: T, arg2: String): Double
          Overriding the comment, the params and returns comments should stay the same.
          Overriding the comment, the params and returns comments should stay the same.
          T the type of the first argument
          arg1 The T term comment
          arg2 The string comment
          returns The return comment
          """, true),
      (Some("DerivedB"),
       """def function[T](arg1: T, arg2: String): Double
          T the type of the first argument
          arg1 The overridden T term comment
          arg2 The overridden string comment
          returns The return comment
          """, true),
      (Some("DerivedC"),
       """def function[T](arg1: T, arg2: String): Double
          T the type of the first argument
          arg1 The T term comment
          arg2 The string comment
          returns The overridden return comment
          """, true),
      (Some("DerivedD"),
       """def function[T](arg1: T, arg2: String): Double
          T The overridden type parameter comment
          arg1 The T term comment
          arg2 The string comment
          returns The return comment
          """, true)
    )

  for (useCaseFile <- List("UseCaseInheritance", "UseCaseOverrideInheritance")) {
    property("Comment inheritance: Correct comment inheritance for usecases") =
      checkText("implicit-inheritance-usecase.scala")(
        (Some(useCaseFile),
         """def missing_arg[T](arg1: T): Double
            [use case]
            [use case]
            T The type parameter
            arg1 The T term comment
            returns The return comment
            """, true),
        (Some(useCaseFile),
         """def missing_targ(arg1: Int, arg2: String): Double
            [use case]
            [use case]
            arg1 The T term comment
            arg2 The string comment
            returns The return comment
            """, true),
        (Some(useCaseFile),
         """def overridden_arg1[T](implicit arg1: T, arg2: String): Double
            [use case]
            [use case]
            T The type parameter
            arg1 The overridden T term comment
            arg2 The string comment
            returns The return comment
            """, true),
        (Some(useCaseFile),
         """def overridden_targ[T](implicit arg1: T, arg2: String): Double
            [use case]
            [use case]
            T The overridden type parameter comment
            arg1 The T term comment
            arg2 The string comment
            returns The return comment
            """, true),
        (Some(useCaseFile),
         """def overridden_return[T](implicit arg1: T, arg2: String): Double
            [use case]
            [use case]
            T The type parameter
            arg1 The T term comment
            arg2 The string comment
            returns The overridden return comment
            """, true),
        (Some(useCaseFile),
         """def added_arg[T](implicit arg1: T, arg2: String, arg3: Float): Double
            [use case]
            [use case]
            T The type parameter
            arg1 The T term comment
            arg2 The string comment
            arg3 The added float comment
            returns The return comment
            """, true),
        (Some(useCaseFile),
         """def overridden_comment[T](implicit arg1: T, arg2: String): Double
            [use case] The overridden comment.
            [use case] The overridden comment.
            T The type parameter
            arg1 The T term comment
            arg2 The string comment
            returns The return comment
            """, true)
      )
  }

  property("Comment inheritance: Correct explicit inheritance for override") =
  checkText("explicit-inheritance-override.scala")(
    (Some("InheritDocDerived"),
     """def function[T](arg1: T, arg2: String): Double
        Starting line
        Starting line
        The base comment. And another sentence...
        The base comment. And another sentence...
        Ending line
        Author: StartAuthor a Scala developer EndAuthor
          T       StartT the type of the first argument EndT
          arg1    Start1 The T term comment End1
          arg2    Start2 The string comment End2
          returns StartRet The return comment EndRet""", true),
    (Some("InheritDocDerived"),
     """Definition Classes InheritDocDerived → InheritDocBase
        Example:   StartExample function[Int](3, "something") EndExample
        Version    StartVer 0.0.2 EndVer
        Since      StartSince 0.0.1 EndSince
        Exceptions thrown
                   SomeException      StartEx if the function is not called with correct parameters EndEx
                   SomeOtherException StartSOE Should Warn <invalid inheritdoc annotation> EndSOE
        To do      StartTodo Call mom. And dad! EndTodo
        Note       StartNote Be careful! EndNote
        See also   StartSee The Manual EndSee
     """, true))

  property("Comment inheritance: Correct explicit inheritance for usecase") =
  checkText("explicit-inheritance-usecase.scala")(
    (Some("UseCaseInheritDoc"),
     """def function[T](arg1: T, arg2: String): Double
        [use case] Starting line
        [use case] Starting line
        The base comment. And another sentence...
        The base comment. And another sentence...
        Ending line
        Author: StartAuthor a Scala developer EndAuthor
          T       StartT the type of the first argument EndT
          arg1    Start1 The T term comment End1
          arg2    Start2 The string comment End2
          returns StartRet The return comment EndRet""", true),
    (Some("UseCaseInheritDoc"),
     """Example:   StartExample function[Int](3,"something") EndExample
        Version    StartVer 0.0.2 EndVer
        Since      StartSince 0.0.1 EndSince
        Exceptions thrown
                   SomeException      StartEx if the function is not called with correct parameters EndEx
                   SomeOtherException StartSOE Should Warn <invalid inheritdoc annotation> EndSOE
        To do      StartTodo Call mom. And dad! EndTodo
        Note       StartNote Be careful! EndNote
        See also   StartSee The Manual EndSee
     """, true))

  property("Comment inheritance: Correct explicit inheritance in corner cases") =
    checkText("inheritdoc-corner-cases.scala")(
      (Some("D"),
        """def hello1: Int
          Inherited: Hello 1 comment
          Inherited: Hello 1 comment
          Definition Classes D → A
       """, true),
      (Some("D"),
        """def hello2: Int
          Inherited: Hello 2 comment
          Inherited: Hello 2 comment
          Definition Classes D → B
       """, true),
      (Some("G"),
        """def hello1: Int
          Inherited: Hello 1 comment
          Inherited: Hello 1 comment
          Definition Classes G → D → A
       """, true),
      (Some("G"),
        """def hello2: Int
          Inherited: Hello 2 comment
          Inherited: Hello 2 comment
          Definition Classes G → D → B
       """, true),
      (Some("I"),
        """def hello1(i: Int): Unit
          [use case] Inherited: Hello 1 comment
          [use case] Inherited: Hello 1 comment
          Definition Classes I → G → D → A
       """, true)
      // traits E, F and H shouldn't crash scaladoc but we don't need to check the output
    )

  property("Indentation normalization for code blocks") = {
    checkTemplate("code-indent.scala", "C.html") { (_, s) =>
      s.contains("<pre>a typicial indented\ncomment on multiple\ncomment lines</pre>") &&
      s.contains("<pre>one liner</pre>") &&
      s.contains("<pre>two lines, one useful</pre>") &&
      s.contains("<pre>line1\nline2\nline3\nline4</pre>") &&
      s.contains("<pre>a ragged example\na (condition)\n  the t h e n branch\nan alternative\n  the e l s e branch</pre>") &&
      s.contains("<pre>Trait example {\n  Val x = a\n  Val y = b\n}</pre>") &&
      s.contains("<pre>l1\n\nl2\n\nl3\n\nl4\n\nl5</pre>")
    }
  }

  property("scala/bug#4014: Scaladoc omits @author: no authors") = {
    checkTemplate("t4014_0.scala", "Foo.html")( (_, s ) => ! s.contains("Author"))
  }

  property("scala/bug#4014: Scaladoc omits @author: one author") = {
    checkTemplate("t4014_1.scala", "Foo.html"){ (_, s) =>
      s.contains("<h6>Author:</h6>") &&
      s.contains("<p>The Only Author</p>")
    }
  }

  property("scala/bug#4014: Scaladoc omits @author: two authors") = {
    checkTemplate("t4014_2.scala", "Foo.html") { (_, s) =>
      s.contains("<h6>Authors:</h6>") &&
      s.contains("<p>The First Author</p>") &&
      s.contains("<p>The Second Author</p>")
    }
  }

  {
    val files = createTemplates("basic.scala")
    //println(files)

    property("class") = files.get("com/example/p1/Clazz.html").exists { page =>
      val html = toHtml(page)

      property("implicit conversion") = html contains """<span class="modifier">implicit </span>"""

      property("gt4s") = html contains """title="gt4s: $colon$colon""""

      property("gt4s of a deprecated method") = html contains """title="gt4s: $colon$colon$colon$colon. Deprecated: """

      true
    }

    property("package") = files.contains("com/example/p1/index.html")

    property("package object") = files.get("com/example/p1/index.html").exists { page =>
      val html = toHtml(page)

      html contains "com.example.p1#packageObjectMethod"
    }

    property("lower bound") = files.contains("com/example/p1/LowerBound.html")

    property("upper bound") = files.contains("com/example/p1/UpperBound.html")

    property("scala/bug#8514: No inconsistencies") =
      checkText("t8514.scala")(
        (Some("a/index"),
         """class A extends AnyRef
            Some doc here
            Some doc here
            Annotations @DeveloperApi()
         """, true),
        (Some("a/index"),
         """class B extends AnyRef
            Annotations @DeveloperApi()
         """, true)
      )
  }

  // scala/bug#8144
  {
    val files = createTemplates("t8144.scala")

    property("scala/bug#8144: Members' permalink - inner package") = files.get("some/pack/index.html").map { page => val html = toHtml(page)
      ("type link" |: html.contains("../../some/pack/index.html")) &&
        ("member: SomeType (object)" |: html.contains("""<a href="../../some/pack/SomeType$.html" title="Permalink">""")) &&
        ("member: SomeType (class)" |: html.contains("""<a href="../../some/pack/SomeType.html" title="Permalink">"""))
    }.getOrElse(Prop.falsified)

    property("scala/bug#8144: Members' permalink - companion object") = files.get("some/pack/SomeType$.html").map { page => val html = toHtml(page)
      ("type link" |: html.contains("../../some/pack/SomeType$.html")) &&
        ("member: someVal" |: html.contains("""<a href="../../some/pack/SomeType$.html#someVal:String" title="Permalink">"""))
    }.getOrElse(Prop.falsified)

    property("scala/bug#8144: Members' permalink - class") = files.get("some/pack/SomeType.html").map { page => val html = toHtml(page)
      (("type link" |: html.contains("../../some/pack/SomeType.html")) &&
      ("constructor " |: html.contains("""<span class="permalink"><a href="../../some/pack/SomeType.html#&lt;init&gt;(arg:String):some.pack.SomeType" title="Permalink">""")) &&
        ( "member: type TypeAlias" |: html.contains("""<span class="permalink"><a href="../../some/pack/SomeType.html#TypeAlias=String" title="Permalink">""")) &&
        ( "member: def >#<():Int " |: html.contains("""<span class="permalink"><a href="../../some/pack/SomeType.html#&gt;#&lt;():Int" title="Permalink">""")) &&
        ( "member: def >@<():TypeAlias " |: html.contains("""<span class="permalink"><a href="../../some/pack/SomeType.html#&gt;@&lt;():SomeType.this.TypeAlias" title="Permalink">""")))
    }.getOrElse(Prop.falsified)

  }

  property("scala/bug#9599 Multiple @todo formatted with comma on separate line") = {
    checkTemplate("t9599.scala", "X.html") { (_, s) => s.contains("""<span class="cmt"><p>todo1</p></span><span class="cmt"><p>todo2</p></span><span class="cmt"><p>todo3</p></span>""") }
  }
}

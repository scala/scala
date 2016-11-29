import org.scalacheck._
import org.scalacheck.Prop._

import java.net.{URLClassLoader, URLDecoder}
import scala.collection.mutable
import scala.xml.NodeSeq

object XMLUtil {
  import scala.xml._

  def stripGroup(seq: Node): Node = {
    seq match {
      case group: Group => {
        <div class="group">{ group.nodes.map(stripGroup _) }</div>
      }
      case e: Elem => {
        val child = e.child.map(stripGroup _)
        Elem(e.prefix, e.label, e.attributes, e.scope, child : _*)
      }
      case _ => seq
    }
  }
}

object Test extends Properties("HtmlFactory") {

  final val RESOURCES = "test/scaladoc/resources/"

  import scala.tools.nsc.ScalaDocReporter
  import scala.tools.nsc.doc.{DocFactory, Settings}
  import scala.tools.nsc.doc.html.HtmlFactory

  def getClasspath = {
    // these things can be tricky
    // this test previously relied on the assumption that the current thread's classloader is an url classloader and contains all the classpaths
    // does partest actually guarantee this? to quote Leonard Nimoy: The answer, of course, is no.
    // this test _will_ fail again some time in the future.
    // Footnote: java.lang.ClassCastException: org.apache.tools.ant.loader.AntClassLoader5 cannot be cast to java.net.URLClassLoader
    val loader = Thread.currentThread.getContextClassLoader.asInstanceOf[URLClassLoader]
    val paths = loader.getURLs.map(u => URLDecoder.decode(u.getPath))
    paths mkString java.io.File.pathSeparator
  }

  def createFactory = {
    val settings = new Settings({Console.err.println(_)})
    settings.scaladocQuietRun = true
    settings.nowarn.value = true
    settings.classpath.value = getClasspath
    settings.docAuthor.value = true

    val reporter = new scala.tools.nsc.reporters.ConsoleReporter(settings)
    new DocFactory(reporter, settings)
  }

  def createTemplates(basename: String): collection.Map[String, NodeSeq] = {
    val result = mutable.Map[String, NodeSeq]()

    createFactory.makeUniverse(Left(List(RESOURCES+basename))) match {
      case Some(universe) => {
        new HtmlFactory(universe, new ScalaDocReporter(universe.settings)).writeTemplates((page) => {
          result += (page.absoluteLinkTo(page.path) -> page.body)
        })
      }
      case _ =>
    }

    result
  }

  def createTemplate(scala: String) = {
    val html = scala.stripSuffix(".scala") + ".html"
    createTemplates(scala)(html)
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
      val fileTextPretty = htmlAllFiles(fileName).text.replace('→',' ').replaceAll("\\s+"," ")
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

  def shortComments(root: scala.xml.Node) =
    XMLUtil.stripGroup(root).descendant.flatMap {
      case e: scala.xml.Elem => {
        if (e.attribute("class").toString.contains("shortcomment")) {
          Some(e)
        } else {
          None
        }
      }
      case _ => None
    }

  property("Trac #3790") = {
    createTemplate("Trac3790.scala") match {
      case node: scala.xml.Node => {
        val comments = shortComments(node)

        comments.exists { _.toString.contains(">A lazy String\n</p>") } &&
          comments.exists { _.toString.contains(">A non-lazy String\n</p>") }
      }
      case _ => false
    }
  }

  property("Trac #4306") = {
    val files = createTemplates("Trac4306.scala")
    files("com/example/trac4306/foo/package$$Bar.html") != None
  }

  property("Trac #4366") = {
    createTemplate("Trac4366.scala") match {
      case node: scala.xml.Node => {
        shortComments(node).exists { n => {
          val str = n.toString
          str.contains("<code>foo</code>") && str.contains("</strong>")
        } }
      }
      case _ => false
    }
  }

  property("Trac #4358") = {
    createTemplate("Trac4358.scala") match {
      case node: scala.xml.Node =>
        ! shortComments(node).exists {
          _.toString.contains("<em>i.</em>")
        }
      case _ => false
    }
  }

  property("Trac #4180") = {
    createTemplate("Trac4180.scala") != None
  }

  property("Trac #4372") = {
    createTemplate("Trac4372.scala") match {
      case node: scala.xml.Node => {
        val html = node.toString
        html.contains("<span title=\"gt4s: $plus$colon\" class=\"name\">+:</span>") &&
          html.contains("<span title=\"gt4s: $minus$colon\" class=\"name\">-:</span>") &&
            html.contains("""<span class="params">(<span name="n">n: <span class="extype" name="scala.Int">Int</span></span>)</span><span class="result">: <span class="extype" name="scala.Int">Int</span></span>""")
      }
      case _ => false
    }
  }

  property("Trac #4374 - public") = {
    val files = createTemplates("Trac4374.scala")
    files("WithPublic.html") match {
      case node: scala.xml.Node => {
        val s = node.toString
        s.contains("""href="WithPublic$.html"""") &&
          files.get("WithPublic$.html") != None
      }
      case _ => false
    }
  }

  property("Trac #4374 - private") = {
    val files = createTemplates("Trac4374.scala")
    files("WithPrivate.html") match {
      case node: scala.xml.Node => {
        val s = node.toString
        ! s.contains("""href="WithPrivate$.html"""") &&
          files.get("WithPrivate$.html") == None
      }
      case _ => false
    }
  }

  property("Trac #4325 - files") = {
    val files = createTemplates("Trac4325.scala")

    files.get("WithSynthetic.html") != None &&
      files.get("WithSynthetic$.html") == None &&
        files.get("WithObject.html") != None &&
          files.get("WithObject$.html") != None
  }

  property("Trac #4325 - Don't link to syntetic companion") = {
    val files = createTemplates("Trac4325.scala")

    files("WithSynthetic.html") match {
      case node: scala.xml.Node => {
        val s = node.toString
        ! s.contains("""href="WithSynthetic$.html"""")
      }
      case _ => false
    }
  }

  property("Trac #4325 - Link to companion") = {
    val files = createTemplates("Trac4325.scala")

    files("WithObject.html") match {
      case node: scala.xml.Node => {
        val s = node.toString
        s.contains("""href="WithObject$.html"""")
      }
      case _ => false
    }
  }

  property("Trac #4420 - no whitespace at end of line") = {
    val files = createTemplates("Trac4420.scala")

    files("TestA.html") match {
      case node: scala.xml.Node => {
        val s = node.toString
        s.contains("""See YYY for more details""")
      }
      case _ => false
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
    val files = createTemplates("Trac4289.scala")

    files("Subclass.html") match {
      case node: scala.xml.Node => {
        node.toString.contains {
          """<dt>returns</dt><dd class="cmt"><p>123</p></dd>"""
        }
      }
      case _ => false
    }
  }

  property("Trac #4409") = {
    createTemplate("Trac4409.scala") match {
      case node: scala.xml.Node => {
        ! node.toString.contains("""<div class="block"><ol>since""")
      }
      case _ => false
    }
  }

  property("Trac #4452") = {
    createTemplate("Trac4452.scala") match {
      case node: scala.xml.Node =>
        ! node.toString.contains(">*")
      case _ => false
    }
  }

  property("SI-4421") = {
    createTemplate("SI_4421.scala") match {
      case node: scala.xml.Node => {
        val html = node.toString
        html.contains(">Example:") && html.contains(">Note<")
      }
      case _ => false
    }
  }

  property("SI-4589") = {
    createTemplate("SI_4589.scala") match {
      case node: scala.xml.Node => {
        val html = node.toString
        html.contains(">x0123456789: <") &&
          html.contains(">x012345678901234567890123456789: <")
      }
      case _ => false
    }
  }

  property("SI-4714: Should decode symbolic type alias name.") = {
    createTemplate("SI_4715.scala") match {
      case node: scala.xml.Node => {
        val html = node.toString
        html.contains(">:+:<")
      }
      case _ => false
    }
  }

  property("SI-4287: Default arguments of synthesized constructor") = {
    val files = createTemplates("SI_4287.scala")

    files("ClassWithSugar.html") match {
      case node: scala.xml.Node => {
        node.toString.contains(">123<")
      }
      case _ => false
    }
  }

  property("SI-4507: Default arguments of synthesized constructor") = {
    createTemplate("SI_4507.scala") match {
      case node: scala.xml.Node =>
        ! node.toString.contains("<li>returns silently when evaluating true and true</li>")
      case _ => false
    }
  }

  property("SI-4898: Use cases and links should not crash scaladoc") = {
    createTemplate("SI_4898.scala")
    true
  }

  property("SI-5054: Use cases should override their original members") =
     checkText("SI_5054_q1.scala")(
       (None,"""def test(): Int""", true)
       //Disabled because the full signature is now displayed
       //(None, """def test(implicit lost: Int): Int""", false)
     )

  property("SI-5054: Use cases should keep their flags - final should not be lost") =
    checkText("SI_5054_q2.scala")((None, """final def test(): Int""", true))

  property("SI-5054: Use cases should keep their flags - implicit should not be lost") =
    checkText("SI_5054_q3.scala")((None, """implicit def test(): Int""", true))

  property("SI-5054: Use cases should keep their flags - real abstract should not be lost") =
    checkText("SI_5054_q4.scala")((None, """abstract def test(): Int""", true))

  property("SI-5054: Use cases should keep their flags - traits should not be affected") =
    checkText("SI_5054_q5.scala")((None, """def test(): Int""", true))

  property("SI-5054: Use cases should keep their flags - traits should not be affected") =
    checkText("SI_5054_q6.scala")((None, """abstract def test(): Int""", true))

  property("SI-5054: Use case individual signature test") =
    checkText("SI_5054_q7.scala")(
      (None, """abstract def test2(explicit: Int): Int [use case] This takes the explicit value passed.""", true),
      (None, """abstract def test1(): Int [use case] This takes the implicit value in scope.""", true)
    )

  property("SI-5287: Display correct \"Definition classes\"") =
    checkText("SI_5287.scala")(
      (None,
          """def method(): Int
           [use case] The usecase explanation
           [use case] The usecase explanation
           Definition Classes SI_5287 SI_5287_B SI_5287_A""", true)
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
    val files = createTemplates("code-indent.scala")

    files("C.html") match {
      case node: scala.xml.Node => {
        val s = node.toString
        s.contains("<pre>a typicial indented\ncomment on multiple\ncomment lines</pre>") &&
        s.contains("<pre>one liner</pre>") &&
        s.contains("<pre>two lines, one useful</pre>") &&
        s.contains("<pre>line1\nline2\nline3\nline4</pre>") &&
        s.contains("<pre>a ragged example\na (condition)\n  the t h e n branch\nan alternative\n  the e l s e branch</pre>") &&
        s.contains("<pre>Trait example {\n  Val x = a\n  Val y = b\n}</pre>") &&
        s.contains("<pre>l1\n\nl2\n\nl3\n\nl4\n\nl5</pre>")
      }
      case _ => false
    }
  }

  property("SI-4014: Scaladoc omits @author: no authors") = {
    val noAuthors = createTemplates("SI-4014_0.scala")("Foo.html")

    noAuthors match {
      case node: scala.xml.Node => {
        val s = node.toString
        ! s.contains("Author")
      }
      case _ => false
    }
  }

  property("SI-4014: Scaladoc omits @author: one author") = {
    val oneAuthor = createTemplates("SI-4014_1.scala")("Foo.html")

    oneAuthor match {
      case node: scala.xml.Node => {
        val s = node.toString
        s.contains("<h6>Author:</h6>") &&
        s.contains("<p>The Only Author</p>")
      }
      case _ => false
    }
  }

  property("SI-4014: Scaladoc omits @author: two authors") = {
    val twoAuthors = createTemplates("SI-4014_2.scala")("Foo.html")

    twoAuthors match {
      case node: scala.xml.Node => {
        val s = node.toString
        s.contains("<h6>Authors:</h6>") &&
        s.contains("<p>The First Author</p>") &&
        s.contains("<p>The Second Author</p>")
      }
      case _ => false
    }
  }

  {
    val files = createTemplates("basic.scala")
    //println(files)

    property("class") = files.get("com/example/p1/Clazz.html") match {
      case Some(node: scala.xml.Node) => {
        property("implicit conversion") =
          node.toString contains "<span class=\"modifier\">implicit </span>"

        property("gt4s") =
          node.toString contains "title=\"gt4s: $colon$colon\""

        property("gt4s of a deprecated method") =
          node.toString contains "title=\"gt4s: $colon$colon$colon$colon. Deprecated: "
        true
      }
      case _ => false
    }
    property("package") = files.get("com/example/p1/index.html") != None

    property("package object") = files("com/example/p1/index.html") match {
      case node: scala.xml.Node =>
        node.toString contains "com.example.p1#packageObjectMethod"
      case _ => false
    }

    property("lower bound") = files("com/example/p1/LowerBound.html") match {
      case node: scala.xml.Node => true
      case _ => false
    }

    property("upper bound") = files("com/example/p1/UpperBound.html") match {
      case node: scala.xml.Node => true
      case _ => false
    }

    property("SI-8514: No inconsistencies") =
      checkText("SI-8514.scala")(
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

  // SI-8144
  {
    implicit class AttributesAwareNode(val node: NodeSeq) {

      def \@(attrName: String): String =
        node \ ("@" + attrName) text

      def \@(attrName: String, attrValue: String): NodeSeq =
        node filter { _ \ ("@" + attrName) exists (_.text == attrValue) }
    }

    implicit class AssertionAwareNode(node: scala.xml.NodeSeq) {

      def assertTypeLink(expectedUrl: String): Boolean = {
        val linkElement: NodeSeq = node \\ "div" \@ ("id", "definition") \\ "span" \@ ("class", "permalink") \ "a"
        linkElement \@ "href" == expectedUrl
      }

      def assertMemberLink(group: String)(memberName: String, expectedUrl: String): Boolean = {
        val linkElement: NodeSeq = node \\ "div" \@ ("id", group) \\ "li" \@ ("name", memberName) \\ "span" \@ ("class", "permalink") \ "a"
        linkElement \@ "href" == expectedUrl
      }

      def assertValuesLink(memberName: String, expectedUrl: String): Boolean = {
        val linkElement: NodeSeq = node \\ "div" \@ ("class", "values members") \\ "li" \@ ("name", memberName) \\ "span" \@ ("class", "permalink") \ "a"
        linkElement \@ "href" == expectedUrl
      }

    }

    val files = createTemplates("SI-8144.scala")

    def check(pagePath: String)(f: NodeSeq => org.scalacheck.Prop): org.scalacheck.Prop =
      files(pagePath) match {
        case node: scala.xml.Node => f(XMLUtil.stripGroup(node))
        case _ => false
      }

    property("SI-8144: Members' permalink - inner package") = check("some/pack/index.html") { node =>
      ("type link" |: node.assertTypeLink("../../some/pack/index.html")) &&
        ("member: SomeType (object)" |: node.assertValuesLink("some.pack.SomeType", "../../some/pack/index.html#SomeType")) &&
        ("member: SomeType (class)" |: node.assertMemberLink("types")("some.pack.SomeType", "../../some/pack/index.html#SomeTypeextendsAnyRef"))
    }

    property("SI-8144: Members' permalink - companion object") = check("some/pack/SomeType$.html") { node =>
      ("type link" |: node.assertTypeLink("../../some/pack/SomeType$.html")) &&
        ("member: someVal" |: node.assertMemberLink("allMembers")("some.pack.SomeType#someVal", "../../some/pack/SomeType$.html#someVal:String"))
    }

    property("SI-8144: Members' permalink - class") = check("some/pack/SomeType.html") { node =>
      ("type link" |: node.assertTypeLink("../../some/pack/SomeType.html")) &&
      ("constructor " |: node.assertMemberLink("constructors")("some.pack.SomeType#<init>", "../../some/pack/SomeType.html#<init>(arg:String):some.pack.SomeType")) &&
        ( "member: type TypeAlias" |: node.assertMemberLink("types")("some.pack.SomeType.TypeAlias", "../../some/pack/SomeType.html#TypeAlias=String")) &&
        ( "member: def >#<():Int " |: node.assertValuesLink("some.pack.SomeType#>#<", "../../some/pack/SomeType.html#>#<():Int")) &&
        ( "member: def >@<():TypeAlias " |: node.assertValuesLink("some.pack.SomeType#>@<", "../../some/pack/SomeType.html#>@<():SomeType.this.TypeAlias"))
    }

  }

  property("SI-9599 Multiple @todo formatted with comma on separate line") = {
    createTemplates("SI-9599.scala")("X.html") match {
      case node: scala.xml.Node => node.text.contains("todo3todo2todo1")
      case _ => false
    }
  }
}

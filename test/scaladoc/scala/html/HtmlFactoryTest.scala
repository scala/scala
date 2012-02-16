import org.scalacheck._
import org.scalacheck.Prop._

import java.net.{URLClassLoader, URLDecoder}

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
	
  import scala.tools.nsc.doc.{DocFactory, Settings}
  import scala.tools.nsc.doc.model.IndexModelFactory
  import scala.tools.nsc.doc.html.HtmlFactory
  import scala.tools.nsc.doc.html.page.ReferenceIndex

  def getClasspath = {
    // these things can be tricky
    // this test previously relied on the assumption that the current thread's classloader is an url classloader and contains all the classpaths
    // does partest actually guarantee this? to quote Leonard Nimoy: The answer, of course, is no.
    // this test _will_ fail again some time in the future.
    val paths = Thread.currentThread.getContextClassLoader.asInstanceOf[URLClassLoader].getURLs.map(u => URLDecoder.decode(u.getPath))
    val morepaths = Thread.currentThread.getContextClassLoader.getParent.asInstanceOf[URLClassLoader].getURLs.map(u => URLDecoder.decode(u.getPath))
    (paths ++ morepaths).mkString(java.io.File.pathSeparator)
  }

  def createFactory = {
    val settings = new Settings({Console.err.println(_)})
    settings.classpath.value = getClasspath

    val reporter = new scala.tools.nsc.reporters.ConsoleReporter(settings)
    new DocFactory(reporter, settings)
  }

  def createTemplates(basename: String) = {
    val result = scala.collection.mutable.Map[String, scala.xml.NodeSeq]()

    createFactory.makeUniverse(List(RESOURCES+basename)) match {
      case Some(universe) => {
        val index = IndexModelFactory.makeIndex(universe)
        (new HtmlFactory(universe, index)).writeTemplates((page) => {
          result += (page.absoluteLinkTo(page.path) -> page.body)
        })
      }
      case _ => ;
    }

    result
  }

  def createReferenceIndex(basename: String) = {
    createFactory.makeUniverse(List(RESOURCES+basename)) match {
      case Some(universe) => {
        val index = IndexModelFactory.makeIndex(universe)
        val pages = index.firstLetterIndex.map({
          case (key, value) => {
            val page = new ReferenceIndex(key, index, universe)
            page.absoluteLinkTo(page.path) -> page.body
          }
        })
        Some(pages)
      }
      case _ =>
        None
    }
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
      val fileText = htmlAllFiles(fileName).text.replace('→',' ').replaceAll("\\s+","")
      val checkText = check.replace('→',' ').replaceAll("\\s+","")
      val checkValue = fileText.contains(checkText) == expected
      if (debug && (!checkValue)) {
        Console.err.println("Check failed: ")
        Console.err.println("HTML: " + fileText)
        Console.err.println("Check: " + checkText)
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

  property("Trac #3484") = {
    val files = createTemplates("Trac3484.scala")

    files("Collection.html") match {
      case node: scala.xml.Node => {
        val s = node.toString
        s.contains("""<span class="result">: Traversable[B]</span>""")
      }
      case _ => false
    }
  }

  property("Trac #3484 - SR704") = {
    val files = createTemplates("Trac3484.scala")

    files("SR704.html") match {
      case node: scala.xml.Node => {
        val s = node.toString
        s.contains("Hello Mister John.")
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

  property("Trac #484 - refinements and existentials") = {
    val files = createTemplates("Trac484.scala")
    val lines = """
        |type Bar = AnyRef { type Dingus <: T forSome { type T <: String } }
        |type Foo = AnyRef { ... /* 3 definitions in type refinement */ }
        |def g(x: T forSome { type T <: String }): String
        |def h(x: Float): AnyRef { def quux(x: Int,y: Int): Int }
        |def hh(x: Float): AnyRef { def quux(x: Int,y: Int): Int }
        |def j(x: Int): Bar
        |def k(): AnyRef { type Dingus <: T forSome { type T <: String } }
      """.stripMargin.trim.lines map (_.trim)

    files("RefinementAndExistentials.html") match {
      case node: scala.xml.Node => {
        val s = node.text.replaceAll("\\s+", " ")
        lines forall (s contains _)
      }
      case _ => false
    }
  }

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

  property("Trac #4471") = {
    createReferenceIndex("Trac4471.scala") match {
      case Some(pages) =>
        (pages.get("index/index-f.html") match {
          case Some(node) => node.toString.contains(">A</a></strike>")
          case _ => false
        }) && (pages.get("index/index-b.html") match {
          case Some(node) => node.toString.contains(">bar</strike>")
          case _ => false
        })
      case _ => false
    }
  }

  property("SI-4641") = {
    createReferenceIndex("SI_4641.scala") match {
      case Some(pages) => pages.contains("index/index-_.html")
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

  property("Should decode symbolic type alias name.") = {
    createTemplate("SI_4715.scala") match {
      case node: scala.xml.Node => {
        val html = node.toString
        html.contains(">: :+:[<")
      }
      case _ => false
    }
  }

  property("Shouldn't drop type arguments to aliased tuple.") = {
    createTemplate("SI_4676.scala") match {
      case node: scala.xml.Node => {
        node.toString.contains(">ss: (String, String)<")
      }
      case _ => false
    }
  }

  property("Default arguments of synthesized constructor") = {
    val files = createTemplates("SI_4287.scala")

    files("ClassWithSugar.html") match {
      case node: scala.xml.Node => {
        node.toString.contains(">123<")
      }
      case _ => false
    }
  }

  property("Default arguments of synthesized constructor") = {
    createTemplate("SI_4507.scala") match {
      case node: scala.xml.Node =>
        ! node.toString.contains("<li>returns silently when evaluating true and true</li>")
      case _ => false
    }
  }

  property("Use cases and links should not crash scaladoc") = {
    createTemplate("SI_4898.scala")
    true
  }
 
  property("Use cases should override their original members") =
     checkText("SI_5054_q1.scala")(
       (None,"""def test(): Int""", true),
       (None,"""def test(implicit lost: Int): Int""", false)
     )

  property("Use cases should keep their flags - final should not be lost") = 
    checkText("SI_5054_q2.scala")((None, """final def test(): Int""", true))
  
  property("Use cases should keep their flags - implicit should not be lost") = 
    checkText("SI_5054_q3.scala")((None, """implicit def test(): Int""", true))
 
  property("Use cases should keep their flags - real abstract should not be lost") = 
    checkText("SI_5054_q4.scala")((None, """abstract def test(): Int""", true))

  property("Use cases should keep their flags - traits should not be affected") = 
    checkText("SI_5054_q5.scala")((None, """def test(): Int""", true))

  property("Use cases should keep their flags - traits should not be affected") = 
    checkText("SI_5054_q6.scala")((None, """abstract def test(): Int""", true))
   
  property("Use case individual signature test") = 
    checkText("SI_5054_q7.scala")(
      (None, """abstract def test2(explicit: Int): Int [use case] This takes the explicit value passed.""", true),
      (None, """abstract def test1(): Int [use case] This takes the implicit value in scope.""", true)
    )

  property("Display correct \"Definition classes\"") = 
    checkText("SI_5287.scala")( 
      (None,
          """def method(): Int
           [use case] The usecase explanation
           [use case] The usecase explanation
           Definition Classes SI_5287 SI_5287_B SI_5287_A""", true)
    )      // the explanation appears twice, as small comment and full comment           
  
    
  property("Correct comment inheritance for overriding") = 
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
          T The overriden type parameter comment
          arg1 The T term comment
          arg2 The string comment
          returns The return comment
          """, true)
    )
    
  for (useCaseFile <- List("UseCaseInheritance", "UseCaseOverrideInheritance")) {
    property("Correct comment inheritance for usecases") = 
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
    
  {
    val files = createTemplates("basic.scala")
    //println(files)

    property("class") = files.get("com/example/p1/Clazz.html") match {
      case Some(node: scala.xml.Node) => {
        property("implicit convertion") =
          node.toString contains "<span class=\"modifier\">implicit </span>"

        property("gt4s") =
          node.toString contains "title=\"gt4s: $colon$colon\""

        property("gt4s of a deprecated method") =
          node.toString contains "title=\"gt4s: $colon$colon$colon$colon. Deprecated: "
        true
      }
      case _ => false
    }
    property("package") = files.get("com/example/p1/package.html") != None

    property("package object") = files("com/example/p1/package.html") match {
      case node: scala.xml.Node =>
        node.toString contains "com.example.p1.package#packageObjectMethod"
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
  }
}

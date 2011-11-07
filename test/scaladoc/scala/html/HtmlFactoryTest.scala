import org.scalacheck._
import org.scalacheck.Prop._

import java.net.URLClassLoader

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
  import scala.tools.nsc.doc.{DocFactory, Settings}
  import scala.tools.nsc.doc.model.IndexModelFactory
  import scala.tools.nsc.doc.html.HtmlFactory
  import scala.tools.nsc.doc.html.page.ReferenceIndex

  def getClasspath = {
    // these things can be tricky
    // this test previously relied on the assumption that the current thread's classloader is an url classloader and contains all the classpaths
    // does partest actually guarantee this? to quote Leonard Nimoy: The answer, of course, is no.
    // this test _will_ fail again some time in the future.
    val paths = Thread.currentThread.getContextClassLoader.asInstanceOf[URLClassLoader].getURLs.map(_.getPath)
    val morepaths = Thread.currentThread.getContextClassLoader.getParent.asInstanceOf[URLClassLoader].getURLs.map(_.getPath)
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

    createFactory.makeUniverse(List("test/scaladoc/resources/"+basename)) match {
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
    createFactory.makeUniverse(List("test/scaladoc/resources/"+basename)) match {
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
        html.contains("<span class=\"name\">+:</span>\n") &&
          html.contains("<span class=\"name\">-:</span>\n") &&
            html.contains("""<span class="params">(<span name="n">n: <span name="scala.Int" class="extype">Int</span></span>)</span><span class="result">: <span name="scala.Int" class="extype">Int</span></span>""")
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
        |def g (x: T forSome { type T <: String }): String 
        |def h (x: Float): AnyRef { def quux(x: Int,y: Int): Int }
        |def hh (x: Float): AnyRef { def quux(x: Int,y: Int): Int }
        |def j (x: Int): Bar
        |def k (): AnyRef { type Dingus <: T forSome { type T <: String } }
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
}

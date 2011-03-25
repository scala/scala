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

  def attributeIs(key: String, value: String) = {
    (element: Node) => {
      element.attribute(key) match {
        case Some(v) =>
          v.toString == value
        case _ =>
          false
      }
    }
  }

  def textIs(value: String) = {
    (node: Node) => {
      node.descendant.exists((n) => n.toString.trim == value)
    }
  }
}

object Test extends Properties("HtmlFactory") {
  import scala.tools.nsc.doc.{DocFactory, Settings}
  import scala.tools.nsc.doc.model.IndexModelFactory
  import scala.tools.nsc.doc.html.HtmlFactory

  def getClasspath = {
    // these things can be tricky
    // this test previously relied on the assumption that the current thread's classloader is an url classloader and contains all the classpaths
    // does partest actually guarantee this? to quote Leonard Nimoy: The answer, of course, is no.
    // this test _will_ fail again some time in the future.
    val paths = Thread.currentThread.getContextClassLoader.asInstanceOf[URLClassLoader].getURLs.map(_.getPath)
    val morepaths = Thread.currentThread.getContextClassLoader.getParent.asInstanceOf[URLClassLoader].getURLs.map(_.getPath)
    (paths ++ morepaths).mkString(java.io.File.pathSeparator)
  }

  def createTemplates(basename: String) = {
    val result = scala.collection.mutable.Map[String, scala.xml.NodeSeq]()

    val factory = {
      val settings = new Settings({Console.err.println(_)})
      settings.classpath.value = getClasspath

      val reporter = new scala.tools.nsc.reporters.ConsoleReporter(settings)
      new DocFactory(reporter, settings)
    }

    factory.makeUniverse(List("test/scaladoc/resources/"+basename)) match {
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

  property("Trac #3790") = {
    import XMLUtil._

    val files = createTemplates("Trac3790.scala")
    files("Trac3790.html") match {
      case node: scala.xml.Node => {
        val comments = (stripGroup(node) \\ "div").flatMap {
          case e: scala.xml.Elem => Some(e)
          case _ => None
        }.filter { attributeIs("class", "fullcomment")(_) }

        comments.filter(textIs("A lazy String")(_)).length == 1 &&
          comments.filter(textIs("A non-lazy String")(_)).length == 1
      }
      case _ => false
    }
  }

  property("Trac #4306") = {
    val files = createTemplates("Trac4306.scala")
    files("com/example/trac4306/foo/package$$Bar.html") != None
  }

  property("Trac #4366") = {
    val files = createTemplates("Trac4366.scala")
    files("Trac4366.html") match {
      case node: scala.xml.Node => {
        val comments = XMLUtil.stripGroup(node).descendant.flatMap {
          case e: scala.xml.Elem => {
            if (e.attribute("class").toString.contains("shortcomment")) {
              Some(e)
            } else {
              None
            }
          }
          case _ => None
        }

        comments.exists {
          (e) => {
            val s = e.toString
            s.contains("<code>foo</code>") && s.contains("</strong>")
          }
        }
      }
      case _ => false
    }
  }

  property("Trac #4358") = {
    val files = createTemplates("Trac4358.scala")
    files("EasyMockSugar.html") match {
      case node: scala.xml.Node => {
        val comments = XMLUtil.stripGroup(node).descendant.flatMap {
          case e: scala.xml.Elem => {
            if (e.attribute("class").toString.contains("shortcomment")) {
              Some(e)
            } else {
              None
            }
          }
          case _ => None
        }
        ! comments.exists { _.toString.contains("<em>i.</em>") }
      }
      case _ => false
    }
  }
}

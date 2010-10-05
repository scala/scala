import org.scalacheck._

import scala.tools.nsc.doc
import scala.tools.nsc.doc.html.page.Index
import java.net.URLClassLoader

object Test extends Properties("Index") {
  val docFactory = {
    val settings = new doc.Settings((s: String) => {
      Console.err.println(s)
    })
    settings.classpath.value =
      Thread.currentThread.getContextClassLoader.asInstanceOf[URLClassLoader].getURLs.map(_.getPath).mkString(":")

    val reporter = new scala.tools.nsc.reporters.ConsoleReporter(settings)

    new doc.DocFactory(reporter, settings)
  }

  val indexModelFactory = {
    val settings = new doc.Settings((s: String) => {
      Console.err.println(s)
    })
    settings.classpath.value =
      Thread.currentThread.getContextClassLoader.asInstanceOf[URLClassLoader].getURLs.map(_.getPath).mkString(":")

    val reporter = new scala.tools.nsc.reporters.ConsoleReporter(settings)

    new doc.model.IndexModelFactory
  }

  def createIndex(path: String): Option[Index] = {
    val maybeModel = {
      val stream = new java.io.ByteArrayOutputStream
      val original = Console.out
      Console.setOut(stream)

      val result = docFactory.universe(List(path))

      // assert(stream.toString == "model contains 2 documentable templates\n")
      Console.setOut(original)

      result
    }

    maybeModel match {
      case Some(model) => {
        val index = new Index(model, indexModelFactory.makeModel(model))
        return Some(index)
      }
      case _ => return None
    }
  }

  property("path") = {
    createIndex("src/compiler/scala/tools/nsc/doc/html/page/Index.scala") match {
      case Some(index) =>
        index.path == List("index.html")
      case None => false
    }
  }

  property("title") = {
    createIndex("src/compiler/scala/tools/nsc/doc/html/page/Index.scala") match {
      case Some(index) =>
        index.title == ""

      case None => false
    }
  }
  property("browser contants a script element") = {
    createIndex("src/compiler/scala/tools/nsc/doc/html/page/Index.scala") match {
      case Some(index) =>
        (index.browser \ "script").size == 1

      case None => false
    }
  }

  property("allPackages") = {
    createIndex("src/compiler/scala/tools/nsc/doc/html/page/Index.scala") match {
      case Some(index) => {
        index.allPackages.map(_.toString) == List(
          "scala",
          "scala.tools",
          "scala.tools.nsc",
          "scala.tools.nsc.doc",
          "scala.tools.nsc.doc.html",
          "scala.tools.nsc.doc.html.page"
        )
      }

      case None => false
    }
  }
}
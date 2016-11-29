import org.scalacheck._
import org.scalacheck.Prop._

import scala.tools.nsc.doc
import scala.tools.nsc.doc.html.page.IndexScript
import java.net.{URLClassLoader, URLDecoder}

object Test extends Properties("IndexScript") {

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

  val docFactory = {
    val settings = new doc.Settings({Console.err.println(_)})
    settings.scaladocQuietRun = true
    settings.nowarn.value = true
    settings.classpath.value = getClasspath
    val reporter = new scala.tools.nsc.reporters.ConsoleReporter(settings)
    new doc.DocFactory(reporter, settings)
  }

  val indexModelFactory = doc.model.IndexModelFactory

  def createIndexScript(path: String) =
    docFactory.makeUniverse(Left(List(path))) match {
      case Some(universe) =>
        Some(new IndexScript(universe))
      case _ =>
        None
    }

  property("allPackages") = {
    createIndexScript("src/scaladoc/scala/tools/nsc/doc/html/page/IndexScript.scala") match {
      case Some(index) =>
        index.allPackages.map(_.toString) == List(
          "scala",
          "scala.tools",
          "scala.tools.nsc",
          "scala.tools.nsc.doc",
          "scala.tools.nsc.doc.html",
          "scala.tools.nsc.doc.html.page"
        )
      case None =>
        false
    }
  }
}

import org.scalacheck._
import org.scalacheck.Prop._

import scala.tools.nsc.doc
import scala.tools.nsc.doc.html.page.Index
import java.net.{URLClassLoader, URLDecoder}

object Test extends Properties("Index") {

  def getClasspath = {
    // these things can be tricky
    // this test previously relied on the assumption that the current thread's classloader is an url classloader and contains all the classpaths
    // does partest actually guarantee this? to quote Leonard Nimoy: The answer, of course, is no.
    // this test _will_ fail again some time in the future.
    val paths = Thread.currentThread.getContextClassLoader.asInstanceOf[URLClassLoader].getURLs.map(u => URLDecoder.decode(u.getPath))
    val morepaths = Thread.currentThread.getContextClassLoader.getParent.asInstanceOf[URLClassLoader].getURLs.map(u => URLDecoder.decode(u.getPath))
    (paths ++ morepaths).mkString(java.io.File.pathSeparator)
  }

  val docFactory = {
    val settings = new doc.Settings({Console.err.println(_)})

    settings.classpath.value = getClasspath
    println(settings.classpath.value)

    val reporter = new scala.tools.nsc.reporters.ConsoleReporter(settings)

    new doc.DocFactory(reporter, settings)
  }

  val indexModelFactory = doc.model.IndexModelFactory

  def createIndex(path: String): Option[Index] = {

    val maybeUniverse = {
      //val stream = new java.io.ByteArrayOutputStream
      //val original = Console.out
      //Console.setOut(stream)

      val result = docFactory.makeUniverse(Left(List(path)))

      // assert(stream.toString == "model contains 2 documentable templates\n")
      //Console.setOut(original)

      result
    }

    maybeUniverse match {
      case Some(universe) => {
        val index = new Index(universe, indexModelFactory.makeIndex(universe))
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

  property("package objects in index") = {
    createIndex("test/scaladoc/resources/SI-5558.scala") match {
      case Some(index) =>
        index.index.firstLetterIndex('f') isDefinedAt "foo"
      case None => false
    }
  }
}

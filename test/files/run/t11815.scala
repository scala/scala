
// check writing a jar with manifest.

import scala.reflect.io.{AbstractFile}
import scala.tools.nsc._, backend.jvm.ClassfileWriters, reporters.StoreReporter, io.Jar
import scala.tools.partest.DirectTest
import scala.util.chaining._

class JarTest(global: Global) {
  val cfws = new ClassfileWriters {
    val postProcessor: scala.tools.nsc.backend.jvm.PostProcessor = null
  }
  def test(path: String): Unit = {
    //val cfw = cfws.ClassfileWriter.apply(global)
    val f  = AbstractFile.getFile(path)
    val fw = cfws.FileWriter(global, f, Some("XMain"))
    fw.writeFile("foo.class", "hello".getBytes)
    fw.writeFile("bar.class", "world".getBytes)
    fw.close()

    new Jar(path).mainClass match {
      case Some(null) | None => println("Missing main-class")
      case Some("XMain") =>
      case Some(_) => println("Wrong main-class")
    }
  }
}

object Test extends DirectTest {
  def code     = ""
  def show()   = {
    val jarpath  = (testOutput / "xmain.jar").path
    val settings = new Settings().tap(_.processArgumentString(s"-Xmain-class XMain -d $jarpath"))
    val storer   = new StoreReporter(settings)
    val global   = Global(settings, storer)
    new JarTest(global).test(jarpath)
    assert(storer.infos.isEmpty)
  }
}

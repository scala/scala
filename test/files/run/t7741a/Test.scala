import java.io.{ByteArrayInputStream, FileOutputStream, BufferedOutputStream}
import java.util

import java.io.File

import scala.tools.partest.DirectTest

object Test extends DirectTest {

  def code = ""

  override def show(): Unit = {

    val class1: Array[Byte] = GroovyInterfaceDump.dump()
    val class2: Array[Byte] = GroovyInterface$1Dump.dump()
    def writeFile(contents: Array[Byte], f: java.io.File): Unit = {
      val out = new BufferedOutputStream(new FileOutputStream(f))
      try {
        out.write(contents)
      } finally out.close()
    }

    val outdir = testOutput.jfile

    // interface GroovyInterface {
    //
    //   // This is the line that causes scalac to choke.
    //   // It results in a GroovyInterface$1 class, which is a non-static inner class but its constructor does not
    //   // include the implicit parameter that is the immediate enclosing instance.
    //   // See http://jira.codehaus.org/browse/GROOVY-7312
    //   //
    //   // Scalac error:
    //   //   [scalac] error: error while loading 1, class file '..../scala_groovy_interop/classes/com/example/groovy/GroovyInterface$1.class' is broken
    //   //   [scalac] (class java.util.NoSuchElementException/head of empty list)
    //   final static def closure = { x -> "banana" }
    //
    // }
    writeFile(GroovyInterfaceDump.dump(), new File(outdir, "GroovyInterface.class"))
    writeFile(GroovyInterface$1Dump.dump(), new File(outdir, "GroovyInterface$1.class"))
    compileCode("object Test { def foo(g: GroovyInterface) = g.toString }")
  }

  def compileCode(code: String) = {
    val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
    compileString(newCompiler("-cp", classpath, "-d", testOutput.path))(code)
  }
}

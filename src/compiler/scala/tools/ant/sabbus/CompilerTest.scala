/*                     __                                               *\
**     ________ ___   / /  ___     Scala Ant Tasks                      **
**    / __/ __// _ | / /  / _ |    (c) 2005-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools.ant.sabbus

import java.io.File
import java.net.URL

object CompilerTest {

  def main(args: Array[String]): Unit = {

    implicit def fileToURL(file: File): URL = file.toURL

    val scalalib = new File("/Developer/Scala/latest/lib")
    val sabbus = new File("/Users/Dubochet/Documents/Eclipse/FaSabbus")

    val classpath: Array[URL] = Array (
      new File(scalalib, "scala-library.jar"),
      new File(scalalib, "scala-compiler.jar"),
      new File(sabbus, "bin")
    )

    val settings = new CompilerSettings
    settings.d = new File("/Users/Dubochet/Documents/Eclipse/FaSabbus/bin_sabbus")
    val compiler = new Compiler(classpath, settings)

    val files: Array[File] = Array (
      new File(sabbus, "src/scala/tools/ant/sabbus/CompilationFailure.scala"),
      new File(sabbus, "src/scala/tools/ant/sabbus/Compiler.scala"),
      new File(sabbus, "src/scala/tools/ant/sabbus/CompilerTest.scala"),
      new File(sabbus, "src/scala/tools/ant/sabbus/ForeignCompiler.scala")
    )

    if (compiler.compile(files)._1 == 0)
      println("Everything a-okey, sir!")
    else
      println("We had some issues, sir!")

  }

}

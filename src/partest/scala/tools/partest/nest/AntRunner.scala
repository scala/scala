/*                     __                                               *\
**     ________ ___   / /  ___     Scala Parallel Testing               **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.tools.partest
package nest

import java.io.File
import scala.tools.nsc.io.{ Directory }

class AntRunner extends DirectRunner {

  val fileManager = new FileManager {
    var JAVACMD: String = "java"
    var JAVAC_CMD: String = "javac"
    var CLASSPATH: String = _
    var LATEST_LIB: String = _
    var LATEST_COMP: String = _
    var LATEST_PARTEST: String = _
    var LATEST_ACTORS: String = _
    var LATEST_ACTORS_MIGRATION: String = _
    val testRootPath: String = "test"
    val testRootDir: Directory = Directory(testRootPath)
  }

  def reflectiveRunTestsForFiles(kindFiles: Array[File], kind: String) =
    runTestsForFiles(kindFiles.toList, kind)
}

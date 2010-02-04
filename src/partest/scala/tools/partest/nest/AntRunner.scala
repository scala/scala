/*                     __                                               *\
**     ________ ___   / /  ___     Scala Parallel Testing               **
**    / __/ __// _ | / /  / _ |    (c) 2007-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.tools.partest
package nest

import java.io.File

class AntRunner extends DirectRunner {

  val fileManager = new FileManager {
    var JAVACMD: String = "java"
    var JAVAC_CMD: String = "javac"
    var CLASSPATH: String = _
    var EXT_CLASSPATH: String = _
    var LATEST_LIB: String = _
    val TESTROOT: String = ""
  }

  def reflectiveRunTestsForFiles(kindFiles: Array[File], kind: String) =
    runTestsForFiles(kindFiles.toList, kind)
}

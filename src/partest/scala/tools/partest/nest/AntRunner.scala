/*                     __                                               *\
**     ________ ___   / /  ___     Scala Parallel Testing               **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.tools.partest
package nest

class AntRunner extends DirectRunner {

  val fileManager = new FileManager {
    var JAVACMD: String                 = "java"
    var JAVAC_CMD: String               = "javac"
    var CLASSPATH: String               = _
    var LATEST_LIB: String              = _
    var LATEST_REFLECT: String          = _
    var LATEST_COMP: String             = _
    var LATEST_PARTEST: String          = _
    var LATEST_ACTORS: String           = _
    val testRootPath: String            = "test"
    val testRootDir: Directory          = Directory(testRootPath)

    /* documenting that the build supplies the optimise
     * flag as scalacOpts arg to the task, which updates SCALAC_OPTS.
    import scala.tools.nsc.Properties.propOrNone
    for (more <- propOrNone("scalac.args.optimise"))
      SCALAC_OPTS = SCALAC_OPTS :+ more
     */
  }

  def reflectiveRunTestsForFiles(kindFiles: Array[File], kind: String): List[TestState] =
    runTestsForFiles(kindFiles.toList, kind)
}

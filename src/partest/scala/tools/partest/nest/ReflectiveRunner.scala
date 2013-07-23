/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Philipp Haller
 */

package scala.tools.partest
package nest

import scala.tools.nsc.Properties.{ setProp, propOrEmpty }
import scala.tools.nsc.util.ClassPath
import scala.tools.nsc.io
import io.Path

/* This class is used to load an instance of DirectRunner using
 * a custom class loader.
 * The purpose is to "auto-detect" a good classpath for the
 * rest of the classes (Worker, CompileManager etc.), so that
 * the main NestRunner can be started merely by putting its
 * class on the classpath (ideally).
 */
class ReflectiveRunner {
  // TODO: we might also use fileManager.testClassPath
  // to use the same classes as used by `scala` that
  // was used to start the runner.
  val runnerClassName = "scala.tools.partest.nest.ConsoleRunner"

  private def searchPath(option: String, as: List[String]): Option[String] = as match {
    case `option` :: r :: _ => Some(r)
    case _ :: rest          => searchPath(option, rest)
    case Nil                => None
  }

  def main(args: String) {
    val argList = (args.split("\\s")).toList

    if (isPartestDebug)
      showAllJVMInfo

    // find out which build to test
    val buildPath = searchPath("--buildpath", argList)
    val classPath = searchPath("--classpath", argList)
    val fileManager = new ConsoleFileManager(if (argList contains "--pack") Some("build/pack") else buildPath, classPath)

    val classPathEntries = fileManager.testClassLoader.classPathURLs.map(_.toString)

    if (isPartestDebug)
      println("Loading classes from:\n  " + classPathEntries.mkString("\n  "))

    setProp("java.class.path", ClassPath.join(classPathEntries: _*))

    // don't let partest find pluginsdir; in ant build, standard plugin has dedicated test suite
    //setProp("scala.home", latestLibFile.parent.parent.path)
    setProp("scala.home", "")

    if (isPartestDebug)
      for (prop <- List("java.class.path", "sun.boot.class.path", "java.ext.dirs"))
        println(prop + ": " + propOrEmpty(prop))

    try {
      val runnerClass = fileManager.testClassLoader loadClass runnerClassName
      runnerClass.getMethod("main", classOf[Array[String]]).invoke(null, Array(args))
    } catch {
      case cnfe: ClassNotFoundException =>
        cnfe.printStackTrace()
        NestUI.failure(runnerClassName + " could not be loaded from:\n")
        fileManager.testClassLoader.classPathURLs foreach (x => NestUI.failure(x + "\n"))
    }
  }
}

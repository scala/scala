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
import java.net.URLClassLoader

/* This class is used to load an instance of DirectRunner using
 * a custom class loader.
 * The purpose is to "auto-detect" a good classpath for the
 * rest of the classes (Worker, CompileManager etc.), so that
 * the main NestRunner can be started merely by putting its
 * class on the classpath (ideally).
 */
class ReflectiveRunner {
  // TODO: we might also use fileManager.COMPILATION_CLASSPATH
  // to use the same classes as used by `scala` that
  // was used to start the runner.
  val sepRunnerClassName = "scala.tools.partest.nest.ConsoleRunner"

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
    val fileManager = new ConsoleFileManager(
      if (argList contains "--pack") Some("build/pack") else buildPath,
      classPath,
      updateCheck = argList contains "--update-check",
      failed = argList contains "--failed")

    // this seems to be the core classloader that determines which classes can be found when running partest from the test/partest script    
    val sepLoader = new URLClassLoader(fileManager.testClassPathUrls.toArray, null)

    if (isPartestDebug)
      println("Loading classes from:\n  " + fileManager.testClassPathUrls.mkString("\n  "))

    // @partest maintainer: it seems to me that commented lines are incorrect
    // if classPath is not empty, then it has been provided by the --classpath option
    // which points to the root of Scala home (see ConsoleFileManager's testClasses and the true flag in the ctor for more information)
    // this doesn't mean that we had custom Java classpath set, so we don't have to override latestXXXFiles from the file manager
    //
    //val paths = classPath match {
    //  case Some(cp) => Nil
    //  case _        => files.toList map (_.path)
    //}

    setProp("java.class.path", ClassPath.join(fileManager.testClassPath: _*))

    // don't let partest find pluginsdir; in ant build, standard plugin has dedicated test suite
    //setProp("scala.home", latestLibFile.parent.parent.path)
    setProp("scala.home", "")

    if (isPartestDebug)
      for (prop <- List("java.class.path", "sun.boot.class.path", "java.ext.dirs"))
        println(prop + ": " + propOrEmpty(prop))

    try {
      val sepRunnerClass = sepLoader loadClass sepRunnerClassName
      val sepMainMethod = sepRunnerClass.getMethod("main", classOf[Array[String]])
      val cargs: Array[AnyRef] = Array(Array(args))
      sepMainMethod.invoke(null, cargs: _*)
    } catch {
      case cnfe: ClassNotFoundException =>
        cnfe.printStackTrace()
        NestUI.failure(sepRunnerClassName + " could not be loaded from:\n")
        fileManager.testClassPathUrls foreach (x => NestUI.failure(x + "\n"))
    }
  }
}

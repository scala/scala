/* NEST (New Scala Test)
 * Copyright 2007-2011 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest
package nest

import scala.tools.nsc.Properties.{ setProp, propOrEmpty }
import scala.tools.nsc.util.ClassPath
import scala.tools.nsc.io
import io.Path
import RunnerUtils._
import java.net.URLClassLoader

/* This class is used to load an instance of DirectRunner using
 * a custom class loader.
 * The purpose is to "auto-detect" a good classpath for the
 * rest of the classes (Worker, CompileManager etc.), so that
 * the main NestRunner can be started merely by putting its
 * class on the classpath (ideally).
 */
class ReflectiveRunner {
  // TODO: we might also use fileManager.CLASSPATH
  // to use the same classes as used by `scala` that
  // was used to start the runner.
  val sepRunnerClassName = "scala.tools.partest.nest.ConsoleRunner"

  def main(args: String) {
    val argList = (args.split("\\s")).toList

    if (isPartestDebug)
      showAllJVMInfo

    // find out which build to test
    val buildPath = searchPath("--buildpath", argList)
    val classPath = searchPath("--classpath", argList)
    val fileManager =
      if (!buildPath.isEmpty)
        new ConsoleFileManager(buildPath.get)
      else if (!classPath.isEmpty)
        new ConsoleFileManager(classPath.get, true)
      else if (argList contains "--pack")
        new ConsoleFileManager("build/pack")
      else // auto detection
        new ConsoleFileManager

    import fileManager.
      { latestCompFile, latestLibFile, latestPartestFile, latestFjbgFile, latestScalapFile, latestActorsFile, latestActMigFile }
    val files =
      Array(latestCompFile, latestLibFile, latestPartestFile, latestFjbgFile, latestScalapFile, latestActorsFile, latestActMigFile) map (x => io.File(x))

    val sepUrls   = files map (_.toURL)
    var sepLoader = new URLClassLoader(sepUrls, null)

    // this is a workaround for https://issues.scala-lang.org/browse/SI-5433
    // when that bug is fixed, this paragraph of code can be safely removed
    // we hack into the classloader that will become parent classloader for scalac
    // this way we ensure that reflective macro lookup will pick correct Code.lift
    sepLoader = new URLClassLoader((PathSettings.srcCodeLib +: files) map (_.toURL), null)

    if (isPartestDebug)
      println("Loading classes from:\n" + sepUrls.mkString("\n"))

    // @partest maintainer: it seems to me that commented lines are incorrect
    // if classPath is not empty, then it has been provided by the --classpath option
    // which points to the root of Scala home (see ConsoleFileManager's testClasses and the true flag in the ctor for more information)
    // this doesn't mean that we had custom Java classpath set, so we don't have to override latestXXXFiles from the file manager
    //
    //val paths = classPath match {
    //  case Some(cp) => Nil
    //  case _        => files.toList map (_.path)
    //}
    val paths = files.toList map (_.path)

    val newClasspath = ClassPath.join(paths: _*)

    setProp("java.class.path", newClasspath)
    setProp("scala.home", "")

    if (isPartestDebug)
      for (prop <- List("java.class.path", "sun.boot.class.path", "java.ext.dirs"))
        println(prop + ": " + propOrEmpty(prop))

    try {
      val sepRunnerClass  = sepLoader loadClass sepRunnerClassName
      val sepRunner       = sepRunnerClass.newInstance()
      val sepMainMethod   = sepRunnerClass.getMethod("main", Array(classOf[String]): _*)
      val cargs: Array[AnyRef] = Array(args)
      sepMainMethod.invoke(sepRunner, cargs: _*)
    }
    catch {
      case cnfe: ClassNotFoundException =>
        cnfe.printStackTrace()
        NestUI.failure(sepRunnerClassName +" could not be loaded from:\n")
        sepUrls foreach (x => NestUI.failure(x + "\n"))
    }
  }
}

/* NEST (New Scala Test)
 * Copyright 2007-2008 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id: $

package scala.tools.partest.nest

/* This class is used to load an instance of DirectRunner using
 * a custom class loader.
 * The purpose is to "auto-detect" a good classpath for the
 * rest of the classes (Worker, CompileManager etc.), so that
 * the main NestRunner can be started merely by putting its
 * class on the classpath (ideally).
 */
class ReflectiveRunner {
  // we might also use FileManager.CLASSPATH
  // to use the same classes as used by `scala` that
  // was used to start the runner.

  import FileManager.{latestCompFile, latestLibFile, latestActFile,
                      latestPartestFile, latestFjbgFile}

  val sepUrls = Array(latestCompFile.toURL, latestLibFile.toURL,
                      latestActFile.toURL, latestPartestFile.toURL,
                      latestFjbgFile.toURL)
  val sepLoader = new java.net.URLClassLoader(sepUrls, null)

  val sepRunnerClass =
    sepLoader.loadClass("scala.tools.partest.nest.DirectRunner")
  val sepRunner = sepRunnerClass.newInstance()

  val stringClass = Class.forName("java.lang.String")
  val sepMainMethod =
    sepRunnerClass.getMethod("main", Array(stringClass))

  def main(args: String) {
    val cargs: Array[AnyRef] = Array(args)
    sepMainMethod.invoke(sepRunner, cargs)
  }
}

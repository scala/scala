/* NEST (New Scala Test)
 * Copyright 2007-2008 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest.nest

/* This class is used to load an instance of DirectRunner using
 * a custom class loader.
 * The purpose is to "auto-detect" a good classpath for the
 * rest of the classes (Worker, CompileManager etc.), so that
 * the main NestRunner can be started merely by putting its
 * class on the classpath (ideally).
 */
class ReflectiveRunner extends RunnerUtils {
  // TODO: we might also use fileManager.CLASSPATH
  // to use the same classes as used by `scala` that
  // was used to start the runner.

  def main(args: String) {
    val argList = List.fromArray(args.split("\\s"))

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
      else if (argList contains "--four")
        new ConsoleFileManager("build/four-pack", false, "-target:jvm-1.4")
      else // auto detection
        new ConsoleFileManager

    import fileManager.{latestCompFile, latestLibFile, latestActFile,
                        latestPartestFile, latestFjbgFile}

    val sepUrls = if (!classPath.isEmpty)
      Array(latestCompFile.toURL, latestLibFile.toURL,
            latestActFile.toURL, latestPartestFile.toURL,
            latestFjbgFile.toURL)
    else
      Array(latestCompFile.toURL, latestLibFile.toURL,
            latestActFile.toURL, latestPartestFile.toURL)

    val sepLoader = new java.net.URLClassLoader(sepUrls, null)

    if (fileManager.debug) {
      println("Loading classes from:")
      sepUrls foreach { url => println(url) }
    }

    try {
      val newClasspath = sepUrls.mkString(java.io.File.pathSeparator)
      System.setProperty("java.class.path", newClasspath)
      System.setProperty("env.classpath", newClasspath)
      System.setProperty("scala.home", "")
      if (fileManager.debug) {
 	println("java.class.path: "+System.getProperty("java.class.path"))
 	println("env.classpath: "+System.getProperty("env.classpath"))
 	println("sun.boot.class.path: "+System.getProperty("sun.boot.class.path"))
 	println("java.ext.dirs: "+System.getProperty("java.ext.dirs"))
      }

      val sepRunnerClass =
        sepLoader.loadClass("scala.tools.partest.nest.ConsoleRunner")

      val sepRunner = sepRunnerClass.newInstance()

      val stringClass = Class.forName("java.lang.String")
      val sepMainMethod =
        sepRunnerClass.getMethod("main", Array(stringClass): _*)

      val cargs: Array[AnyRef] = Array(args)
      sepMainMethod.invoke(sepRunner, cargs: _*)
    } catch {
      case cnfe: ClassNotFoundException =>
        cnfe.printStackTrace()
        NestUI.failure("scala.tools.partest.nest.ConsoleRunner could not be loaded from: \n")
        sepUrls foreach { url => NestUI.failure(url+"\n") }
    }
  }
}

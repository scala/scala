/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Philipp Haller
 */



package scala.tools.partest
package nest

import java.io.{ FilenameFilter, IOException }
import java.net.URI
import scala.util.Properties.{ propOrElse, scalaCmd, scalacCmd }
import scala.tools.nsc.{ io, util }
import PathResolver.{ Environment, Defaults }

object ConsoleFileManager {
  val classPath = {
    val srcDir = {
      val src = PathSettings.srcDir
      if (!src.isDirectory) {
        NestUI.failure("Source directory \"" + src.path + "\" not found")
        sys.exit(1)
      }
      src
    }
    val libs = (srcDir / Directory("lib")).files filter (_ hasExtension "jar") map (_.toCanonical.path)

    // add all jars in libs
    val cp = (PartestDefaults.classPath ++ libs.toList)
    vlog("testClassPath: " + cp)
    cp map (Path(_))
  }

  def mostRecentTrifecta(testBuild: Option[String], testClasses: Option[String]) = {
    val testParent     = PathSettings.testRoot.parent
    val testClassesDir = testClasses map (tc => Path(tc).toCanonical.toDirectory)
    val testBuildDir   = testBuild map (b => (testParent / b).toCanonical.toDirectory)

    vlog("test parent: "+testParent)

    def prefixFileWith(parent: File, relPath: String) = (SFile(parent) / relPath).toCanonical
    def prefixFile(relPath: String) = (testParent / relPath).toCanonical

    (testClassesDir map { testClassesDir => vlog(s"Running with classes in $testClassesDir")
      (testClassesDir / "library",
       testClassesDir / "reflect",
       testClassesDir / "compiler")
    }) orElse (testBuildDir map { testBuild => vlog(s"Running on $testBuild")
      (testBuild / "lib/scala-library.jar",
       testBuild / "lib/scala-reflect.jar",
       testBuild / "lib/scala-compiler.jar")
    }) getOrElse {
      def setupQuick = {
        vlog("Running build/quick")
        (prefixFile("build/quick/classes/library"),
         prefixFile("build/quick/classes/reflect"),
         prefixFile("build/quick/classes/compiler"))
      }

      def setupInst = {
        vlog("Running dist (installed)")
        val p = testParent.getParentFile
        (prefixFileWith(p, "lib/scala-library.jar"),
         prefixFileWith(p, "lib/scala-reflect.jar"),
         prefixFileWith(p, "lib/scala-compiler.jar"))
      }

      def setupDist = {
        vlog("Running dists/latest")
        (prefixFile("dists/latest/lib/scala-library.jar"),
         prefixFile("dists/latest/lib/scala-reflect.jar"),
         prefixFile("dists/latest/lib/scala-compiler.jar"))
      }

      def setupPack = {
        vlog("Running build/pack")
        (prefixFile("build/pack/lib/scala-library.jar"),
         prefixFile("build/pack/lib/scala-reflect.jar"),
         prefixFile("build/pack/lib/scala-compiler.jar"))
      }

      def mostRecentOf(base: String, names: String*) =
        names map (x => prefixFile(base + "/" + x).lastModified) reduceLeft (_ max _)

      // detect most recent build
      val quickTime = mostRecentOf("build/quick/classes", "compiler/compiler.properties", "reflect/reflect.properties", "library/library.properties")
      val instTime  = mostRecentOf("lib", "scala-compiler.jar", "scala-reflect.jar", "scala-library.jar")
      val distTime  = mostRecentOf("dists/latest/lib", "scala-compiler.jar", "scala-reflect.jar", "scala-library.jar")
      val packTime  = mostRecentOf("build/pack/lib", "scala-compiler.jar", "scala-reflect.jar", "scala-library.jar")

      val pairs = Map(
        (quickTime, () => setupQuick),
        (instTime,  () => setupInst),
        (distTime,  () => setupDist),
        (packTime,  () => setupPack)
      )

      // run setup based on most recent time
      pairs(pairs.keys max)()
    }
  }
}

case class ConsoleFileManager(testBuild: Option[String] = PartestDefaults.testBuild, testClasses: Option[String] = None)
  extends FileManager(ConsoleFileManager.classPath, ConsoleFileManager.mostRecentTrifecta(testBuild, testClasses))
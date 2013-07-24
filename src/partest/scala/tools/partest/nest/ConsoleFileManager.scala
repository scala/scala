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
  private def mostRecentTrifecta(testBuild: Option[String], testClasses: Option[String]) = {
    import PathSettings.testParent
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

  def classPathFromMostRecentTrifecta(testBuild: Option[String], testClasses: Option[String]): List[Path] = {
    val (library, reflect, compiler) = mostRecentTrifecta(testBuild, testClasses)

    val srcDir = {
      val src = PathSettings.srcDir
      if (!src.isDirectory) {
        NestUI.failure("Source directory \"" + src.path + "\" not found")
        sys.exit(1)
      }
      src
    }
    val libs = (srcDir / Directory("lib")).files filter (_ hasExtension "jar") map (file => Path(file.toCanonical.path))

    // def classPath   = propOrElse("partest.classpath", "")
    val userCp = ClassPath split PathResolver.Environment.javaUserClassPath map (Path(_))

    val usingJars = library.getAbsolutePath endsWith ".jar"
    // basedir for jars or classfiles on core classpath
    val baseDir = SFile(library).parent

    def relativeToLibrary(what: String): Path = {
      if (usingJars) (baseDir / s"$what.jar")
      else (baseDir.parent / "classes" / what)
    }

    // all jars or dirs with prefix `what`
    def relativeToLibraryAll(what: String): Iterator[Path] = (
      if (usingJars) FileManager.jarsWithPrefix(baseDir, what)
      else FileManager.dirsWithPrefix(baseDir.parent / "classes" toDirectory, what)
    )

    userCp ++ List[Path](
      library, reflect, compiler,
      relativeToLibrary("scala-actors"),
      relativeToLibrary("scala-parser-combinators"),
      relativeToLibrary("scala-xml"),
      relativeToLibrary("scala-scaladoc"),
      relativeToLibrary("scala-interactive"),
      relativeToLibrary("scalap"),
      PathSettings.diffUtils.fold(sys.error, identity)
    ) ++ relativeToLibraryAll("scala-partest") ++ libs
  }
}

case class ConsoleFileManager(testBuild: Option[String] = PartestDefaults.testBuild, testClasses: Option[String] = None) extends FileManager(ConsoleFileManager.classPathFromMostRecentTrifecta(testBuild, testClasses))

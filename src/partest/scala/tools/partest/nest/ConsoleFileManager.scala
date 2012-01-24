/* NEST (New Scala Test)
 * Copyright 2007-2011 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest
package nest

import java.io.{ File, FilenameFilter, IOException, StringWriter }
import java.net.URI
import scala.util.Properties.{ propOrElse, scalaCmd, scalacCmd }
import scala.tools.util.PathResolver
import scala.tools.nsc.{ io, util }
import util.{ ClassPath }
import io.{ Path, Directory }
import File.pathSeparator
import ClassPath.{ join }
import PathResolver.{ Environment, Defaults }
import RunnerUtils._


class ConsoleFileManager extends FileManager {
  var testBuild: Option[String] = PartestDefaults.testBuild
  def testBuildFile = testBuild map (testParent / _)

  var testClasses: Option[String] = None

  def this(buildPath: String, rawClasses: Boolean) = {
    this()
    if (rawClasses)
      testClasses = Some(buildPath)
    else
      testBuild = Some(buildPath)
    // re-run because initialization of default
    // constructor must be updated
    findLatest()
  }

  def this(buildPath: String) = {
    this(buildPath, false)
  }

  def this(buildPath: String, rawClasses: Boolean, moreOpts: String) = {
    this(buildPath, rawClasses)
    SCALAC_OPTS = SCALAC_OPTS ++ moreOpts.split(' ').toSeq.filter(_.length > 0)
  }

  lazy val srcDir        = PathSettings.srcDir
  lazy val testRootDir   = PathSettings.testRoot
  lazy val testRootPath  = testRootDir.toAbsolute.path
  def testParent    = testRootDir.parent

  var CLASSPATH   = PartestDefaults.classPath
  var JAVACMD     = PartestDefaults.javaCmd
  var JAVAC_CMD   = PartestDefaults.javacCmd


  NestUI.verbose("CLASSPATH: "+CLASSPATH)

  if (!srcDir.isDirectory) {
    NestUI.failure("Source directory \"" + srcDir.path + "\" not found")
    sys.exit(1)
  }

  CLASSPATH = {
    val libs = (srcDir / Directory("lib")).files filter (_ hasExtension "jar") map (_.toCanonical.path)

    // add all jars in libs
    (CLASSPATH :: libs.toList) mkString pathSeparator
  }

  def findLatest() {
    NestUI.verbose("test parent: "+testParent)

    def prefixFileWith(parent: File, relPath: String) = (io.File(parent) / relPath).toCanonical
    def prefixFile(relPath: String) = (testParent / relPath).toCanonical

    if (!testClasses.isEmpty) {
      testClassesDir = Path(testClasses.get).toCanonical.toDirectory
      NestUI.verbose("Running with classes in "+testClassesDir)

      latestFile        = testClassesDir.parent / "bin"
      latestLibFile     = testClassesDir / "library"
      latestCompFile    = testClassesDir / "compiler"
      latestPartestFile = testClassesDir / "partest"
      latestFjbgFile    = testParent / "lib" / "fjbg.jar"
    }
    else if (testBuild.isDefined) {
      val dir = Path(testBuild.get)
      NestUI.verbose("Running on "+dir)
      latestFile        = dir / "bin"
      latestLibFile     = dir / "lib/scala-library.jar"
      latestCompFile    = dir / "lib/scala-compiler.jar"
      latestPartestFile = dir / "lib/scala-partest.jar"
    }
    else {
      def setupQuick() {
        NestUI.verbose("Running build/quick")
        latestFile        = prefixFile("build/quick/bin")
        latestLibFile     = prefixFile("build/quick/classes/library")
        latestCompFile    = prefixFile("build/quick/classes/compiler")
        latestPartestFile = prefixFile("build/quick/classes/partest")
      }

      def setupInst() {
        NestUI.verbose("Running dist (installed)")
        val p = testParent.getParentFile
        latestFile        = prefixFileWith(p, "bin")
        latestLibFile     = prefixFileWith(p, "lib/scala-library.jar")
        latestCompFile    = prefixFileWith(p, "lib/scala-compiler.jar")
        latestPartestFile = prefixFileWith(p, "lib/scala-partest.jar")
      }

      def setupDist() {
        NestUI.verbose("Running dists/latest")
        latestFile        = prefixFile("dists/latest/bin")
        latestLibFile     = prefixFile("dists/latest/lib/scala-library.jar")
        latestCompFile    = prefixFile("dists/latest/lib/scala-compiler.jar")
        latestPartestFile = prefixFile("dists/latest/lib/scala-partest.jar")
      }

      def setupPack() {
        NestUI.verbose("Running build/pack")
        latestFile        = prefixFile("build/pack/bin")
        latestLibFile     = prefixFile("build/pack/lib/scala-library.jar")
        latestCompFile    = prefixFile("build/pack/lib/scala-compiler.jar")
        latestPartestFile = prefixFile("build/pack/lib/scala-partest.jar")
      }

      val dists = testParent / "dists"
      val build = testParent / "build"
      // in case of an installed dist, testRootDir is one level deeper
      val bin = testParent.parent / "bin"

      def mostRecentOf(base: String, names: String*) =
        names map (x => prefixFile(base + "/" + x).lastModified) reduceLeft (_ max _)

      // detect most recent build
      val quickTime = mostRecentOf("build/quick/classes", "compiler/compiler.properties", "library/library.properties")
      val packTime  = mostRecentOf("build/pack/lib", "scala-compiler.jar", "scala-library.jar")
      val distTime  = mostRecentOf("dists/latest/lib", "scala-compiler.jar", "scala-library.jar")
      val instTime  = mostRecentOf("lib", "scala-compiler.jar", "scala-library.jar")

      val pairs = Map(
        (quickTime, () => setupQuick()),
        (packTime,  () => setupPack()),
        (distTime,  () => setupDist()),
        (instTime,  () => setupInst())
      )

      // run setup based on most recent time
      pairs(pairs.keys max)()

      latestFjbgFile = prefixFile("lib/fjbg.jar")
    }

    LATEST_LIB = latestLibFile.getAbsolutePath
    LATEST_COMP = latestCompFile.getAbsolutePath
    LATEST_PARTEST = latestPartestFile.getAbsolutePath
  }

  var LATEST_LIB: String = ""
  var LATEST_COMP: String = ""
  var LATEST_PARTEST: String = ""

  var latestFile: File = _
  var latestLibFile: File = _
  var latestCompFile: File = _
  var latestPartestFile: File = _
  var latestFjbgFile: File = _
  def latestScalapFile: File = (latestLibFile.parent / "scalap.jar").jfile
  var testClassesDir: Directory = _
  // initialize above fields
  findLatest()

  var testFiles: List[io.Path] = Nil

  def getFiles(kind: String, cond: Path => Boolean): List[File] = {
    def ignoreDir(p: Path) = List("svn", "obj") exists (p hasExtension _)

    val dir = Directory(srcDir / kind)

    if (dir.isDirectory) NestUI.verbose("look in %s for tests" format dir)
    else NestUI.failure("Directory '%s' not found" format dir)

    val files =
      if (testFiles.nonEmpty) testFiles filter (_.parent isSame dir)
      else dir.list filterNot ignoreDir filter cond toList

    ( if (failed) files filter (x => logFileExists(x, kind)) else files ) map (_.jfile)
  }
}

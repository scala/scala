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

  var COMPILATION_CLASSPATH   = PartestDefaults.classPath
  var JAVACMD     = PartestDefaults.javaCmd
  var JAVAC_CMD   = PartestDefaults.javacCmd

  override def compilerUnderTest =
    (if (testClasses.isDefined) testClassesDir
    else testBuildFile getOrElse {
      latestCompFile.getParentFile.getParentFile.getAbsoluteFile
    }).toString


  vlog("COMPILATION_CLASSPATH: "+COMPILATION_CLASSPATH)

  if (!srcDir.isDirectory) {
    NestUI.failure("Source directory \"" + srcDir.path + "\" not found")
    sys.exit(1)
  }

  COMPILATION_CLASSPATH = {
    val libs = (srcDir / Directory("lib")).files filter (_ hasExtension "jar") map (_.toCanonical.path)

    // add all jars in libs
    (COMPILATION_CLASSPATH :: libs.toList) mkString pathSeparator
  }

  def findLatest() {
    vlog("test parent: "+testParent)

    def prefixFileWith(parent: File, relPath: String) = (SFile(parent) / relPath).toCanonical
    def prefixFile(relPath: String) = (testParent / relPath).toCanonical

    if (!testClasses.isEmpty) {
      testClassesDir = Path(testClasses.get).toCanonical.toDirectory
      vlog("Running with classes in "+testClassesDir)

      latestLibFile     = testClassesDir / "library"
      latestReflectFile = testClassesDir / "reflect"
      latestCompFile    = testClassesDir / "compiler"
    }
    else if (testBuild.isDefined) {
      val dir = Path(testBuild.get)
      vlog("Running on "+dir)
      latestLibFile     = dir / "lib/scala-library.jar"
      latestReflectFile = dir / "lib/scala-reflect.jar"
      latestCompFile    = dir / "lib/scala-compiler.jar"
    }
    else {
      def setupQuick() {
        vlog("Running build/quick")
        latestLibFile     = prefixFile("build/quick/classes/library")
        latestReflectFile = prefixFile("build/quick/classes/reflect")
        latestCompFile    = prefixFile("build/quick/classes/compiler")
      }

      def setupInst() {
        vlog("Running dist (installed)")
        val p = testParent.getParentFile
        latestLibFile     = prefixFileWith(p, "lib/scala-library.jar")
        latestReflectFile = prefixFileWith(p, "lib/scala-reflect.jar")
        latestCompFile    = prefixFileWith(p, "lib/scala-compiler.jar")
      }

      def setupDist() {
        vlog("Running dists/latest")
        latestLibFile     = prefixFile("dists/latest/lib/scala-library.jar")
        latestReflectFile = prefixFile("dists/latest/lib/scala-reflect.jar")
        latestCompFile    = prefixFile("dists/latest/lib/scala-compiler.jar")
      }

      def setupPack() {
        vlog("Running build/pack")
        latestLibFile     = prefixFile("build/pack/lib/scala-library.jar")
        latestReflectFile = prefixFile("build/pack/lib/scala-reflect.jar")
        latestCompFile    = prefixFile("build/pack/lib/scala-compiler.jar")
      }

      def mostRecentOf(base: String, names: String*) =
        names map (x => prefixFile(base + "/" + x).lastModified) reduceLeft (_ max _)

      // detect most recent build
      val quickTime = mostRecentOf("build/quick/classes", "compiler/compiler.properties", "reflect/reflect.properties", "library/library.properties")
      val packTime  = mostRecentOf("build/pack/lib", "scala-compiler.jar", "scala-reflect.jar", "scala-library.jar")
      val distTime  = mostRecentOf("dists/latest/lib", "scala-compiler.jar", "scala-reflect.jar", "scala-library.jar")
      val instTime  = mostRecentOf("lib", "scala-compiler.jar", "scala-reflect.jar", "scala-library.jar")

      val pairs = Map(
        (quickTime, () => setupQuick()),
        (packTime,  () => setupPack()),
        (distTime,  () => setupDist()),
        (instTime,  () => setupInst())
      )

      // run setup based on most recent time
      pairs(pairs.keys max)()
    }

    LATEST_LIB = latestLibFile.getAbsolutePath
    LATEST_REFLECT = latestReflectFile.getAbsolutePath
    LATEST_COMP = latestCompFile.getAbsolutePath
  }

  var LATEST_LIB: String = ""
  var LATEST_REFLECT: String = ""
  var LATEST_COMP: String = ""

  var latestLibFile: File = _
  var latestReflectFile: File = _
  var latestCompFile: File = _
  var testClassesDir: Directory = _
  // initialize above fields
  findLatest()
}

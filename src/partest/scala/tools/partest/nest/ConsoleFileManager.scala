/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest
package nest

import java.io.{ File, FilenameFilter, IOException, StringWriter }
import java.net.URI
import scala.tools.util.PathResolver
import scala.tools.nsc.io
import io.{ Path, Directory }
import File.pathSeparator
import PathResolver.{ propOrElse }

class ConsoleFileManager extends FileManager {
  implicit private def tempPathConversion(x: Path): File = x.jfile

  var testBuild: Option[String] = Option(System.getProperty("scalatest.build"))
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
    SCALAC_OPTS = SCALAC_OPTS+" "+moreOpts
  }

  var CLASSPATH = PathResolver.Environment.javaUserClassPath

  NestUI.verbose("CLASSPATH: "+CLASSPATH)

  var JAVACMD   = propOrElse("scalatest.javacmd", "java")
  var JAVAC_CMD = propOrElse("scalatest.javac_cmd", "javac")

  val prefixDir = Directory.Current map (_.normalize.toDirectory) getOrElse error("user.dir property not set")
  val PREFIX    = prefixDir.toAbsolute.path

/*
if [ -d "$PREFIX/test" ]; then
    TESTROOT="$PREFIX/test";
elif [ -d "$PREFIX/misc/scala-test" ]; then
    TESTROOT="$PREFIX/misc/scala-test";
else
    abort "Test directory not found";
*/

  val testRootDir = {
    val testRootProp = Option(propOrElse("scalatest.root", null)) map (x => Directory(x))
    def isTestDir(d: Directory) = d.name == "test" && (d / "files" isDirectory)

    (
      testRootProp orElse (
        if (isTestDir(prefixDir)) Some(prefixDir) else None   // cwd is `test`
      ) orElse (
        (prefixDir / "test") ifDirectory (x => x)             // cwd is `test/..`
      ) orElse (
        (prefixDir / "misc" / "scala-test") ifDirectory (x => x)
      ) getOrElse (
        error("Test directory not found")
      )
    ).normalize
  }
  val TESTROOT = testRootDir.toAbsolute.path

  def testParent = testRootDir.parent

  var srcDirName: String = ""

  val srcDir: io.Directory = {
    srcDirName = Option(System.getProperty("partest.srcdir")) getOrElse "files"
    val src = testRootDir / srcDirName

    if (src.isDirectory) src.toDirectory
    else {
      NestUI.failure("Source directory \"" + src.path + "\" not found")
      exit(1)
    }
  }

  LIB_DIR = (testParent / "lib").normalize.toAbsolute.path

  CLASSPATH = {
    val libs = (srcDir / Directory("lib")).files filter (_ hasExtension "jar") map (_.normalize.toAbsolute.path)

    // add all jars in libs
    (CLASSPATH :: libs.toList) mkString pathSeparator
  }

  def findLatest() {
    NestUI.verbose("test parent: "+testParent)

    def prefixFileWith(parent: File, relPath: String) = (io.File(parent) / relPath).normalize
    def prefixFile(relPath: String) = (testParent / relPath).normalize

    if (!testClasses.isEmpty) {
      testClassesDir = Path(testClasses.get).normalize.toDirectory
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
      pairs(pairs.keysIterator.toList max)()

      latestFjbgFile = prefixFile("lib/fjbg.jar")
    }

    BIN_DIR = latestFile.getAbsolutePath
    LATEST_LIB = latestLibFile.getAbsolutePath
    LATEST_COMP = latestCompFile.getAbsolutePath
    LATEST_PARTEST = latestPartestFile.getAbsolutePath

    import util.Properties.isWin

    val scalaCommand  = if (isWin) "scala.bat" else "scala"
    val scalacCommand = if (isWin) "scalac.bat" else "scalac"

    SCALA = (new File(latestFile, scalaCommand)).getAbsolutePath
    SCALAC_CMD = (new File(latestFile, scalacCommand)).getAbsolutePath
  }

  var BIN_DIR: String = ""
  var LATEST_LIB: String = ""
  var LATEST_COMP: String = ""
  var LATEST_PARTEST: String = ""
  var SCALA: String = ""
  var SCALAC_CMD: String = ""

  var latestFile: File = _
  var latestLibFile: File = _
  var latestCompFile: File = _
  var latestPartestFile: File = _
  var latestFjbgFile: File = _
  var testClassesDir: Directory = _
  // initialize above fields
  findLatest()

  var testFiles: List[File] = List()

  def getFiles(kind: String, doCheck: Boolean, filter: Option[(String, Boolean)]): List[File] = {

    val dir = new File(srcDir, kind)
    NestUI.verbose("look in "+dir+" for tests")
    val files = if (dir.isDirectory) {
      if (!testFiles.isEmpty) {
        val dirpath = dir.getAbsolutePath
        testFiles filter { _.getParentFile.getAbsolutePath == dirpath }
      } else if (doCheck) filter match {
        case Some((ending, enableDirs)) =>
          val filter = new FilenameFilter {
            def accept(dir: File, name: String) =
              name.endsWith(ending) ||
              (enableDirs && (name != ".svn") && (!name.endsWith(".obj")) &&
              (new File(dir, name)).isDirectory)
          }
          dir.listFiles(filter).toList
        case None =>
          val filter = new FilenameFilter {
            def accept(dir: File, name: String) = name != ".svn"
          }
          dir.listFiles(filter).toList
      } else // skip
        Nil
    } else {
      NestUI.failure("Directory \"" + dir.getPath + "\" not found")
      Nil
    }
    if (failed)
      files filter { logFileExists(_, kind) }
    else
      files
  }

  def getFiles(kind: String, doCheck: Boolean): List[File] =
    getFiles(kind, doCheck, Some((".scala", true)))

}

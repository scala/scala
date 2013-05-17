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

  var CLASSPATH   = PartestDefaults.classPath
  var JAVACMD     = PartestDefaults.javaCmd
  var JAVAC_CMD   = PartestDefaults.javacCmd


  vlog("CLASSPATH: "+CLASSPATH)

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
    vlog("test parent: "+testParent)

    def prefixFileWith(parent: File, relPath: String) = (SFile(parent) / relPath).toCanonical
    def prefixFile(relPath: String) = (testParent / relPath).toCanonical

    if (!testClasses.isEmpty) {
      testClassesDir = Path(testClasses.get).toCanonical.toDirectory
      vlog("Running with classes in "+testClassesDir)

      latestLibFile     = testClassesDir / "library"
      latestActorsFile  = testClassesDir / "library" / "actors"
      latestReflectFile = testClassesDir / "reflect"
      latestCompFile    = testClassesDir / "compiler"
      latestPartestFile = testClassesDir / "partest"
    }
    else if (testBuild.isDefined) {
      val dir = Path(testBuild.get)
      vlog("Running on "+dir)
      latestLibFile     = dir / "lib/scala-library.jar"
      latestActorsFile  = dir / "lib/scala-actors.jar"
      latestReflectFile = dir / "lib/scala-reflect.jar"
      latestCompFile    = dir / "lib/scala-compiler.jar"
      latestPartestFile = dir / "lib/scala-partest.jar"
    }
    else {
      def setupQuick() {
        vlog("Running build/quick")
        latestLibFile     = prefixFile("build/quick/classes/library")
        latestActorsFile  = prefixFile("build/quick/classes/library/actors")
        latestReflectFile = prefixFile("build/quick/classes/reflect")
        latestCompFile    = prefixFile("build/quick/classes/compiler")
        latestPartestFile = prefixFile("build/quick/classes/partest")
      }

      def setupInst() {
        vlog("Running dist (installed)")
        val p = testParent.getParentFile
        latestLibFile     = prefixFileWith(p, "lib/scala-library.jar")
        latestActorsFile  = prefixFileWith(p, "lib/scala-actors.jar")
        latestReflectFile = prefixFileWith(p, "lib/scala-reflect.jar")
        latestCompFile    = prefixFileWith(p, "lib/scala-compiler.jar")
        latestPartestFile = prefixFileWith(p, "lib/scala-partest.jar")
      }

      def setupDist() {
        vlog("Running dists/latest")
        latestLibFile     = prefixFile("dists/latest/lib/scala-library.jar")
        latestActorsFile  = prefixFile("dists/latest/lib/scala-actors.jar")
        latestReflectFile = prefixFile("dists/latest/lib/scala-reflect.jar")
        latestCompFile    = prefixFile("dists/latest/lib/scala-compiler.jar")
        latestPartestFile = prefixFile("dists/latest/lib/scala-partest.jar")
      }

      def setupPack() {
        vlog("Running build/pack")
        latestLibFile     = prefixFile("build/pack/lib/scala-library.jar")
        latestActorsFile  = prefixFile("build/pack/lib/scala-actors.jar")
        latestReflectFile = prefixFile("build/pack/lib/scala-reflect.jar")
        latestCompFile    = prefixFile("build/pack/lib/scala-compiler.jar")
        latestPartestFile = prefixFile("build/pack/lib/scala-partest.jar")
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
    LATEST_PARTEST = latestPartestFile.getAbsolutePath
    LATEST_ACTORS = latestActorsFile.getAbsolutePath
  }

  var LATEST_LIB: String = ""
  var LATEST_REFLECT: String = ""
  var LATEST_COMP: String = ""
  var LATEST_PARTEST: String = ""
  var LATEST_ACTORS: String = ""

  var latestLibFile: File = _
  var latestActorsFile: File = _
  var latestReflectFile: File = _
  var latestCompFile: File = _
  var latestPartestFile: File = _
  //def latestScalapFile: File = (latestLibFile.parent / "scalap.jar").jfile
  //def latestScalapFile: File = new File(latestLibFile.getParentFile, "scalap.jar")
  var testClassesDir: Directory = _
  // initialize above fields
  findLatest()

  /*
  def getFiles(kind: String, cond: Path => Boolean): List[File] = {
    def ignoreDir(p: Path) = List("svn", "obj") exists (p hasExtension _)

    val dir = Directory(srcDir / kind)

    if (dir.isDirectory) NestUI.verbose("look in %s for tests" format dir)
    else NestUI.failure("Directory '%s' not found" format dir)

    val files = dir.list filterNot ignoreDir filter cond toList

    ( if (failed) files filter (x => logFileExists(x, kind)) else files ) map (_.jfile)
  }
  */
  var latestFjbgFile: File = _
}

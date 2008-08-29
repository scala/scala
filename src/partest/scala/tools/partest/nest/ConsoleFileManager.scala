/* NEST (New Scala Test)
 * Copyright 2007-2008 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest.nest

import java.io.{File, FilenameFilter, IOException, StringWriter}
import java.net.URI

class ConsoleFileManager extends FileManager {

  var testBuild = System.getProperty("scalatest.build")
  var testClasses: Option[String] = None

  val debug: Boolean =
    (System.getProperty("partest.debug", "false") equals "true") ||
    (System.getProperty("scalatest.debug", "false") equals "true")

  def this(buildPath: String, rawClasses: Boolean) = {
    this()
    if (rawClasses)
      testClasses = Some(buildPath)
    else
      testBuild = buildPath
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

  var CLASSPATH = System.getProperty("java.class.path", ".")
  NestUI.verbose("CLASSPATH: "+CLASSPATH)

  var JAVACMD   = System.getProperty("scalatest.javacmd", "java")
  val prefixFile = {
    val cwd = System.getProperty("user.dir")
    if (cwd != null)
      (new File(cwd)).getCanonicalFile
    else
      error("user.dir property not set")
  }
  val PREFIX = prefixFile.getAbsolutePath

/*
if [ -d "$PREFIX/test" ]; then
    TESTROOT="$PREFIX/test";
elif [ -d "$PREFIX/misc/scala-test" ]; then
    TESTROOT="$PREFIX/misc/scala-test";
else
    abort "Test directory not found";
*/

  val testRootFile = {
    val testRootProp = System.getProperty("scalatest.root")
    val testroot =
      if (testRootProp != null)
        new File(testRootProp)
      else {
        // case 1: cwd is `test`
        if (prefixFile.getName == "test" && (new File(prefixFile, "files")).exists)
          prefixFile
        else {
        // case 2: cwd is `test/..`
          val test = new File(prefixFile, "test")
          val scalaTest = new File(new File(prefixFile, "misc"), "scala-test")
          if (test.isDirectory)
            test
          else if (scalaTest.isDirectory)
            scalaTest
          else
            error("Test directory not found")
        }
      }
    testroot.getCanonicalFile
  }
  val TESTROOT = testRootFile.getAbsolutePath

  CLASSPATH = CLASSPATH + File.pathSeparator + {
    val libs = new File(TESTROOT, "files/lib")
    // add all jars in libs
    (libs.listFiles(new FilenameFilter {
      def accept(dir: File, name: String) = name endsWith ".jar"
    }) map {file => file.getCanonicalFile.getAbsolutePath}).mkString(""+File.pathSeparator)
  }

  def findLatest() {
    val testParent = testRootFile.getParentFile
    NestUI.verbose("test parent: "+testParent)

    def prefixFileWith(parent: File, relPath: String): File =
      (new File(parent, relPath)).getCanonicalFile

    def prefixFile(relPath: String): File =
      prefixFileWith(testParent, relPath)

    if (!testClasses.isEmpty) {
      testClassesFile = (new File(testClasses.get)).getCanonicalFile
      NestUI.verbose("Running with classes in "+testClassesFile)
      latestFile        = prefixFileWith(testClassesFile.getParentFile, "bin")
      latestLibFile     = prefixFileWith(testClassesFile, "library")
      latestActFile     = prefixFileWith(testClassesFile, "library")
      latestCompFile    = prefixFileWith(testClassesFile, "compiler")
      latestPartestFile = prefixFileWith(testClassesFile, "partest")
      latestFjbgFile    = prefixFile("lib/fjbg.jar")
    }
    else if (testBuild != null) {
      testBuildFile = prefixFile(testBuild)
      NestUI.verbose("Running on "+testBuild)
      latestFile        = prefixFile(testBuild+"/bin")
      latestLibFile     = prefixFile(testBuild+"/lib/scala-library.jar")
      latestActFile     = prefixFile(testBuild+"/lib/scala-library.jar")
      latestCompFile    = prefixFile(testBuild+"/lib/scala-compiler.jar")
      latestPartestFile = prefixFile(testBuild+"/lib/scala-partest.jar")
    } else {
      val dists = new File(testParent, "dists")
      val build = new File(testParent, "build")
      // in case of an installed dist, testRootFile is one level deeper
      val bin = new File(testParent.getParentFile, "bin")

      if (dists.isDirectory) {
        NestUI.verbose("Running on DISTRIBUTION")
        latestFile        = prefixFile("dists/latest/bin")
        latestLibFile     = prefixFile("dists/latest/lib/scala-library.jar")
        latestActFile     = prefixFile("dists/latest/lib/scala-library.jar")
        latestCompFile    = prefixFile("dists/latest/lib/scala-compiler.jar")
        latestPartestFile = prefixFile("dists/latest/lib/scala-partest.jar")
      }
      else if (build.isDirectory && (new File(build, "pack/lib/scala-library.jar")).exists) {
        NestUI.verbose("Running on SuperSABBUS PACK")
        latestFile        = prefixFile("build/pack/bin")
        latestLibFile     = prefixFile("build/pack/lib/scala-library.jar")
        latestActFile     = prefixFile("build/pack/lib/scala-library.jar")
        latestCompFile    = prefixFile("build/pack/lib/scala-compiler.jar")
        latestPartestFile = prefixFile("build/pack/lib/scala-partest.jar")
      }
      else if (build.isDirectory) {
        NestUI.verbose("Running on SABBUS QUICK")
        latestFile        = prefixFile("build/quick/bin")
        latestLibFile     = prefixFile("build/quick/lib/library")
        latestActFile     = prefixFile("build/quick/lib/actors")
        latestCompFile    = prefixFile("build/quick/lib/compiler")
        latestPartestFile = prefixFile("build/quick/lib/partest")
      }
      else if (bin.isDirectory) {
        NestUI.verbose("Running on INSTALLED DIST")
        val p = testParent.getParentFile
        latestFile        = prefixFileWith(p, "bin")
        latestLibFile     = prefixFileWith(p, "lib/scala-library.jar")
        latestActFile     = prefixFileWith(p, "lib/scala-library.jar")
        latestCompFile    = prefixFileWith(p, "lib/scala-compiler.jar")
        latestPartestFile = prefixFileWith(p, "lib/scala-partest.jar")
      }
      else
        error("Scala binaries could not be found")
    }

    BIN_DIR = latestFile.getAbsolutePath
    LATEST_LIB = latestLibFile.getAbsolutePath
    LATEST_COMP = latestCompFile.getAbsolutePath
    LATEST_PARTEST = latestPartestFile.getAbsolutePath

    // detect whether we are running on Windows
    val osName = System.getProperty("os.name")
    NestUI.verbose("OS: "+osName)

    val scalaCommand = if (osName startsWith "Windows")
      "scala.bat" else "scala"
    val scalacCommand = if (osName startsWith "Windows")
      "scalac.bat" else "scalac"

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
  var latestActFile: File = _
  var latestCompFile: File = _
  var latestPartestFile: File = _
  var latestFjbgFile: File = _
  var testBuildFile: File = _
  var testClassesFile: File = _
  // initialize above fields
  findLatest()

  val srcDir: File = {
    val src = new File(TESTROOT, "files")
    if (src.isDirectory)
      src.getCanonicalFile
    else {
      val path = TESTROOT + File.separator + "files"
      NestUI.failure("Source directory \"" + path + "\" not found")
      exit(1)
    }
  }

  private def basename(name: String): String = {
    val inx = name.lastIndexOf(".")
    if (inx < 0) name else name.substring(0, inx)
  }

  var testFiles: List[File] = List()

  def getFiles(kind: String, doCheck: Boolean, filter: Option[(String, Boolean)]): List[File] = {
    val dir = new File(srcDir, kind)
    NestUI.verbose("look in "+dir+" for tests")
    if (dir.isDirectory) {
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
  }

  def getFiles(kind: String, doCheck: Boolean): List[File] =
    getFiles(kind, doCheck, Some((".scala", false)))

}

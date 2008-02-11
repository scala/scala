/* NEST (New Scala Test)
 * @author Philipp Haller
 */

package scala.tools.partest.nest

import java.io.{File, FilenameFilter, ByteArrayOutputStream, PrintStream,
                IOException, BufferedOutputStream}
import java.net.URI

object FileManager {

  val PATH_SEP = File.pathSeparatorChar
  val CLASSPATH = System.getProperty("java.class.path", ".")
  val PREFIX = System.getProperty("user.dir", ".")+"/.."
  val JAVACMD = System.getProperty("scalatest.javacmd", "java")

/*
if [ -d "$PREFIX/test" ]; then
    TESTROOT="$PREFIX/test";
elif [ -d "$PREFIX/misc/scala-test" ]; then
    TESTROOT="$PREFIX/misc/scala-test";
else
    abort "Test directory not found";
*/
  val TESTROOT = {
    val test = new File(PREFIX, "test")
    val scala_test = new File(PREFIX, "misc/scala-test")
    val testroot =
      if (test.exists && test.isDirectory)
        test
      else if (scala_test.exists && scala_test.isDirectory)
        scala_test
      else
        error("Test directory not found")
    testroot.getAbsolutePath
  }
  val EXT_CLASSPATH = {
    val libs = new File(TESTROOT, "files/lib")
    // add all jars in libs to EXT_CLASSPATH
    (libs.listFiles(new FilenameFilter {
      def accept(dir: File, name: String) =
        name.endsWith(".jar")
    }) map {file => file.getAbsolutePath}).mkString(""+PATH_SEP)
  }

/*
if [ -d "$PREFIX/dists" ]; then
    LATEST="$PREFIX/dists/latest/bin";
    LATEST_LIB="$PREFIX/dists/latest/lib/scala-library.jar";
    LATEST_COMP="$PREFIX/dists/latest/lib/scala-compiler.jar";
    LATEST_PREDEF="$PREFIX/dists/latest/lib/predef.dll";
    LATEST_CLDC=$QUICK_CLDC;
    LATEST_CLDCAPI=$QUICK_CLDCAPI;
elif [ -d "$PREFIX/build" ]; then
    LATEST="$QUICK";
    LATEST_LIB=$QUICK_LIB;
    LATEST_COMP=$QUICK_COMP;
    LATEST_ACT=$QUICK_ACT;
    LATEST_PREDEF=$QUICK_PREDEF;
    LATEST_CLDC=$QUICK_CLDC;
    LATEST_CLDCAPI=$QUICK_CLDCAPI;
elif [ -d "$PREFIX/bin" ]; then
    LATEST="$PREFIX/bin";
    LATEST_LIB="$PREFIX/lib/scala-library.jar";
    LATEST_COMP="$PREFIX/lib/scala-compiler.jar";
    LATEST_PREDEF="$PREFIX/lib/predef.dll";
    LATEST_CLDC="$PREFIX/lib/scalaapi10-unverified.jar";
    LATEST_CLDCAPI="$PREFIX/lib/scalaapi10.jar";
*/
  def findLatest() {
    def prefixFile(relPath: String) =
      new File(PREFIX, relPath)

    NestUI.verbose("PREFIX: "+PREFIX)
    val dists = new File(PREFIX, "dists")
    val build = new File(PREFIX, "build")
    val bin = new File(PREFIX, "bin")

    if (dists.exists && dists.isDirectory) {
      latestFile = prefixFile("dists/latest/bin")
      latestLibFile = prefixFile("dists/latest/lib/scala-library.jar")
      latestCompFile = prefixFile("dists/latest/lib/scala-compiler.jar")
      latestPartestFile = prefixFile("dists/latest/lib/scala-partest.jar")
    }
    else if (build.exists && build.isDirectory) {
      latestFile = prefixFile("build/quick/bin")
      latestLibFile = prefixFile("build/quick/lib/library")
      latestCompFile = prefixFile("build/quick/lib/compiler")
      latestPartestFile = prefixFile("build/quick/lib/partest")
    }
    else if (bin.exists && bin.isDirectory) {
      latestFile = prefixFile("bin")
      latestLibFile = prefixFile("lib/scala-library.jar")
      latestCompFile = prefixFile("lib/scala-compiler.jar")
      latestPartestFile = prefixFile("lib/scala-partest.jar")
    }
    else
      error("Scala binaries could not be found")

    BIN_DIR = latestFile.getAbsolutePath
    LATEST_LIB = latestLibFile.getAbsolutePath
    LATEST_COMP = latestCompFile.getAbsolutePath
    LATEST_PARTEST = latestPartestFile.getAbsolutePath
    SCALA = (new File(latestFile, "scala")).getAbsolutePath
    SCALAC_CMD = (new File(latestFile, "scalac")).getAbsolutePath
  }

  var BIN_DIR: String = ""
  var LATEST_LIB: String = ""
  var LATEST_COMP: String = ""
  var LATEST_PARTEST: String = ""
  var SCALA: String = ""
  var SCALAC_CMD: String = ""

  val SCALAC_OPTS = System.getProperty("scalatest.scalac_opts", "-deprecation -encoding utf8")

  var latestFile: File = _
  var latestLibFile: File = _
  var latestCompFile: File = _
  var latestPartestFile: File = _
  // initialize above fields
  findLatest()

  val srcDir = {
    val dirname = System.getProperty("scalatest.cwd", "")
    val dir = if (dirname.isEmpty) { // guess
      val libDir = new File(new URI(classOf[Test].getResource("/").toString))
      val path = libDir.getAbsolutePath
      val parent = libDir.getParentFile
      val rootDir =
        if (path contains "quick") parent.getParentFile.getParentFile.getParentFile
        else if (path contains "dists") parent.getParentFile.getParentFile
        else parent
      new File(rootDir, "test" + File.separator + "files")
    } else
      new File(dirname)
    dir
  }

  if (!srcDir.isDirectory) {
    NestUI.failure("Test directory \"" + srcDir.getAbsolutePath + "\" not found")
    exit(1)
  }

  private def basename(name: String): String = {
    val inx = name.lastIndexOf(".")
    if (inx < 0) name else name.substring(0, inx)
  }

  def getLogFile(file: File, kind: String) = {
    val dir = file.getParentFile
    val fileBase = basename(file.getName)
    new File(dir, fileBase + "-" + kind + ".log")
  }

  def deleteRecursive(dir: File) {
    if (dir.isDirectory) {
      for (file <- dir.list) deleteRecursive(new File(dir, file))
    }
    dir.delete
  }

  /**
   * Compares two files using a Java implementation of the GNU diff
   * available at http://www.bmsi.com/java/#diff.
   *
   * @param f1
   * @param f2
   */
  def compareFiles(f1: File, f2: File): String = {
    var res = ""
    try {
      val diffStream = new ByteArrayOutputStream
      val diffOutput = new PrintStream(
        new BufferedOutputStream(diffStream), true)
      System.setOut(diffOutput)
      System.setErr(diffOutput)
      val args = Array(f1.getCanonicalPath(), f2.getCanonicalPath())
      DiffPrint.main(args)
      System.setOut(System.out)
      System.setErr(System.err)

      res = diffStream.toString
      if (res.startsWith("No"))
        res = ""
    } catch {
      case e: IOException =>
        e.printStackTrace()
    }
    res
  }
}

class FileManager {

  var testFiles: List[File] = List()

  def getFiles(kind: String, doCheck: Boolean): List[File] = {
    val filter = new FilenameFilter {
      def accept(dir: File, name: String): Boolean = name endsWith ".scala"
    }
    val dir = new File(FileManager.srcDir, kind)
    NestUI.verbose("look in "+dir+" for tests")
    if (dir.isDirectory) {
      if (!testFiles.isEmpty) {
        val dirpath = dir.getAbsolutePath
        testFiles filter { _.getParentFile.getAbsolutePath == dirpath }
      } else if (doCheck)
        dir.listFiles(filter).toList
        else // skip
          Nil
    } else {
      NestUI.failure("Directory \"" + dir.getPath + "\" not found")
      Nil
    }
  }
}

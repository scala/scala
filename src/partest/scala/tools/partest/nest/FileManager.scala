/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest
package nest

import java.io.{File, FilenameFilter, IOException, StringWriter,
                FileInputStream, FileOutputStream, BufferedReader,
                FileReader, PrintWriter, FileWriter}
import java.net.URI
import scala.reflect.io.AbstractFile
import scala.collection.mutable

trait FileUtil {
  /**
   * Compares two files using difflib to produce a unified diff.
   *
   * @param  f1  the first file to be compared
   * @param  f2  the second file to be compared
   * @return the unified diff of the compared files or the empty string if they're equal
   */
  def compareFiles(f1: File, f2: File): String = {
    compareContents(io.Source.fromFile(f1).getLines.toSeq, io.Source.fromFile(f2).getLines.toSeq, f1.getName, f2.getName)
  }

  /**
   * Compares two lists of lines using difflib to produce a unified diff.
   *
   * @param  origLines  the first seq of lines to be compared
   * @param  newLines   the second seq of lines to be compared
   * @param  origName   file name to be used in unified diff for `origLines`
   * @param  newName    file name to be used in unified diff for `newLines`
   * @return the unified diff of the `origLines` and `newLines` or the empty string if they're equal
   */
  def compareContents(origLines: Seq[String], newLines: Seq[String], origName: String = "a", newName: String = "b"): String = {
    import collection.JavaConverters._

    val diff = difflib.DiffUtils.diff(origLines.asJava, newLines.asJava)
    if (diff.getDeltas.isEmpty) ""
    else difflib.DiffUtils.generateUnifiedDiff(origName, newName, origLines.asJava, diff, 1).asScala.mkString("\n")
  }

  def jarsWithPrefix(dir: Directory, name: String): Iterator[SFile] =
    dir.files filter (f => (f hasExtension "jar") && (f.name startsWith name))

  def dirsWithPrefix(dir: Directory, name: String): Iterator[Directory] =
    dir.dirs filter (_.name startsWith name)
}
object FileUtil extends FileUtil { }

trait FileManager extends FileUtil {

  def testRootDir: Directory
  def testRootPath: String

  def compilerUnderTest = LATEST_COMP

  var JAVACMD: String
  var JAVAC_CMD: String

  var COMPILATION_CLASSPATH: String
  var LATEST_LIB: String
  var LATEST_REFLECT: String
  var LATEST_COMP: String

  // basedir for jars or classfiles on core classpath
  lazy val baseDir = SFile(LATEST_LIB).parent

  protected def relativeToLibrary(what: String): String = {
    if (LATEST_LIB endsWith ".jar")
      (baseDir / s"$what.jar").toAbsolute.path
    else
      (baseDir.parent / "classes" / what).toAbsolute.path
  }

  // all jars or dirs with prefix `what`
  protected def relativeToLibraryAll(what: String): Iterator[String] = (
    if (LATEST_LIB endsWith ".jar") jarsWithPrefix(baseDir, what)
    else dirsWithPrefix(baseDir.parent / "classes" toDirectory, what)
  ).map(_.toAbsolute.path)

  // this determines the classpath we're running tests under -- should just be out classpath...
  // this way we can actually use maven resolution when invoking partest to construct the classpath that has all partest's dependencies
  lazy val testClassPath: List[String] = List(
      LATEST_LIB, LATEST_REFLECT, LATEST_COMP,
      relativeToLibrary("scala-actors"),
      relativeToLibrary("scala-parser-combinators"),
      relativeToLibrary("scala-xml"),
      relativeToLibrary("scala-scaladoc"),
      relativeToLibrary("scala-interactive"),
      relativeToLibrary("scalap"),
      PathSettings.srcCodeLib.toString, // is this necessary? there's a `prependToClasspaths(s, codelib)` elsewhere
      PathSettings.diffUtils.toString,
      PathSettings.testInterface.toString,
      PathSettings.scalaCheck.toString
  ) ++ relativeToLibraryAll("scala-partest")
  def testClassPathUrls = testClassPath map (p => new java.io.File(p).toURI.toURL)


  var showDiff = false
  var updateCheck = false
  var showLog = false
  var failed = false

  var SCALAC_OPTS = PartestDefaults.scalacOpts.split(' ').toSeq
  var JAVA_OPTS   = PartestDefaults.javaOpts

  /** Only when --debug is given. */
  lazy val testTimings = new mutable.HashMap[String, Long]
  def recordTestTiming(name: String, milliseconds: Long) =
    synchronized { testTimings(name) = milliseconds }

  def getLogFile(dir: File, fileBase: String, kind: String): File =
    new File(dir, fileBase + "-" + kind + ".log")

  def getLogFile(file: File, kind: String): File = {
    val dir      = file.getParentFile
    val fileBase = basename(file.getName)

    getLogFile(dir, fileBase, kind)
  }

  def logFileExists(file: File, kind: String) =
    getLogFile(file, kind).canRead

  def overwriteFileWith(dest: File, file: File) =
    dest.isFile && copyFile(file, dest)

  def copyFile(from: File, dest: File): Boolean = {
    if (from.isDirectory) {
      assert(dest.isDirectory, "cannot copy directory to file")
      val subDir:Directory = Path(dest) / Directory(from.getName)
      subDir.createDirectory()
      from.listFiles.toList forall (copyFile(_, subDir))
    }
    else {
      val to = if (dest.isDirectory) new File(dest, from.getName) else dest

      try {
        SFile(to) writeAll SFile(from).slurp()
        true
      }
      catch { case _: IOException => false }
    }
  }

  def mapFile(file: File, replace: String => String) {
    val f = SFile(file)

    f.printlnAll(f.lines.toList map replace: _*)
  }

  /** Massage args to merge plugins and fix paths.
   *  Plugin path can be relative to test root, or cwd is out.
   *  While we're at it, mix in the baseline options, too.
   *  That's how ant passes in the plugins dir.
   */
  def updatePluginPath(args: List[String], out: AbstractFile, srcdir: AbstractFile): List[String] = {
    val dir = testRootDir
    // The given path, or the output dir if ".", or a temp dir if output is virtual (since plugin loading doesn't like virtual)
    def pathOrCwd(p: String) =
      if (p == ".") {
        val plugxml = "scalac-plugin.xml"
        val pout    = if (out.isVirtual) Directory.makeTemp() else Path(out.path)
        val srcpath = Path(srcdir.path)
        val pd      = (srcpath / plugxml).toFile
        if (pd.exists) pd copyTo (pout / plugxml)
        pout
      } else Path(p)
    def absolutize(path: String) = pathOrCwd(path) match {
      case x if x.isAbsolute  => x.path
      case x                  => (dir / x).toAbsolute.path
    }

    val xprefix          = "-Xplugin:"
    val (xplugs, others) = args partition (_ startsWith xprefix)
    val Xplugin          = if (xplugs.isEmpty) Nil else List(xprefix +
      (xplugs map (_ stripPrefix xprefix) flatMap (_ split pathSeparator) map absolutize mkString pathSeparator)
    )
    SCALAC_OPTS.toList ::: others ::: Xplugin
  }
}

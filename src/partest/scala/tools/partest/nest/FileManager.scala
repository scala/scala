/* NEST (New Scala Test)
 * Copyright 2007-2011 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest
package nest

import java.io.{File, FilenameFilter, IOException, StringWriter,
                FileInputStream, FileOutputStream, BufferedReader,
                FileReader, PrintWriter, FileWriter}
import java.net.URI
import scala.tools.nsc.io.{ Path, Directory, File => SFile }
import sys.process._
import scala.collection.mutable

trait FileUtil {
  /**
   * Compares two files using a Java implementation of the GNU diff
   * available at http://www.bmsi.com/java/#diff.
   *
   * @param  f1  the first file to be compared
   * @param  f2  the second file to be compared
   * @return the text difference between the compared files
   */
  def compareFiles(f1: File, f2: File): String = {
    val diffWriter = new StringWriter
    val args = Array(f1.getAbsolutePath(), f2.getAbsolutePath())

    DiffPrint.doDiff(args, diffWriter)
    val res = diffWriter.toString
    if (res startsWith "No") "" else res
  }
  def compareContents(lines1: Seq[String], lines2: Seq[String]): String = {
    val xs1 = lines1.toArray[AnyRef]
    val xs2 = lines2.toArray[AnyRef]

    val diff   = new Diff(xs1, xs2)
    val change = diff.diff_2(false)
    val writer = new StringWriter
    val p      = new DiffPrint.NormalPrint(xs1, xs2, writer)

    p.print_script(change)
    val res = writer.toString
    if (res startsWith "No ") ""
    else res
  }
}
object FileUtil extends FileUtil { }

trait FileManager extends FileUtil {

  def testRootDir: Directory
  def testRootPath: String

  var JAVACMD: String
  var JAVAC_CMD: String

  var CLASSPATH: String
  var LATEST_LIB: String
  var LATEST_COMP: String
  var LATEST_PARTEST: String
  var LATEST_ACTORS: String
  var LATEST_ACTORS_MIGRATION: String

  var showDiff = false
  var updateCheck = false
  var showLog = false
  var failed = false

  var SCALAC_OPTS = PartestDefaults.scalacOpts.split(' ').toSeq
  var JAVA_OPTS   = PartestDefaults.javaOpts
  var timeout     = PartestDefaults.timeout
  // how can 15 minutes not be enough? What are you doing, run/lisp.scala?
  // You complete in 11 seconds on my machine.
  var oneTestTimeout = 60 * 60 * 1000

  /** Only when --debug is given. */
  lazy val testTimings = new mutable.HashMap[String, Long]
  def recordTestTiming(name: String, milliseconds: Long) =
    synchronized { testTimings(name) = milliseconds }
  def showTestTimings() {
    testTimings.toList sortBy (-_._2) foreach { case (k, v) => println("%s: %s".format(k, v)) }
  }

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
}

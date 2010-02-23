/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest
package nest

import java.io.{File, FilenameFilter, IOException, StringWriter,
                FileInputStream, FileOutputStream, BufferedReader,
                FileReader, PrintWriter, FileWriter}
import java.net.URI
import scala.tools.nsc.io.{ Path, Directory }

trait FileManager {
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
    val args = Array(f1.getCanonicalPath(), f2.getCanonicalPath())

    DiffPrint.doDiff(args, diffWriter)
    val res = diffWriter.toString
    if (res startsWith "No") "" else res
  }

  var JAVACMD: String
  var JAVAC_CMD: String

  var CLASSPATH: String
  var LATEST_LIB: String
  var LIB_DIR: String = ""

  val TESTROOT: String

  var showDiff = false
  var showLog = false
  var failed = false

  var SCALAC_OPTS = PartestDefaults.scalacOpts
  var JAVA_OPTS   = PartestDefaults.javaOpts
  var timeout     = PartestDefaults.timeout

  def getLogFile(dir: File, fileBase: String, kind: String): LogFile =
    new LogFile(dir, fileBase + "-" + kind + ".log")

  def getLogFile(file: File, kind: String): LogFile = {
    val dir = file.getParentFile
    val fileBase = basename(file.getName)
    getLogFile(dir, fileBase, kind)
  }

  def logFileExists(file: File, kind: String) =
    getLogFile(file, kind).canRead

  def overwriteFileWith(dest: File, file: File) =
    dest.isFile && copyFile(file, dest)

  def copyFile(from: File, to: File): Boolean =
    try {
      val appender = StreamAppender(from, to)
      appender.run()
      appender.closeAll()
      true
    }
    catch {
      case _: IOException => false
    }

  def mapFile(file: File, suffix: String, dir: File, replace: String => String) {
    val tmpFile = File.createTempFile("tmp", suffix, dir) // prefix required by API

    val appender = StreamAppender(file, tmpFile)
    appender.runAndMap(replace)
    appender.closeAll()

    val appender2 = StreamAppender(tmpFile, file)
    appender2.run()
    appender2.closeAll()

    tmpFile.delete()
  }
}

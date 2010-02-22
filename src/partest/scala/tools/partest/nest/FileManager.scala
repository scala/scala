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

  def basename(name: String): String = Path(name).stripExtension

  /**
   * Compares two files using a Java implementation of the GNU diff
   * available at http://www.bmsi.com/java/#diff.
   *
   * @param  f1  the first file to be compared
   * @param  f2  the second file to be compared
   * @return the text difference between the compared files
   */
  def compareFiles(f1: File, f2: File): String = {
    var res = ""
    try {
      val diffWriter = new StringWriter
      val args = Array(f1.getCanonicalPath(), f2.getCanonicalPath())
      DiffPrint.doDiff(args, diffWriter)
      res = diffWriter.toString
      if (res startsWith "No")
        res = ""
    } catch {
      case e: IOException =>
        e.printStackTrace()
    }
    res
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

  var SCALAC_OPTS = System.getProperty("scalatest.scalac_opts", "-deprecation")
  var JAVA_OPTS   = System.getProperty("scalatest.java_opts", "")

  var timeout = "1200000"

  def getLogFile(dir: File, fileBase: String, kind: String): LogFile =
    new LogFile(dir, fileBase + "-" + kind + ".log")

  def getLogFile(file: File, kind: String): LogFile = {
    val dir = file.getParentFile
    val fileBase = basename(file.getName)
    getLogFile(dir, fileBase, kind)
  }

  def logFileExists(file: File, kind: String): Boolean = {
    val logFile = getLogFile(file, kind)
    logFile.exists && logFile.canRead
  }

  def overwriteFileWith(dest: File, file: File): Boolean =
    if (dest.exists && dest.isFile)
        copyFile(file, dest)
    else
      false

  def copyFile(from: File, to: File): Boolean =
    try {
      val fromReader = new BufferedReader(new FileReader(from))
      val toWriter = new PrintWriter(new FileWriter(to))

      new StreamAppender(fromReader, toWriter).run()

      fromReader.close()
      toWriter.close()
      true
    } catch {
      case _:IOException => false
    }

  def mapFile(file: File, suffix: String, dir: File, replace: String => String) {
    val tmpFile = File.createTempFile("tmp", suffix, dir) // prefix required by API
    val fileReader = new BufferedReader(new FileReader(file))
    val tmpFilePrinter = new PrintWriter(new FileWriter(tmpFile))
    val appender = new StreamAppender(fileReader, tmpFilePrinter)

    appender.runAndMap(replace)

    fileReader.close()
    tmpFilePrinter.close()

    val tmpFileReader = new BufferedReader(new FileReader(tmpFile))
    val filePrinter= new PrintWriter(new FileWriter(file), true)
    (new StreamAppender(tmpFileReader, filePrinter)).run
    tmpFileReader.close()
    filePrinter.close()
    tmpFile.delete()
  }
}

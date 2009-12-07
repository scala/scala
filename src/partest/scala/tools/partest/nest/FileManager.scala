/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest
package nest

import java.io.{File, FilenameFilter, IOException, StringWriter}
import java.net.URI
import scala.tools.nsc.io.Directory

trait FileManager {

  def basename(name: String): String = {
    val inx = name.lastIndexOf(".")
    if (inx < 0) name else name.substring(0, inx)
  }

  def deleteRecursive(dir: File) { Directory(dir).deleteRecursively() }

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
}

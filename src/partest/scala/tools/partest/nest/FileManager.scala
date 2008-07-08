/* NEST (New Scala Test)
 * Copyright 2007-2008 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest.nest

import java.io.{File, FilenameFilter, IOException, StringWriter}
import java.net.URI

trait FileManager {

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
      if (res.startsWith("No"))
        res = ""
    } catch {
      case e: IOException =>
        e.printStackTrace()
    }
    res
  }


  var JAVACMD: String

  var CLASSPATH: String
  var LATEST_LIB: String

  var showDiff = false
  var showLog = false
  var failed = false

  var SCALAC_OPTS = System.getProperty("scalatest.scalac_opts", "-deprecation")

  var timeout = "600000"
}

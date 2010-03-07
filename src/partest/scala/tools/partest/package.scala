/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 */

package scala.tools

import java.io.{ File => JFile }
import nsc.io.{ Path, Process, Directory }
import util.{ PathResolver }
import nsc.Properties.{ propOrElse, propOrNone, propOrEmpty }

package object partest {
  import nest.NestUI

  implicit private[partest] def temporaryPath2File(x: Path): JFile = x.jfile
  implicit private[partest] def temporaryFile2Path(x: JFile): Path = Path(x)

  def basename(name: String): String = Path(name).stripExtension
  def resultsToStatistics(results: Iterable[(_, Int)]): (Int, Int) = {
    val (files, failures) = results map (_._2 == 0) partition (_ == true)
    (files.size, failures.size)
  }

  def vmArgString = {
    val str = Process.javaVmArguments mkString " "
    "Java VM started with arguments: '%s'" format str
  }

  def allPropertiesString = {
    import collection.JavaConversions._
    System.getProperties.toList.sorted map { case (k, v) => "%s -> %s\n".format(k, v) } mkString
  }

  def showAllJVMInfo {
    NestUI.verbose(vmArgString)
    NestUI.verbose(allPropertiesString)
  }

  def isPartestDebug = propOrEmpty("partest.debug") == "true"
}
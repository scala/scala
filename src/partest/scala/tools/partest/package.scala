/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 */

package scala.tools

package object partest {
  import nest.NestUI

  def vmArgString = {
    import scala.tools.nsc.io.Process

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

  def isPartestDebug = {
    (System.getProperty("partest.debug") == "true") ||
    (System.getProperty("scalatest.debug") == "true")
  }
}
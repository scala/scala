/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 */

package scala.tools

package object partest {
  import nest.NestUI

  def showVMArgs {
    import scala.tools.nsc.io.Process

    val str = Process.javaVmArguments mkString " "
    NestUI.verbose("Java VM started with arguments: '%s'" format str)
  }

  def showAllProperties {
    import collection.JavaConversions._
    for ((k, v) <- System.getProperties.toList.sorted) {
      NestUI.verbose("%s -> %s".format(k, v))
    }
  }

  def showAllJVMInfo {
    showVMArgs
    showAllProperties
  }

  def isPartestDebug = {
    (System.getProperty("partest.debug") == "true") ||
    (System.getProperty("scalatest.debug") == "true")
  }
}
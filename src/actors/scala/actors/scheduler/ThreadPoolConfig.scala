/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors
package scheduler

/**
 * @author Erik Engbrecht
 */
object ThreadPoolConfig {
  private val rt = Runtime.getRuntime()
  private val minNumThreads = 4

  private def getIntegerProp(propName: String): Option[Int] = {
    try {
      val prop = System.getProperty(propName)
      Some(Integer.parseInt(prop))
    } catch {
      case ace: java.security.AccessControlException => None
      case nfe: NumberFormatException => None
    }
  }

  val corePoolSize = getIntegerProp("actors.corePoolSize") match {
    case Some(i) if i > 0 => i
    case _ => {
      val byCores = rt.availableProcessors() * 2
      if (byCores > minNumThreads) byCores else minNumThreads
    }
  }

  val maxPoolSize = getIntegerProp("actors.maxPoolSize") match {
    case Some(i) if (i >= corePoolSize) => i
    case Some(i) if (i < corePoolSize) => corePoolSize
    case _ => 256
  }

  private[actors] def useForkJoin: Boolean =
    try {
      val fjProp = System.getProperty("actors.enableForkJoin")
      if (fjProp != null)
        fjProp.equals("true")
      else {
        val javaVersion = System.getProperty("java.version")
        val jvmVendor =   System.getProperty("java.vm.vendor")
        Debug.info(this+": java.version = "+javaVersion)
        Debug.info(this+": java.vm.vendor = "+jvmVendor)
        (javaVersion.indexOf("1.6") != -1 ||
         javaVersion.indexOf("1.7") != -1) &&
        // on IBM J9 1.6 do not use ForkJoinPool
        (jvmVendor.indexOf("Sun") != -1)
      }
    } catch {
      case se: SecurityException => false
    }

}

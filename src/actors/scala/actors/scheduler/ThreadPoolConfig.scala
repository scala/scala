/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.actors
package scheduler

import scala.util.Properties.{ javaVersion, javaVmVendor, isJavaAtLeast, propIsSetTo, propOrNone }

/**
 * @author Erik Engbrecht
 * @author Philipp Haller
 */
private[actors] object ThreadPoolConfig {
  private val rt = Runtime.getRuntime()
  private val minNumThreads = 4

  private def getIntegerProp(propName: String): Option[Int] =
    try propOrNone(propName) map (_.toInt)
    catch { case _: SecurityException | _: NumberFormatException => None }

  val corePoolSize = getIntegerProp("actors.corePoolSize") match {
    case Some(i) if i > 0 => i
    case _ => {
      val byCores = rt.availableProcessors() * 2
      if (byCores > minNumThreads) byCores else minNumThreads
    }
  }

  val maxPoolSize = {
    val preMaxSize = getIntegerProp("actors.maxPoolSize") getOrElse 256
    if (preMaxSize >= corePoolSize) preMaxSize else corePoolSize
  }

  private[actors] def useForkJoin: Boolean =
    try !propIsSetTo("actors.enableForkJoin", "false") &&
      (propIsSetTo("actors.enableForkJoin", "true") || {
        Debug.info(this+": java.version = "+javaVersion)
        Debug.info(this+": java.vm.vendor = "+javaVmVendor)
        isJavaAtLeast("1.6")
      })
    catch {
      case _: SecurityException => false
    }
}

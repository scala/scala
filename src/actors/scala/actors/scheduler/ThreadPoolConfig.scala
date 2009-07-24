/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
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
}

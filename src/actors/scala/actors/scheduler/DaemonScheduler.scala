/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.actors
package scheduler

/**
 * Default scheduler for actors with daemon semantics, such as those backing futures.
 *
 * @author Erik Engbrecht
 */
object DaemonScheduler extends DelegatingScheduler {

  def makeNewScheduler(): IScheduler = {
    // test on which JVM we are running
    val jvmVendor = System.getProperty("java.vm.vendor")
    val sched = if (jvmVendor.indexOf("IBM") != -1) {
      Debug.info(this+": running on a "+jvmVendor+" JVM")
      // on IBM J9 1.6 do not use ForkJoinPool
      val s = new ResizableThreadPoolScheduler(true)
      s.start()
      s
    } else {
      val s = new ForkJoinScheduler(true)
      s.start()
      s
    }
    Debug.info(this+": starting new "+sched+" ["+sched.getClass+"]")
    sched
  }

}

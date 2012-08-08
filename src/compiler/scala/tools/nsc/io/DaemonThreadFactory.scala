/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package io

import java.util.concurrent._

class DaemonThreadFactory extends ThreadFactory {
  def newThread(r: Runnable): Thread = {
    val thread = new Thread(r)
    thread setDaemon true
    thread
  }
}

object DaemonThreadFactory {
  def newPool() = Executors.newCachedThreadPool(new DaemonThreadFactory)
}
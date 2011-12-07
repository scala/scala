/**
 * Copyright (C) 2009-2011 Scalable Solutions AB <http://scalablesolutions.se>
 */

package akka.util

trait Bootable {
  def onLoad() {}
  def onUnload() {}
}

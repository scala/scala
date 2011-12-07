/**
 * Copyright (C) 2009-2011 Scalable Solutions AB <http://scalablesolutions.se>
 */

package akka.util

import java.util.concurrent.ConcurrentSkipListSet
import akka.actor.{ ActorInitializationException, ActorRef }

/**
 * A manager for listener actors. Intended for mixin by observables.
 *
 * @author Martin Krasser
 */
trait ListenerManagement {

  private val listeners = new ConcurrentSkipListSet[ActorRef]

  /**
   * Specifies whether listeners should be started when added and stopped when removed or not
   */
  protected def manageLifeCycleOfListeners: Boolean = true

  /**
   * Adds the <code>listener</code> this registry's listener list.
   * The <code>listener</code> is started by this method if manageLifeCycleOfListeners yields true.
   */
  def addListener(listener: ActorRef) {
    if (manageLifeCycleOfListeners) listener.start()
    listeners add listener
  }

  /**
   * Removes the <code>listener</code> this registry's listener list.
   * The <code>listener</code> is stopped by this method if manageLifeCycleOfListeners yields true.
   */
  def removeListener(listener: ActorRef) {
    listeners remove listener
    if (manageLifeCycleOfListeners) listener.stop()
  }

  /*
   * Returns whether there are any listeners currently
   */
  def hasListeners: Boolean = !listeners.isEmpty

  /**
   * Checks if a specific listener is registered. ActorInitializationException leads to removal of listener if that
   * one isShutdown.
   */
  def hasListener(listener: ActorRef): Boolean = listeners.contains(listener)

  protected[akka] def notifyListeners(message: => Any) {
    if (hasListeners) {
      val msg = message
      val iterator = listeners.iterator
      while (iterator.hasNext) {
        val listener = iterator.next
        // Uncomment if those exceptions are so frequent as to bottleneck
        // if (listener.isShutdown) iterator.remove() else
        try {
          listener ! msg
        } catch {
          case e: ActorInitializationException =>
            if (listener.isShutdown) iterator.remove()
        }
      }
    }
  }

  /**
   * Execute <code>f</code> with each listener as argument. ActorInitializationException is not handled.
   */
  protected[akka] def foreachListener(f: (ActorRef) => Unit) {
    val iterator = listeners.iterator
    while (iterator.hasNext) {
      val listener = iterator.next
      if (listener.isRunning) f(listener)
    }
  }
}

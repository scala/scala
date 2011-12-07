/**
 * Copyright (C) 2009-2011 Scalable Solutions AB <http://scalablesolutions.se>
 */

package akka.util

import java.util.concurrent.locks.{ ReentrantReadWriteLock, ReentrantLock }
import java.util.concurrent.atomic.{ AtomicBoolean }
import akka.event.EventHandler

/**
 * @author <a href="http://jonasboner.com">Jonas Bon&#233;r</a>
 */
final class ReentrantGuard {
  val lock = new ReentrantLock

  final def withGuard[T](body: => T): T = {
    lock.lock
    try {
      body
    } finally {
      lock.unlock
    }
  }

  final def tryWithGuard[T](body: => T): T = {
    while (!lock.tryLock) { Thread.sleep(10) } // wait on the monitor to be unlocked
    try {
      body
    } finally {
      lock.unlock
    }
  }
}

/**
 * @author <a href="http://jonasboner.com">Jonas Bon&#233;r</a>
 */
class ReadWriteGuard {
  private val rwl = new ReentrantReadWriteLock
  val readLock = rwl.readLock
  val writeLock = rwl.writeLock

  def withWriteGuard[T](body: => T): T = {
    writeLock.lock
    try {
      body
    } finally {
      writeLock.unlock
    }
  }

  def withReadGuard[T](body: => T): T = {
    readLock.lock
    try {
      body
    } finally {
      readLock.unlock
    }
  }
}

/**
 * A very simple lock that uses CCAS (Compare Compare-And-Swap)
 * Does not keep track of the owner and isn't Reentrant, so don't nest and try to stick to the if*-methods
 */
class SimpleLock {
  val acquired = new AtomicBoolean(false)

  def ifPossible(perform: () => Unit): Boolean = {
    if (tryLock()) {
      try {
        perform
      } finally {
        unlock()
      }
      true
    } else false
  }

  def ifPossibleYield[T](perform: () => T): Option[T] = {
    if (tryLock()) {
      try {
        Some(perform())
      } finally {
        unlock()
      }
    } else None
  }

  def ifPossibleApply[T, R](value: T)(function: (T) => R): Option[R] = {
    if (tryLock()) {
      try {
        Some(function(value))
      } finally {
        unlock()
      }
    } else None
  }

  def tryLock() = {
    if (acquired.get) false
    else acquired.compareAndSet(false, true)
  }

  def tryUnlock() = {
    acquired.compareAndSet(true, false)
  }

  def locked = acquired.get

  def unlock() {
    acquired.set(false)
  }
}

/**
 * An atomic switch that can be either on or off
 */
class Switch(startAsOn: Boolean = false) {
  private val switch = new AtomicBoolean(startAsOn)

  protected def transcend(from: Boolean, action: => Unit): Boolean = synchronized {
    if (switch.compareAndSet(from, !from)) {
      try {
        action
      } catch {
        case e: Throwable =>
          EventHandler.error(e, this, e.getMessage)
          switch.compareAndSet(!from, from) // revert status
          throw e
      }
      true
    } else false
  }

  def switchOff(action: => Unit): Boolean = transcend(from = true, action)
  def switchOn(action: => Unit): Boolean = transcend(from = false, action)

  def switchOff: Boolean = synchronized { switch.compareAndSet(true, false) }
  def switchOn: Boolean = synchronized { switch.compareAndSet(false, true) }

  def ifOnYield[T](action: => T): Option[T] = {
    if (switch.get) Some(action)
    else None
  }

  def ifOffYield[T](action: => T): Option[T] = {
    if (!switch.get) Some(action)
    else None
  }

  def ifOn(action: => Unit): Boolean = {
    if (switch.get) {
      action
      true
    } else false
  }

  def ifOff(action: => Unit): Boolean = {
    if (!switch.get) {
      action
      true
    } else false
  }

  def whileOnYield[T](action: => T): Option[T] = synchronized {
    if (switch.get) Some(action)
    else None
  }

  def whileOffYield[T](action: => T): Option[T] = synchronized {
    if (!switch.get) Some(action)
    else None
  }

  def whileOn(action: => Unit): Boolean = synchronized {
    if (switch.get) {
      action
      true
    } else false
  }

  def whileOff(action: => Unit): Boolean = synchronized {
    if (switch.get) {
      action
      true
    } else false
  }

  def ifElseYield[T](on: => T)(off: => T) = synchronized {
    if (switch.get) on else off
  }

  def isOn = switch.get
  def isOff = !isOn
}

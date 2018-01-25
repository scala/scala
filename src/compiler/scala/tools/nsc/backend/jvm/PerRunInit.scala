package scala.tools.nsc.backend.jvm

import scala.collection.mutable.ListBuffer

/**
 * Utility for backend components that have state that needs to be re-initialized at every compiler
 * run, for example state that depends on compiler settings of frontend types (Symbols, Types).
 *
 * The trait provides an `initialize` method that runs all initializers added through `perRunLazy`.
 */
trait PerRunInit {
  // We have to synchronize on inits, as many of the initializers are themselves lazy,
  // so the back end may initialise them in parallel, and ListBuffer is not threadsafe
  private val inits = ListBuffer.empty[() => Unit]

  def perRunInit(init: => Unit): Unit = inits.synchronized (inits += (() => init))

  def initialize(): Unit = inits.synchronized(inits.foreach(_.apply()))
}


package scala.tools.nsc.backend.jvm

import scala.collection.mutable.ListBuffer

/**
 * Utility for backend components that have state that needs to be re-initialized at every compiler
 * run, for example state that depends on compiler settings of frontend types (Symbols, Types).
 *
 * The trait provides an `initialize` method that runs all initializers added through `perRunLazy`.
 */
trait PerRunInit {
  private val inits = ListBuffer.empty[() => Unit]

  def perRunInit(init: => Unit): Unit = inits += (() => init)

  def initialize(): Unit = inits.foreach(_.apply())
}


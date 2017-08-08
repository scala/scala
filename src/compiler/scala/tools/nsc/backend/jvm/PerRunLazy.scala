package scala.tools.nsc.backend.jvm

import scala.collection.mutable.ListBuffer

/**
 * Utility for backend components that have state that needs to be re-initialized at every compiler
 * run, for example state that depends on compiler settings of frontend types (Symbols, Types).
 *
 * The state is computed lazily to work around / not worry about initialization ordering issues.
 *
 * The trait provides an `initialize` method that forces re-initialization of all state that was
 * created through `perRunLazy`.
 */
trait PerRunLazy {
  private val ls = ListBuffer.empty[LazyVar[_]]

  def perRunLazy[T](init: => T): LazyVar[T] = {
    val r = new LazyVar(() => init)
    ls += r
    r
  }

  def initialize(): Unit = ls.foreach(_.reInit())
}

/**
 * This implements a lazy value that can be reset and re-initialized.
 */
class LazyVar[T](init: () => T) {
  @volatile private[this] var isInit: Boolean = false
  private[this] var v: T = _

  def get = {
    if (isInit) v
    else synchronized {
      if (!isInit) v = init()
      isInit = true
      v
    }
  }

  def reInit(): Unit = synchronized(isInit = false)
}

object LazyVar {
  import language.implicitConversions
  implicit def lGet[T](l: LazyVar[T]): T = l.get
}
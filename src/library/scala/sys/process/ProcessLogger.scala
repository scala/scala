/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.sys
package process

trait ProcessLogger {
  def info(s: => String): Unit
  def error(s: => String): Unit
  def buffer[T](f: => T): T
}

object ProcessLogger {
  def apply(fn: String => Unit): ProcessLogger = new ProcessLogger {
    def info(s: => String): Unit = fn(s)
    def error(s: => String): Unit = fn(s)
    def buffer[T](f: => T): T = f
  }
}

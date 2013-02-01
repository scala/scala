/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala

/** Throwing this exception can be a temporary replacement for a method
 *  body that remains to be implemented. For instance, the exception is thrown by
 *  `Predef.???`.
 */
final class NotImplementedError(msg: String) extends Error(msg) {
  def this() = this("an implementation is missing")
}

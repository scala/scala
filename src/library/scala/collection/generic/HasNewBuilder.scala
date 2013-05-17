/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala
package collection
package generic

import mutable.Builder

trait HasNewBuilder[+A, +Repr] extends Any {
  /** The builder that builds instances of Repr */
  protected[this] def newBuilder: Builder[A, Repr]
}

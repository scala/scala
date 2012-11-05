/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

import Specializable._

/** Annotate type parameters on which code should be automatically
 *  specialized. For example:
 *  {{{
 *    class MyList[@specialized T] ...
 *  }}}
 *
 *  Type T can be specialized on a subset of the primitive types by
 *  specifying a list of primitive types to specialize at:
 *  {{{
 *    class MyList[@specialized(Int, Double, Boolean) T] ..
 *  }}}
 *
 *  @since 2.8
 */
// class tspecialized[T](group: Group[T]) extends scala.annotation.StaticAnnotation {

class specialized(group: SpecializedGroup) extends scala.annotation.StaticAnnotation {
  def this(types: Specializable*) = this(new Group(types.toList))
  def this() = this(Primitives)
}

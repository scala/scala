/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
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
class specialized(group: SpecializedGroup) extends annotation.StaticAnnotation {
  def this(types: Specializable*) = this(new Group(types.toList))
  def this() = this(Primitives)
}

/** This annotation is used internally by the compiler to denote classes that
 *  were compiled without the -Xanyref-specialization flag. It is an anti-
 *  annotation marking that the type parameter should not be specialized on
 *  AnyRef, iregardless of what the @specialized annotation says.
 */
private class specializedExcludeAnyRef extends annotation.StaticAnnotation

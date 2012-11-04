/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.annotation

/**
 * An annotation that designates the class to which it is applied as cloneable
 */
@deprecated("instead of `@cloneable class C`, use `class C extends Cloneable`", "2.10.0")
class cloneable extends scala.annotation.StaticAnnotation

/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

/** `AnyVal` is the root class of all ''value types'', which describe values
 *  not implemented as objects in the underlying host system.  The value classes
 *  are specified in SLS 12.2.
 *
 *  The standard implementation includes nine `AnyVal` subtypes:
 *
 *  [[scala.Double]], [[scala.Float]], [[scala.Long]], [[scala.Int]], [[scala.Char]],
 *  [[scala.Short]], and [[scala.Byte]] are the ''numeric value types''.
 *
 *  [[scala.Unit]] and [[scala.Boolean]] are the ''non-numeric value types''.
 *
 *  Other groupings:
 *
 *  The ''subrange types'' are [[scala.Byte]], [[scala.Short]], and [[scala.Char]].
 *  The ''integer types'' include the subrange types as well as [[scala.Int]] and [[scala.Long]].
 *  The ''floating point types'' are [[scala.Float]] and [[scala.Double]].
 */
sealed trait AnyVal

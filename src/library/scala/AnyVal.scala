/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

/** `AnyVal` is the root class of all ''value types'', which describe values
 *  not implemented as objects in the underlying host system. Value classes
 *  are specified in Scala Language Specification, section 12.2.
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
 *   - The ''subrange types'' are [[scala.Byte]], [[scala.Short]], and [[scala.Char]].
 *   - The ''integer types'' include the subrange types as well as [[scala.Int]] and [[scala.Long]].
 *   - The ''floating point types'' are [[scala.Float]] and [[scala.Double]].
 *
 * Prior to Scala 2.10, `AnyVal` was a sealed trait. Beginning with Scala 2.10,
 * however, it is possible to define a subclass of `AnyVal` called a ''user-defined value class''
 * which is treated specially by the compiler. Properly-defined user value classes provide a way
 * to improve performance on user-defined types by avoiding object allocation at runtime, and by
 * replacing virtual method invocations with static method invocations.
 *
 * User-defined value classes which avoid object allocation...
 *
 *   - must have a single `val` parameter that is the underlying runtime representation.
 *   - can define `def`s, but no `val`s, `var`s, or nested `traits`s, `class`es or `object`s.
 *   - typically extend no other trait apart from `AnyVal`.
 *   - cannot be used in type tests or pattern matching.
 *   - may not override `equals` or `hashCode` methods.
 *
 * A minimal example:
 * {{{
 *     class Wrapper(val underlying: Int) extends AnyVal {
 *       def foo: Wrapper = new Wrapper(underlying * 19)
 *     }
 * }}}
 *
 * It's important to note that user-defined value classes are limited, and in some circumstances,
 * still must allocate a value class instance at runtime. These limitations and circumstances are
 * explained in greater detail in the [[http://docs.scala-lang.org/overviews/core/value-classes.html Value Classes and Universal Traits]].
 */
abstract class AnyVal extends Any {
  def getClass(): Class[_ <: AnyVal] = null
}

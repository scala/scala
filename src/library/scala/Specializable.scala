/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

/** A common supertype for companions of specializable types.
 *  Should not be extended in user code.
 */
trait Specializable

object Specializable {
  // No type parameter in @specialized annotation.
  trait SpecializedGroup { }

  // Smuggle a list of types by way of a tuple upon which Group is parameterized.
  class Group[T >: Null](value: T) extends SpecializedGroup { }

  final val Primitives  = new Group((Byte, Short, Int, Long, Char, Float, Double, Boolean, Unit))
  final val Everything  = new Group((Byte, Short, Int, Long, Char, Float, Double, Boolean, Unit, AnyRef))
  final val Bits32AndUp = new Group((Int, Long, Float, Double))
  final val Integral    = new Group((Byte, Short, Int, Long, Char))
  final val AllNumeric  = new Group((Byte, Short, Int, Long, Char, Float, Double))
  final val BestOfBreed = new Group((Int, Double, Boolean, Unit, AnyRef))
}

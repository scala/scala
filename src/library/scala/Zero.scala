/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

import reflect.ClassManifest
import collection._
import collection.immutable.{List, Stream, Nil}
import xml.NodeSeq

/** A trait representing Zero of a given type.  It should be understood
 *  that "Zero" can be context-dependant, for instance with respect to
 *  multiplication, Zero[Int] is 1.  At present this class does not attempt
 *  to address more interesting uses, and provides the standard value of
 *  0 for all the AnyVal types, and empty containers for many container types.
 *
 * @author  Paul Phillips (standing on the shoulders of giants)
 * @since   2.8
 */

trait Zero[+Z] {
  val zero: Z
}

trait LowPriorityZeroImplicits {
  import Zero.zero

  // If we just provide a Zero for collection.Set, then immutable.Set and
  // mutable.Set have no Zero, but if we try to provide both of those, then
  // Zero[collection.Set] fails as ambiguous.  Fortunately the prioritization
  // mechanism let us deal with this with a subclass, but this isn't a
  // terribly general solution...
  implicit def MutableSeqZero[A]        = zero[mutable.Seq[A]](mutable.Seq.empty)
  implicit def MutableSetZero[A]        = zero[mutable.Set[A]](mutable.Set.empty)
  implicit def MutableMapZero[A, B]     = zero[mutable.Map[A, B]](mutable.Map.empty)
}

object Zero extends LowPriorityZeroImplicits {
  def apply[Z](implicit z: Zero[Z])   = z.zero
  def zero[Z](z: Z): Zero[Z]          = new Zero[Z] { val zero = z }

  implicit object UnitZero extends Zero[Unit]             { val zero = () }
  implicit object StringZero extends Zero[String]         { val zero = "" }
  implicit object BooleanZero extends Zero[Boolean]       { val zero = false }
  implicit object ByteZero extends Zero[Byte]             { val zero = (0: Byte) }
  implicit object ShortZero extends Zero[Short]           { val zero = (0: Short) }
  implicit object IntZero extends Zero[Int]               { val zero = 0 }
  implicit object LongZero extends Zero[Long]             { val zero = 0l }
  implicit object CharZero extends Zero[Char]             { val zero = (0: Char) }
  implicit object FloatZero extends Zero[Float]           { val zero = 0f }
  implicit object DoubleZero extends Zero[Double]         { val zero = 0d }
  implicit object BigIntZero extends Zero[BigInt]         { val zero = BigInt(0) }
  implicit object BigDecimalZero extends Zero[BigDecimal] { val zero = BigDecimal(0) }
  implicit object NodeSeqZero extends Zero[NodeSeq]       { val zero = NodeSeq.Empty }

  implicit def OptionZero[A] = zero[Option[A]](None)
  implicit def ArrayZero[A: ClassManifest] = zero(Array.empty[A])

  implicit def TraversableZero[A] = zero[Traversable[A]](Traversable.empty)
  implicit def IterableZero[A] = zero[Iterable[A]](Iterable.empty)

  implicit def ImmutableSeqZero[A]      = zero[immutable.Seq[A]](immutable.Seq.empty)
  implicit def ImmutableSetZero[A]      = zero[immutable.Set[A]](immutable.Set.empty)
  implicit def ImmutableMapZero[A, B]   = zero[immutable.Map[A, B]](immutable.Map.empty)

  implicit def IteratorZero[A] = zero[Iterator[A]](Iterator.empty)
  implicit def IndexedSeqZero[A] = zero[IndexedSeq[A]](IndexedSeq.empty)
  implicit def ListZero[A] = zero[List[A]](Nil)
  implicit def StreamZero[A] = zero[Stream[A]](Stream.empty)

}

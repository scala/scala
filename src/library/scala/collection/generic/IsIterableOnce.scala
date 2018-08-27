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

/** Type class witnessing that a collection representation type `Repr` has
 *  elements of type `A` and has a conversion to `IterableOnce[A]`.
 *
 *  This type enables simple enrichment of `IterableOnce`s with extension
 *  methods which can make full use of the mechanics of the Scala collections
 *  framework in their implementation.
 *
 *  Example usage,
 * {{{
 *    class FilterMapImpl[Repr, I <: IsIterableOnce[Repr]](coll: Repr, it: I) {
 *      final def filterMap[B, That](f: it.A => Option[B])(implicit bf: BuildFrom[Repr, B, That]): That = {
 *        val b = bf.newBuilder(coll)
 *        for(e <- it(coll).iterator) f(e) foreach (b +=)
 *        b.result()
 *      }
 *    }
 *    implicit def filterMap[Repr](coll: Repr)(implicit it: IsIterableOnce[Repr]): FilterMapImpl[Repr, it.type] =
 *      new FilterMapImpl(coll, it)
 *
 *    List(1, 2, 3, 4, 5) filterMap (i => if(i % 2 == 0) Some(i) else None)
 *    // == List(2, 4)
 * }}}
 *
 * @author Miles Sabin
 * @author J. Suereth
 * @since 2.10
 */
trait IsIterableOnce[Repr] {

  /** The type of elements we can traverse over (e.g. `Int`). */
  type A

  @deprecated("'conversion' is now a method named 'apply'", "2.13.0")
  val conversion: Repr => IterableOnce[A] = apply(_)

  /** A conversion from the representation type `Repr` to a `IterableOnce[A]`. */
  def apply(coll: Repr): IterableOnce[A]

}

object IsIterableOnce extends IsIterableOnceLowPriority {
  import scala.language.higherKinds

  // Straightforward case: IterableOnce subclasses
  implicit def iterableOnceIsIterableOnce[CC0[A] <: IterableOnce[A], A0]: IsIterableOnce[CC0[A0]] { type A = A0 } =
    new IsIterableOnce[CC0[A0]] {
      type A = A0
      def apply(coll: CC0[A0]): IterableOnce[A0] = coll
    }

}

trait IsIterableOnceLowPriority {

  // Makes `IsIterable` instance visible in `IsIterableOnce` companion
  implicit def isIterableLikeIsIterableOnce[Repr](implicit
    isIterableLike: IsIterable[Repr]
  ): IsIterableOnce[Repr] { type A = isIterableLike.A } = isIterableLike

}
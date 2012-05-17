/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2012, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package generic

/** Type class witnessing that a collection representation type `Repr` has
 *  elements of type `A` and has a conversion to `GenTraversableLike[A, Repr]`.
 *
 *  This type enables simple enrichment of `GenTraversable`s with extension
 *  methods which can make full use of the mechanics of the Scala collections
 *  framework in their implementation.
 *
 *  Example usage,
 * {{{
 *    import scala.collection.generic.{ CanBuildFrom, FromRepr, HasElem }
 *
 *    class FilterMapImpl[A, Repr](val r : Repr)(implicit hasElem : HasElem[Repr, A]) {
 *      def filterMap[B, That](f : A => Option[B])
 *        (implicit cbf : CanBuildFrom[Repr, B, That]) : That = r.flatMap(f(_).toSeq)
 *    }
 *
 *    implicit def filterMap[Repr : FromRepr](r : Repr) = new FilterMapImpl(r)
 *
 *    val l = List(1, 2, 3, 4, 5)
 *    List(1, 2, 3, 4, 5) filterMap (i => if(i % 2 == 0) Some(i) else None)
 *    // == List(2, 4)
 * }}}
 *
 * @author Miles Sabin
 * @since 2.10
 */
trait FromRepr[Repr] {
  type A
  val hasElem: HasElem[Repr, A]
}

object FromRepr {
  import language.higherKinds

  implicit val stringFromRepr : FromRepr[String] { type A = Char } = new FromRepr[String] {
    type A = Char
    val hasElem = implicitly[HasElem[String, Char]]
  }

  implicit def genTraversableLikeFromRepr[C[_], A0]
    (implicit hasElem0: HasElem[C[A0], A0]) : FromRepr[C[A0]] { type A = A0 } = new FromRepr[C[A0]] {
      type A = A0
      val hasElem = hasElem0
    }
}

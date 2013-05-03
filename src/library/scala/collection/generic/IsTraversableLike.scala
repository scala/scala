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

/** A trait which can be used to avoid code duplication when defining extension
 *  methods that should be applicable both to existing Scala collections (i.e.,
 *  types extending `GenTraversableLike`) as well as other (potentially user-defined)
 *  types that could be converted to a Scala collection type. This trait
 *  makes it possible to treat Scala collections and types that can be implicitly
 *  converted to a collection type uniformly. For example, one can provide
 *  extension methods that work both on collection types and on `String`s (`String`s
 *  do not extend `GenTraversableLike`, but can be converted to `GenTraversableLike`)
 *
 * `IsTraversable` provides two members:
 *
 *  1. type member `A`, which represents the element type of the target `GenTraversableLike[A, Repr]`
 *  1. value member `conversion`, which provides a way to convert between the type we wish to add extension methods to, `Repr`, and `GenTraversableLike[A, Repr]`.
 *
 * ===Usage===
 *
 * One must provide `IsTraversableLike` as an implicit parameter type of an implicit
 * conversion. Its usage is shown below. Our objective in the following example
 * is to provide a generic extension method `mapReduce` to any type that extends
 * or can be converted to `GenTraversableLike`. In our example, this includes
 * `String`.
 *
 * {{{
 *    import scala.collection.GenTraversableLike
 *    import scala.collection.generic.IsTraversableLike
 *
 *    class ExtensionMethods[A, Repr](coll: GenTraversableLike[A, Repr]) {
 *      def mapReduce[B](mapper: A => B)(reducer: (B, B) => B): B = {
 *        val iter = coll.toIterator
 *        var res = mapper(iter.next())
 *        while (iter.hasNext)
 *          res = reducer(res, mapper(iter.next()))
 *        res
 *      }
 *    }
 *
 *    implicit def withExtensions[Repr](coll: Repr)(implicit traversable: IsTraversableLike[Repr]) =
 *      new ExtensionMethods(traversable.conversion(coll))
 *
 *  // See it in action!
 *  List(1, 2, 3).mapReduce(_ * 2)(_ + _) // res0: Int = 12
 *  "Yeah, well, you know, that's just, like, your opinion, man.".mapReduce(x => 1)(_ + _) // res1: Int = 59
 *}}}
 *
 * Here, we begin by creating a class `ExtensionMethods` which contains our
 * `mapReduce` extension method. Note that `ExtensionMethods` takes a constructor
 * argument `coll` of type `GenTraversableLike[A, Repr]`, where `A` represents the
 * element type and `Repr` represents (typically) the collection type. The
 * implementation of `mapReduce` itself is straightforward.
 *
 * The interesting bit is the implicit conversion `withExtensions`, which
 * returns an instance of `ExtensionMethods`. This implicit conversion can
 * only be applied if there is an implicit value `traversable` of type
 * `IsTraversableLike[Repr]` in scope. Since `IsTraversableLike` provides
 * value member `conversion`, which gives us a way to convert between whatever
 * type we wish to add an extension method to (in this case, `Repr`) and
 * `GenTraversableLike[A, Repr]`, we can now convert `coll` from type `Repr`
 * to `GenTraversableLike[A, Repr]`. This allows us to create an instance of
 * the `ExtensionMethods` class, which we pass our new
 * `GenTraversableLike[A, Repr]` to.
 *
 * When the `mapReduce` method is called on some type of which it is not
 * a member, implicit search is triggered. Because implicit conversion
 * `withExtensions` is generic, it will be applied as long as an implicit
 * value of type `IsTraversableLike[Repr]` can be found. Given that
 * `IsTraversableLike` contains implicit members that return values of type
 * `IsTraversableLike`, this requirement is typically satisfied, and the chain
 * of interactions described in the previous paragraph is set into action.
 * (See the `IsTraversableLike` companion object, which contains a precise
 * specification of the available implicits.)
 *
 * ''Note'': Currently, it's not possible to combine the implicit conversion and
 * the class with the extension methods into an implicit class due to
 * limitations of type inference.
 *
 * ===Implementing `IsTraversableLike` for New Types===
 *
 * One must simply provide an implicit value of type `IsTraversableLike`
 * specific to the new type, or an implicit conversion which returns an
 * instance of `IsTraversableLike` specific to the new type.
 *
 * Below is an example of an implementation of the `IsTraversableLike` trait
 * where the `Repr` type is `String`.
 *
 *{{{
 * implicit val stringRepr: IsTraversableLike[String] { type A = Char } =
 *   new IsTraversableLike[String] {
 *     type A = Char
 *     val conversion = implicitly[String => GenTraversableLike[Char, String]]
 *   }
 *}}}
 *
 * @author Miles Sabin
 * @author J. Suereth
 * @since 2.10
 */
trait IsTraversableLike[Repr] {
  /** The type of elements we can traverse over. */
  type A
  /** A conversion from the representation type `Repr` to a `GenTraversableLike[A,Repr]`. */
  val conversion: Repr => GenTraversableLike[A, Repr]
}

object IsTraversableLike {
  import scala.language.higherKinds

  implicit val stringRepr: IsTraversableLike[String] { type A = Char } =
    new IsTraversableLike[String] {
      type A = Char
      val conversion = implicitly[String => GenTraversableLike[Char, String]]
    }

  implicit def genTraversableLikeRepr[C[_], A0](implicit conv: C[A0] => GenTraversableLike[A0,C[A0]]): IsTraversableLike[C[A0]] { type A = A0 } =
    new IsTraversableLike[C[A0]] {
      type A = A0
      val conversion = conv
    }
}

/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.collection
package generic

/** A trait which can be used to avoid code duplication when defining extension
 *  methods that should be applicable both to existing Scala collections (i.e.,
 *  types extending `Iterable`) as well as other (potentially user-defined)
 *  types that could be converted to a Scala collection type. This trait
 *  makes it possible to treat Scala collections and types that can be implicitly
 *  converted to a collection type uniformly. For example, one can provide
 *  extension methods that work both on collection types and on `String`s (`String`s
 *  do not extend `Iterable`, but can be converted to `Iterable`)
 *
 * `IsIterable` provides three members:
 *
 *  1. type member `A`, which represents the element type of the target `Iterable[A]`
 *  1. type member `C`, which represents the type returned by transformation operations that preserve the collection’s elements type
 *  1. method `apply`, which provides a way to convert between the type we wish to add extension methods to, `Repr`, and `IterableOps[A, Iterable, C]`.
 *
 * ===Usage===
 *
 * One must provide `IsIterable` as an implicit parameter type of an implicit
 * conversion. Its usage is shown below. Our objective in the following example
 * is to provide a generic extension method `mapReduce` to any type that extends
 * or can be converted to `Iterable`. In our example, this includes
 * `String`.
 *
 * {{{
 *    import scala.collection.{Iterable, IterableOps}
 *    import scala.collection.generic.IsIterable
 *
 *    class ExtensionMethods[Repr, I <: IsIterable[Repr]](coll: Repr, it: I) {
 *      def mapReduce[B](mapper: it.A => B)(reducer: (B, B) => B): B = {
 *        val iter = it(coll).iterator
 *        var res = mapper(iter.next())
 *        while (iter.hasNext)
 *          res = reducer(res, mapper(iter.next()))
 *        res
 *      }
 *    }
 *
 *    implicit def withExtensions[Repr](coll: Repr)(implicit it: IsIterable[Repr]): ExtensionMethods[Repr, it.type] =
 *      new ExtensionMethods(coll, it)
 *
 *  // See it in action!
 *  List(1, 2, 3).mapReduce(_ * 2)(_ + _) // res0: Int = 12
 *  "Yeah, well, you know, that's just, like, your opinion, man.".mapReduce(x => 1)(_ + _) // res1: Int = 59
 *}}}
 *
 * Here, we begin by creating a class `ExtensionMethods` which contains our
 * `mapReduce` extension method.
 *
 * Note that `ExtensionMethods` takes a constructor argument `coll` of type `Repr`, where
 * `Repr` represents (typically) the collection type, and an argument `it` of a subtype of `IsIterable[Repr]`.
 * The body of the method starts by converting the `coll` argument to an `IterableOps` in order to
 * call the `iterator` method on it.
 * The remaining of the implementation is straightforward.
 *
 * The `withExtensions` implicit conversion makes the `mapReduce` operation available
 * on any type `Repr` for which it exists an implicit `IsIterable[Repr]` instance.
 * Note how we keep track of the precise type of the implicit `it` argument by using the
 * `it.type` singleton type, rather than the wider `IsIterable[Repr]` type. We do that
 * so that the information carried by the type members `A` and `C` of the `it` argument
 * is not lost.
 *
 * When the `mapReduce` method is called on some type of which it is not
 * a member, implicit search is triggered. Because implicit conversion
 * `withExtensions` is generic, it will be applied as long as an implicit
 * value of type `IsIterable[Repr]` can be found. Given that the
 * `IsIterable` companion object contains implicit members that return values of type
 * `IsIterable`, this requirement is typically satisfied, and the chain
 * of interactions described in the previous paragraph is set into action.
 * (See the `IsIterable` companion object, which contains a precise
 * specification of the available implicits.)
 *
 * ''Note'': Currently, it's not possible to combine the implicit conversion and
 * the class with the extension methods into an implicit class due to
 * limitations of type inference.
 *
 * ===Implementing `IsIterable` for New Types===
 *
 * One must simply provide an implicit value of type `IsIterable`
 * specific to the new type, or an implicit conversion which returns an
 * instance of `IsIterable` specific to the new type.
 *
 * Below is an example of an implementation of the `IsIterable` trait
 * where the `Repr` type is `Range`.
 *
 *{{{
 * implicit val rangeRepr: IsIterable[Range] { type A = Int; type C = IndexedSeq[Int] } =
 *   new IsIterable[Range] {
 *     type A = Int
 *     type C = IndexedSeq[Int]
 *     def apply(coll: Range): IterableOps[Int, IndexedSeq, IndexedSeq[Int]] = coll
 *   }
 *}}}
 *
 * (Note that in practice the `IsIterable[Range]` instance is already provided by
 * the standard library, and it is defined as an `IsSeq[Range]` instance)
 */
trait IsIterable[Repr] extends IsIterableOnce[Repr] {

  /** The type returned by transformation operations that preserve the same elements
    * type (e.g. `filter`, `take`).
    *
    * In practice, this type is often `Repr` itself, excepted in the case
    * of `SeqView[A]` (and other `View[A]` subclasses), where it is “only” `View[A]`.
    */
  type C

  @deprecated("'conversion' is now a method named 'apply'", "2.13.0")
  override val conversion: Repr => IterableOps[A, Iterable, C] = apply(_)

  /** A conversion from the type `Repr` to `IterableOps[A, Iterable, C]` */
  def apply(coll: Repr): IterableOps[A, Iterable, C]

}

object IsIterable extends IsIterableLowPriority {

  // Straightforward case: IterableOps subclasses
  implicit def iterableOpsIsIterable[A0, CC0[X] <: IterableOps[X, Iterable, CC0[X]]]: IsIterable[CC0[A0]] { type A = A0; type C = CC0[A0] } =
    new IsIterable[CC0[A0]] {
      type A = A0
      type C = CC0[A0]
      def apply(coll: CC0[A]): IterableOps[A, Iterable, C] = coll
    }

  // The `BitSet` type can not be unified with the `CC0` parameter of
  // the above definition because it does not take a type parameter.
  // Hence the need for a separate case:
  implicit def bitSetOpsIsIterable[C0 <: BitSet with BitSetOps[C0]]: IsIterable[C0] { type A = Int; type C = C0 } =
    new IsIterable[C0] {
      type A = Int
      type C = C0
      def apply(coll: C0): IterableOps[Int, Iterable, C0] = coll
    }

}

trait IsIterableLowPriority {

  // Makes `IsSeq` instances visible in `IsIterable` companion
  implicit def isSeqLikeIsIterable[Repr](implicit
    isSeqLike: IsSeq[Repr]
  ): IsIterable[Repr] { type A = isSeqLike.A; type C = isSeqLike.C } = isSeqLike

  // Makes `IsMap` instances visible in `IsIterable` companion
  implicit def isMapLikeIsIterable[Repr](implicit
    isMapLike: IsMap[Repr]
  ): IsIterable[Repr] { type A = isMapLike.A; type C = isMapLike.C } = isMapLike

}

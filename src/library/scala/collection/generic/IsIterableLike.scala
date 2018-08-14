package scala.collection
package generic

import scala.collection.mutable
import scala.reflect.ClassTag


/** A trait which can be used to avoid code duplication when defining extension
 *  methods that should be applicable both to existing Scala collections (i.e.,
 *  types extending `Iterable`) as well as other (potentially user-defined)
 *  types that could be converted to a Scala collection type. This trait
 *  makes it possible to treat Scala collections and types that can be implicitly
 *  converted to a collection type uniformly. For example, one can provide
 *  extension methods that work both on collection types and on `String`s (`String`s
 *  do not extend `Iterable`, but can be converted to `Iterable`)
 *
 * `IsIterableLike` provides two members:
 *
 *  1. type member `A`, which represents the element type of the target `Iterable[A]`
 *  1. value member `conversion`, which provides a way to convert between the type we wish to add extension methods to, `Repr`, and `Iterable[A]`.
 *
 * ===Usage===
 *
 * One must provide `IsIterableLike` as an implicit parameter type of an implicit
 * conversion. Its usage is shown below. Our objective in the following example
 * is to provide a generic extension method `mapReduce` to any type that extends
 * or can be converted to `Iterable`. In our example, this includes
 * `String`.
 *
 * {{{
 *    import scala.collection.{Iterable, IterableOps}
 *    import scala.collection.generic.IsIterableLike
 *
 *    class ExtensionMethods[A, Repr](coll: IterableOps[A, Iterable, Repr]) {
 *      def mapReduce[B](mapper: A => B)(reducer: (B, B) => B): B = {
 *        val iter = coll.iterator
 *        var res = mapper(iter.next())
 *        while (iter.hasNext)
 *          res = reducer(res, mapper(iter.next()))
 *        res
 *      }
 *    }
 *
 *    implicit def withExtensions[Repr](coll: Repr)(implicit Iterable: IsIterableLike[Repr]) =
 *      new ExtensionMethods(Iterable.conversion(coll))
 *
 *  // See it in action!
 *  List(1, 2, 3).mapReduce(_ * 2)(_ + _) // res0: Int = 12
 *  "Yeah, well, you know, that's just, like, your opinion, man.".mapReduce(x => 1)(_ + _) // res1: Int = 59
 *}}}
 *
 * Here, we begin by creating a class `ExtensionMethods` which contains our
 * `mapReduce` extension method. Note that `ExtensionMethods` takes a constructor
 * argument `coll` of type `IterableLike[A, Repr]`, where `A` represents the
 * element type and `Repr` represents (typically) the collection type. The
 * implementation of `mapReduce` itself is straightforward.
 *
 * The interesting bit is the implicit conversion `withExtensions`, which
 * returns an instance of `ExtensionMethods`. This implicit conversion can
 * only be applied if there is an implicit value `Iterable` of type
 * `IsIterableLike[Repr]` in scope. Since `IsIterableLike` provides
 * value member `conversion`, which gives us a way to convert between whatever
 * type we wish to add an extension method to (in this case, `Repr`) and
 * `IterableLike[A, Repr]`, we can now convert `coll` from type `Repr`
 * to `IterableLike[A, Repr]`. This allows us to create an instance of
 * the `ExtensionMethods` class, which we pass our new
 * `IterableLike[A, Repr]` to.
 *
 * When the `mapReduce` method is called on some type of which it is not
 * a member, implicit search is triggered. Because implicit conversion
 * `withExtensions` is generic, it will be applied as long as an implicit
 * value of type `IsIterableLike[Repr]` can be found. Given that
 * `IsIterableLike` contains implicit members that return values of type
 * `IsIterableLike`, this requirement is typically satisfied, and the chain
 * of interactions described in the previous paragraph is set into action.
 * (See the `IsIterableLike` companion object, which contains a precise
 * specification of the available implicits.)
 *
 * ''Note'': Currently, it's not possible to combine the implicit conversion and
 * the class with the extension methods into an implicit class due to
 * limitations of type inference.
 *
 * ===Implementing `IsIterableLike` for New Types===
 *
 * One must simply provide an implicit value of type `IsIterableLike`
 * specific to the new type, or an implicit conversion which returns an
 * instance of `IsIterableLike` specific to the new type.
 *
 * Below is an example of an implementation of the `IsIterableLike` trait
 * where the `Repr` type is `String`.
 *
 *{{{
 * implicit val stringRepr: IsIterableLike[String] { type A = Char } =
 *   new IsIterableLike[String] {
 *     type A = Char
 *     val conversion = implicitly[String => IterableOps[Char, Any, String]]
 *   }
 *}}}
 */
trait IsIterableLike[Repr] {
  /** The type of elements we can traverse over. */
  type A
  /** A conversion from the representation type `Repr` to `IterableOps[A, Iterable, Repr]`. */
  val conversion: Repr => IterableOps[A, Iterable, Repr]
}

object IsIterableLike {
  import scala.language.higherKinds

  implicit val stringRepr: IsIterableLike[String] { type A = Char } =
    new IsIterableLike[String] {
      type A = Char
      val conversion: String => IterableOps[Char, Iterable, String] = s => new IterableOps[Char, Iterable, String] {
        def toIterable: Iterable[Char] = new immutable.WrappedString(s)
        protected[this] def coll: String = s
        protected[this] def fromSpecific(coll: IterableOnce[Char]): String = coll.mkString
        def iterableFactory: IterableFactory[Iterable] = Iterable
        protected[this] def newSpecificBuilder: mutable.Builder[Char, String] = new StringBuilder
        def iterator: Iterator[Char] = s.iterator
        override def isEmpty: Boolean = s.isEmpty
        override def knownSize: Int = s.length
        override def size: Int = s.length
      }
    }

  implicit def arrayRepr[A0: ClassTag]: IsIterableLike[Array[A0]] { type A = A0 } =
    new IsIterableLike[Array[A0]] {
      type A = A0
      val conversion: Array[A] => IterableOps[A, Iterable, Array[A]] = a => new IterableOps[A, Iterable, Array[A]] {
        def toIterable: Iterable[A] = mutable.ArraySeq.make(a)
        protected def coll: Array[A] = a
        protected def fromSpecific(coll: IterableOnce[A]): Array[A] = Array.from(coll)
        def iterableFactory: IterableFactory[Iterable] = Iterable
        protected def newSpecificBuilder: mutable.Builder[A, Array[A]] = Array.newBuilder
        def iterator: Iterator[A] = a.iterator
        override def isEmpty: Boolean = a.length == 0
        override def knownSize: Int = a.length
        override def size: Int = a.length
      }
    }

  implicit def iterableRepr[C[X] <: Iterable[X], A0](implicit conv: C[A0] => IterableOps[A0, C, C[A0]]): IsIterableLike[C[A0]] { type A = A0 } =
    new IsIterableLike[C[A0]] {
      type A = A0
      val conversion = conv
    }
}

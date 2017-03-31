package strawman.collection

import scala.language.implicitConversions

import scala.Any
import scala.annotation.implicitNotFound
import mutable.Builder

/** BuildFrom can be used by methods outside the collections framework to build collections of various types without
  * having to define overloaded methods for unconstrained / constrained / map-like / etc. collections like we do in
  * the collections framework.
  */
@implicitNotFound(msg = "Cannot construct a collection with elements of type ${Elem} based on a collection of type ${From}.")
trait BuildFrom[-From, -Elem] {
  type To

  /** Creates a new builder on request of a collection.
   *  @param from  the collection requesting the builder to be created.
   *  @return a builder for collections of type `To` with element type `Elem`.
   */
  def newBuilder(from: From): Builder[Elem, To]

  def fromIterable(from: From)(it: Iterable[Elem]): To
}

object BuildFrom {
  /** Build the source collection type from a PolyBuildable */
  implicit def buildFromPolyBuildable[C[X] <: PolyBuildable[X, C], A, E]: BuildFrom[C[A], E] { type To = C[E] } = new BuildFrom[C[A], E] {
    //TODO: Reuse a prototype instance
    type To = C[E]
    def newBuilder(from: C[A]): Builder[E, To] = from.newBuilder[E]
    def fromIterable(from: C[A])(it: Iterable[E]): To = from.fromIterable(it)
  }

  /** Build the source collection type from a ConstrainedPolyBuildable */
  implicit def buildFromConstrainedPolyBuildable[Ev[_], Impl[_], CC[_], A, E : Ev]: BuildFrom[ConstrainedPolyBuildable[A, CC, Ev], E] { type To = CC[E] } = new BuildFrom[ConstrainedPolyBuildable[A, CC, Ev], E] {
    //TODO: Reuse a prototype instance
    type To = CC[E]
    def newBuilder(from: ConstrainedPolyBuildable[A, CC, Ev]): Builder[E, To] = from.newConstrainedBuilder[E]
    def fromIterable(from: ConstrainedPolyBuildable[A, CC, Ev])(it: Iterable[E]): To = from.constrainedFromIterable(it)
  }

  /** Convert an IterableFactory to a BuildFrom */
  implicit def buildBoundedIterableFactory[E, B >: E](fact: BoundedIterableFactory[B]): BuildFrom[Any, E] { type To = fact.To[E] } = new BuildFrom[Any, E] {
    type To = fact.To[E]
    def newBuilder(from: Any): Builder[E, To] = fact.newBuilder
    def fromIterable(from: Any)(it: Iterable[E]): To = fact.fromIterable(it)
  }

  /** Convert a ConstrainedIterableFactory to a BuildFrom */
  implicit def buildConstrainedIterableFactory[CC[_], Ev[_], E : Ev](fact: ConstrainedIterableFactory[CC, Ev]): BuildFrom[Any, E] { type To = CC[E] } = new BuildFrom[Any, E] {
    type To = CC[E]
    def newBuilder(from: Any): Builder[E, To] = fact.constrainedNewBuilder
    def fromIterable(from: Any)(it: Iterable[E]): To = fact.constrainedFromIterable(it)
  }

  /** Convert a MapFactory to a BuildFrom */
  implicit def buildMapFactory[C[_, _], K, V](fact: MapFactory[C]): BuildFrom[Any, (K, V)] { type To = C[K, V] } = new BuildFrom[Any, (K, V)] {
    type To = C[K, V]
    def newBuilder(from: Any): Builder[(K, V), To] = fact.newBuilder
    def fromIterable(from: Any)(it: Iterable[(K, V)]): To = fact.fromIterable(it)
  }

  /** Convert a ConstrainedMapFactory to a BuildFrom */
  implicit def buildConstrainedMapFactory[CC[_, _], Ev[_], K : Ev, V](fact: ConstrainedMapFactory[CC, Ev]): BuildFrom[Any, (K, V)] { type To = CC[K, V] } = new BuildFrom[Any, (K, V)] {
    type To = CC[K, V]
    def newBuilder(from: Any): Builder[(K, V), To] = fact.constrainedNewBuilder
    def fromIterable(from: Any)(it: Iterable[(K, V)]): To = fact.constrainedFromIterable(it)
  }
}

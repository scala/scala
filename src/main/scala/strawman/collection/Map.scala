package strawman.collection

import strawman.collection.mutable.Builder

import scala.{Option, Any, Nothing}
import scala.annotation.unchecked.uncheckedVariance
import scala.Predef.???

/** Base Map type */
trait Map[K, +V]
  extends Iterable[(K, V)]
    with MapLike[K, V, Map]

/** Base Map implementation type */
trait MapLike[K, +V, +C[X, Y] <: Map[X, Y]]
  extends IterableLike[(K, V), Iterable]
    with IterableMonoTransforms[(K, V), C[K, V @uncheckedVariance]]
    with MapPolyTransforms[K, V, C] {

  def get(key: K): Option[V]

}

/** Polymorphic transformation methods */
trait MapPolyTransforms[K, +V, +C[X, Y] <: Map[X, Y]] extends IterablePolyTransforms[(K, V), Iterable] {

  def map[K2, V2](f: (K, V) => (K2, V2)): C[K2, V2]

  def flatMap[K2, V2](f: (K, V) => IterableOnce[(K2, V2)]): C[K2, V2]

}

/** Factory methods for collections of kind `* −> * -> *` */
trait MapFactory[+C[_, _]] extends BuildConstrained { self =>
  class ConstraintImpl[E, Repr] extends Constraint[E] {
    type To[_] = Repr
    def fromIterable[X <: E](it: Iterable[X]): Repr = ???
  }
  implicit def constraint[K, V]: ConstraintImpl[(K, V), C[K, V] @uncheckedVariance] = new ConstraintImpl[(K, V), C[K, V] @uncheckedVariance]

  def newBuilder[K, V]: Builder[(K, V), C[K, V]]

  def fromIterable[K, V](it: Iterable[(K, V)]): C[K, V] =
    newBuilder[K, V].++=(it).result

  def empty[K, V]: C[K, V] =
    newBuilder[K, V].result

  def apply[K, V](elems: (K, V)*): C[K, V] =
    newBuilder[K, V].++=(elems.toStrawman).result

  protected[this] lazy val buildFromProto: BuildFrom[C[Any, Any], (Any, Any)] { type To[_] <: C[Any, Any] } = new BuildFrom[C[Any, Any], (Any, Any)] {
    type To[_] = C[Any, Any]
    def fromIterable[E <: (Any, Any)](it: Iterable[E]): C[Any, Any] = self.fromIterable[Any, Any](it)
    def newBuilder[E <: (Any, Any)]: Builder[E, C[Any, Any]] = self.newBuilder[Any, Any]
  }

  implicit def buildFrom[K2, V2, K, V]: BuildFrom[C[K2, V2], (K, V)] { type To[_] <: C[K, V] } =
    buildFromProto.asInstanceOf[BuildFrom[C[K2, V2], (K, V)] { type To[_] <: C[K, V] }]

  def buildFromAny[K, V]: BuildFrom[Nothing, (K, V)] { type To[_] <: C[K, V] } =
    buildFrom[Any, Any, K, V].asInstanceOf[BuildFrom[Nothing, (K, V)] { type To[_] <: C[K, V] }]
}

/** Factory methods for collections of kind `* −> * -> *` which require an implicit evidence value for the key type */
trait ConstrainedMapFactory[+C[_, _], Ev[_]] { self =>

  def constrainedNewBuilder[K : Ev, V]: Builder[(K, V), C[K, V]]

  def constrainedFromIterable[K : Ev, V](it: Iterable[(K, V)]): C[K, V] =
    constrainedNewBuilder[K, V].++=(it).result

  def empty[K : Ev, V]: C[K, V] =
    constrainedNewBuilder[K, V].result

  def apply[K : Ev, V](elems: (K, V)*): C[K, V] =
    constrainedNewBuilder[K, V].++=(elems.toStrawman).result

  implicit def buildFrom[K2, V2, K : Ev, V]: BuildFrom[C[K2, V2], (K, V)] { type To[_] <: C[K, V] } = new BuildFrom[C[K2, V2], (K, V)] {
    type To[_] = C[K, V]
    def fromIterable[E <: (K, V)](it: Iterable[E]): C[K, V] = self.constrainedFromIterable(it)
    def newBuilder[E <: (K, V)]: Builder[E, C[K, V]] = self.constrainedNewBuilder
  }

  def buildFromAny[K : Ev, V]: BuildFrom[Nothing, (K, V)] { type To[_] <: C[K, V] } =
    buildFrom[Any, Any, K, V].asInstanceOf[BuildFrom[Nothing, (K, V)] { type To[_] <: C[K, V] }]
}

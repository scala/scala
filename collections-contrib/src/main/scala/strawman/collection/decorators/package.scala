package strawman.collection

import scala.language.implicitConversions

package object decorators {

  implicit def iteratorDecorator[A](it: Iterator[A]): IteratorDecorator[A] =
    new IteratorDecorator[A](it)

  implicit def iterableDecorator[A](it: Iterable[A]): IterableDecorator[A] =
    new IterableDecorator(it)

  implicit def SeqDecorator[A, CC[X] <: SeqOps[X, CC, _]](seq: CC[A]): SeqDecorator[A, CC] =
    new SeqDecorator[A, CC](seq)

  implicit def MapDecorator[K, V](map: Map[K, V]): MapDecorator[K, V] { val `this`: map.type } =
    new MapDecorator[K, V] { val `this`: map.type = map }

  implicit def ImmutableMapDecorator[K, V, CC[X, +Y] <: immutable.Map[X, Y]](map: CC[K, V]): ImmutableMapDecorator[K, V, CC] =
    new ImmutableMapDecorator[K, V, CC](map)

  implicit def MutableMapDecorator[K, V](map: mutable.Map[K, V]): MutableMapDecorator[K, V] =
    new MutableMapDecorator[K, V](map)

}

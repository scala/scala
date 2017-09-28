package strawman.collection

import scala.language.implicitConversions

package object decorators {

  implicit def IteratorDecorator[A](it: Iterator[A]): IteratorDecorator[A] =
    new IteratorDecorator[A](it)

  implicit def SeqDecorator[A, CC[X] <: SeqOps[X, CC, _]](seq: CC[A]): SeqDecorator[A, CC] =
    new SeqDecorator[A, CC](seq)

  implicit def MapDecorator[K, V](map: Map[K, V]): MapDecorator[K, V] { val `this`: map.type } =
    new MapDecorator[K, V] { val `this`: map.type = map }

}

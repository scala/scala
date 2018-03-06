package strawman.collection

import scala.language.implicitConversions

package object decorators {

  implicit def iteratorDecorator[A](it: Iterator[A]): IteratorDecorator[A] =
    new IteratorDecorator[A](it)

  implicit def IterableDecorator[C](coll: C)(implicit it: HasIterableOps[C]): IterableDecorator[C, it.type] =
    new IterableDecorator(coll)(it)

  implicit def SeqDecorator[C](coll: C)(implicit seq: HasSeqOps[C]): SeqDecorator[C, seq.type] =
    new SeqDecorator(coll)(seq)

  implicit def MapDecorator[C](coll: C)(implicit map: HasMapOps[C]): MapDecorator[C, map.type] =
    new MapDecorator(coll)(map)

  implicit def ImmutableMapDecorator[C](coll: C)(implicit map: HasImmutableMapOps[C]): ImmutableMapDecorator[C, map.type] =
    new ImmutableMapDecorator(coll)(map)

  implicit def MutableMapDecorator[K, V](map: mutable.Map[K, V]): MutableMapDecorator[K, V] =
    new MutableMapDecorator[K, V](map)

}

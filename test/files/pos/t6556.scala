package nl.ndervorst.commons.scalapimps

trait Adapter[X] {self =>
  type This = self.type
  val adaptee: X
  val adapt: This = self
}

object Adapter {
  implicit def adaptee[Adaptee](adapter: Adapter[Adaptee]) = adapter.adaptee
}



object IterableW {
  def zipMerge[E](it1: Iterable[E], it2: Iterable[E])(implicit o: Ordering[E]): Iterable[(Option[E], Option[E])] = null
}


class Series[X: Ordering, Y](val adaptee: Iterable[(X, Y)]) extends Adapter[Iterable[(X, Y)]] {
  val order = implicitly[Ordering[X]]
  def zipMerge(other: Series[X, Y]): Series[X, (Option[Y], Option[Y])] = IterableW.zipMerge(this, other)(new Ordering[(X, Y)] {
    def compare(xy1: (X, Y), xy2: (X, Y)) = order.compare(xy1._1, xy2._1)
  }).map {
    case _ => null
  }
}


object Series {
  implicit def wrap[X: Ordering, Y](itble: Iterable[(X, Y)]): Series[X, Y] = new Series(itble)
}

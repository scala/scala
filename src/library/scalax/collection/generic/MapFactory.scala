package scalax.collection.generic

trait MapFactory[CC[A, B] <: MapTemplate[A, B, CC]] {

  def empty[A, B]: CC[A, B]

  def apply[A, B](elems: (A, B)*): CC[A, B] = empty[A, B] ++ elems.asInstanceOf[Iterable[(A, B)]] // !@!

}

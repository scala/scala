class Foo[T] {
  def toMap[K, V](implicit ev: Foo[T] <:< Foo[(K, V)]): Foo[Map[K, V]] = null
  def toMap[K](keySelector:    T      =>  K): Foo[Map[K, T]] = null
}

object Foo {
  (??? : Foo[Int])           toMap (_ % 2)
  (??? : Foo[(Int, String)]).toMap
}

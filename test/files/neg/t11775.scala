object Test {
  trait Cmp[T] { def compareTo(o: T): Int }
  class K[T] extends Cmp[K[_ >: T]] { def compareTo(that: K[_ >: T]): Int = ??? }
  (new K[String]).compareTo(new K[Int])

  class Event[+T](weight: Int, data: T) extends Ordered[Event[_ <: T]] {
    def compare(that: Event[_ <: T]): Int = weight - that.weight
  }
}
class IntMap[+V]
case class Bin[+T](m: IntMap[T]) extends IntMap[T]
case class Tip[+T](x: T) extends IntMap[T]

trait IntMapIterator[V, T] {
  def valueOf(tip: Tip[V]): T
  def pop: IntMap[V]

  def next: T =
    pop match {
      case Bin(t@Tip(_)) => {
        valueOf(t)
      }
    }
}
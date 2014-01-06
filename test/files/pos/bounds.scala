trait Map[A, +C] {
  def ++ [B1 >: C] (kvs: Iterable[Tuple2[A, B1]]): Map[A, B1] = this
  def ++ [B1 >: C] (kvs: Iterator[Tuple2[A, B1]]): Map[A, B1] = this
}

class ListMap[A, +B] extends Map[A, B] {}

object ListMap {
  def empty[X, Y] = new ListMap[X, Y]
  def apply[A1, B2](elems: Tuple2[A1, B2]*): Map[A1, B2] = empty[A1,B2].++(elems.iterator)
}

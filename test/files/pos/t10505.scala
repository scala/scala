object Test {
  def length[@specialized(Int) A](as: List[A], len: A => Int, acc: Int): Int =
    as match {
      case Nil => acc
      case h :: t => length(t, len, acc + len(h))
    }
}

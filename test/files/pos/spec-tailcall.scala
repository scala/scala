class TailCall[@specialized T] {
  final def dropLeft(n: Int, xs: List[T]): List[T] =
    if (n == 0) xs
    else dropLeft(n - 1, xs.tail)
/*
  def filter(pf: PartialFunction[Option[String], Boolean]) = null

  def crash(o: Option[String]) = filter {
    case None if {
      def dropLeft[T](n: Int, xs: List[T]): List[T] =
        if (n == 0) xs
        else dropLeft(n - 1, xs.tail)
      dropLeft(2, List(1, 2, 3)).isEmpty
    } => true
  }
*/
}

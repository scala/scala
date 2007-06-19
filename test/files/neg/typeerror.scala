object Test {
  def add2(x:Long,y:Long): Long = x + y

  def add[Long](x: List[Long], y: List[Long]): List[Long] =
    if (x.isEmpty || y.isEmpty) Nil
    else add2(x.head, y.head) :: add(x.tail, y.tail)
}

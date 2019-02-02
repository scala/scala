class Lst[+A] {
  def map[B, That](f: A => B)(implicit bf: collection.BuildFrom[List[A], B, That]): That = ???
}

object Test {
  def foo(l: Lst[Int]) = l.map[Int, List[String]](x => 1)
}

object Test {
  val xs: List[?] = List(1, 2, 3)
  val ys: Map[? <: AnyRef, ? >: Null] = Map()

  def foo(x: Any) = x match {
    case x: List[?] => x
    case _ => x
  }

  type `?` = Int

  val xs2: List[`?`] = List(1)
  val xs3: List[Int] = xs2

  def foo2(x: List[`?`]): List[Int] = x match {
    case x: List[`?`] => x
  }
}

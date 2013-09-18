object Test {
  def f1(x: Any) = x.isInstanceOf[Seq[_]]
  def f2(x: Any) = x match {
    case _: Seq[_]  => true
    case _          => false
  }

  def f3(x: Any) = x match {
    case _: Array[_]  => true
    case _            => false
  }

  def f4(x: Any) = x.isInstanceOf[Traversable[_]]

  def f5(x1: Any, x2: Any, x3: AnyRef) = (x1, x2, x3) match {
    case (Some(_: Seq[_]), Nil, _)        => 1
    case (None, List(_: List[_], _), _)   => 2
    case _                                => 3
  }

  def f5: Unit = {
    import scala.collection.mutable._
    List(1,2,3,4,5).scanRight(0)(_+_)
  }
}

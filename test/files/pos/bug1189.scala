object test extends Application {
  case class Cell[T](x: T)
  type U = Cell[T1] forSome { type T1 }
  def f[T](x: Any): U = x match { case y: Cell[_] => y }

  var x: U = Cell(1)
  println(x)
}

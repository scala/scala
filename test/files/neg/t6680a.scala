case class Cell[A](var x: A)
object Test {
  def f1(x: Any)        = x match { case y @ Cell(_) => y } // Inferred type is Cell[Any]
  def f2(x: Cell[_])    = x match { case y @ Cell(_) => y } // Inferred type is Cell[_]
  def f3[A](x: Cell[A]) = x match { case y @ Cell(_) => y } // Inferred type is Cell[A]

  def main(args: Array[String]): Unit = {
    val x = new Cell(1)
    val y = f1(x)
    y.x = "abc"
    println(x.x + 1)
  }
}

// The tweetable variation
object Tweet {
  case class C[A](f:A=>A);def f(x:Any)=x match { case C(f)=>f("") };f(C[Int](x=>x))
}

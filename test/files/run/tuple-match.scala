object Test {
  val FOUR = (-1, -2, -3, "bingo donkey vegas")
  val THREE = (-1, -2, -3)
  
  def f(x: Any) = x match {
    case FOUR         => "FOUR"
    case (_, _, 3, _) => "4, #3"
    case (_, 2, _, _) => "4, #2"
    case (_, 2, _)    => "3, #2"
    case Tuple1(1)    => "1, #1"
    case (_, _, _, 4) => "4, #4"
    case THREE        => "THREE"
    case (_, 2)       => "2, #2"
    case _            => "default"
  }
  
  def main(args: Array[String]): Unit = {
    println(f((1, 2, 3, 4)))
    println(f((1, 2, 30, 4)))
    println(f((1, 20, 30, 4)))
    println(f((1, 2, 3)))
    println(f((1, 2)))
    println(f(Tuple1(1)))
    println(f((-1, -2, -3, "bingo donkey vegas")))
    println(f((-1, -2, -3)))
  }
}

object Test {
  def foo(h: Any, t: List[Any]) = h match {
    case 5 :: _     => ()
    case List(from) => from
    case x          => throw new MatchError(x)
  }

  def main(args: Array[String]): Unit = {
    println(foo(5 :: Nil, List(1,2,3)))
    println(foo(6 :: Nil, List(1,2,3)))
  }
}

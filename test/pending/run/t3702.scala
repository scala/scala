object Test {
  def main(args: Array[String]) {
    foo(Nil, Nil)
  }

  def foo(h: Any, t: List[Any]) = h match {
    case 5 :: _ => ()
    case List(from) => List(from, from, from)
  }
}

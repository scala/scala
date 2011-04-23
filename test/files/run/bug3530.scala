object Test {
  def f(x: Any) = println(x match {
    case List(_, _)    => "two"
    case List(_, _, _) => "three"
    case xs @ List(_*) => "list: " + xs.length
    case _             => "not a list"
  })

  def f2[T](x: List[T]) = println(x match {
    case List(_, _)       => "two"
    case List(_, _, _)    => "three"
    case List(xs @ _*)    => "list: " + xs.length
    // bug: the default case is marked unreachable
    // case _                => "not a list"
  })

  def main(args: Array[String]) {
    f(List(1, 2))
    f(List('a', 'b', 'c'))
    f(List('a', 'b', 'c', 'd'))
    f(Nil)
    f(List(1,2,3,4,5))
    f(null)

    println

    f2(List(1, 2))
    f2(List('a', 'b', 'c'))
    f2(List('a', 'b', 'c', 'd'))
    f2(Nil)
    f2(List(1,2,3,4,5))
    // bug: this NPEs on xs.length
    // f2(null)
  }
}
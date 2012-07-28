class A {
  def f(x: Any) = x match {
    case xs: List[String @unchecked]               => xs.head   // okay
    case xs: Set[Int]                              => xs.head   // unchecked
    case xs: Map[String @unchecked, String]        => xs.head   // one unchecked, one okay
    case f: ((Int @unchecked) => (Int @unchecked)) => f(5)      // okay
    case f: ((Int, Int) => Int)                    =>           // unchecked
    case _                                         => ""
  }
}

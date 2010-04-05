object Bop {
  implicit def int2list(x: Int): List[String] = List("hello", "world")
}
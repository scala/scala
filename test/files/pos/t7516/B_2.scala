object B {
  final case class CV(p: Int = 3, g: Int = 2)
  A.demo { val d = 4; CV(g = d); "a" }
}

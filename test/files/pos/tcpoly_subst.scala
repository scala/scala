object test {
  def make[m[x], b]: m[b] = error("foo")
  val lst: List[Int] = make[List, Int]
}

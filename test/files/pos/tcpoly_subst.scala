object test {
  def make[m[x], b]: m[b] = sys.error("foo")
  val lst: List[Int] = make[List, Int]
}

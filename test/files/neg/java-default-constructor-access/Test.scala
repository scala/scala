object W extends a.A {
  def s: Any = new a.A.S
  def i: Any = new I
  class SubS extends a.A.S // ok
  class SubI extends I     // ok
}

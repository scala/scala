class A {
  class C[x]
  val cs = new scala.collection.mutable.HashMap[C[_], Int]
  def c: C[_] = sys.error("")
  val eval: C[_] = c
  cs(c) += 1
}

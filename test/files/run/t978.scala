class Foo(val n: Int) {
  override def hashCode = n % 2 // pretty bad hash
  override def equals(other: Any): Boolean = other match {
    case f: Foo => f.n == n
    case _ => false
  }

  override def toString = "" + n
}

object Test extends App {
  val set = new collection.mutable.HashSet[Foo]
//  val set = new collection.jcl.HashSet[Foo]

  val max = 200
  for (x <- 1 to max)
    set += new Foo(x)

  testRemove(2)
  testExists(2)

  def testRemove(m: Int) {
    for (x <- 1 to max; if x % m == 0) {
      val f = new Foo(x)
      set -= f
      assert(!(set contains f))
      testExists(m)
    }
  }

  def testExists(m: Int) {
    for (x <- 1 to max; if x % m == 1) {
      val f = new Foo(x)
      assert(set contains f, "For element: " + f + " set: " + set)
    }
  }

}

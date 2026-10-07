
class C {
  val b = collection.mutable.ListBuffer.empty[Int]
  def f = b.remove
  def g = b.update
}

class D {
  val c = collection.immutable.BitSet(1, 2, 3)
  def f = c.map
}

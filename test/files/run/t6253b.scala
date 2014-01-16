import scala.collection.immutable.HashSet

object Test extends App {

  var hashCount = 0

  /**
   * A key that produces lots of hash collisions, to exercise the part of the code that deals with those
   */
  case class Collision(value: Int) {

    override def hashCode = {
      hashCount += 1
      value / 5
    }
  }

  /**
   * A key that is identical to int other than that it counts hashCode invocations
   */
  case class HashCounter(value: Int) {

    override def hashCode = {
      hashCount += 1
      value
    }
  }

  def testIntersect[T](sizes: Seq[Int], offsets: Seq[Double], keyType: String, mkKey: Int => T) {
    for {
      i <- sizes
      o <- offsets
    } {
      val e = HashSet.empty[T]
      val j = (i * o).toInt
      // create two sets of size i with overlap o
      val a = e ++ (0 until i).map(mkKey)
      require(a.size == i, s"Building HashSet of size $i failed. Key type $keyType.")
      val b = e ++ (j until (i + j)).map(mkKey)
      require(b.size == i, s"Building HashSet of size $i failed. Key type $keyType.")
      val as = e ++ (0 until j).map(mkKey)
      require(as.size == j, s"Building HashSet of size $j failed. Key type $keyType.")
      val hashCount0 = hashCount
      val u = a intersect b
      require(hashCount == hashCount0, s"key.hashCode should not be called, but has been called ${hashCount - hashCount0} times. Key type $keyType.")
      require(u == (a intersect scala.collection.mutable.HashSet(b.toSeq: _*)), s"Operation must still work for other sets!")
      require(u.size == i - j, s"Expected size ${i + j}. Real size ${u.size}. Key type $keyType.")
      for (x <- j until i)
        require(u.contains(mkKey(x)), s"Key type $keyType. Set (0 until ${i + j}) should contain $x but does not.")
      val a_as = a intersect as
      val as_a = as intersect a
      require((a_as eq as) || (a_as eq a), s"No structural sharing in a intersect as. Key type $keyType, a=(0 until $i) as=(0 until $j)")
      require((as_a eq as) || (as_a eq a), s"No structural sharing in as intersect a. Key type $keyType, a=(0 until $i) as=(0 until $j)")
    }
  }

  val sizes = Seq(1, 10, 100, 1000, 10000, 100000)
  val offsets = Seq(0.0, 0.25, 0.5, 0.75, 1.0)
  testIntersect(sizes, offsets, "int", identity[Int])
  testIntersect(sizes, offsets, "hashcounter", HashCounter.apply)
  testIntersect(sizes, offsets, "collision", Collision.apply)
}

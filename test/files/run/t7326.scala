import scala.collection.immutable.ListSet
import scala.collection.immutable.HashSet

object Test extends App {

  def testCorrectness() {
    // a key that has many hashCode collisions
    case class Collision(i: Int) { override def hashCode = i / 5 }

    def subsetTest[T](emptyA:Set[T], emptyB:Set[T], mkKey:Int => T, n:Int) {
      val outside = mkKey(n + 1)
      for(i <- 0 to n) {
        val a = emptyA ++ (0 until i).map(mkKey)
        // every set must be a subset of itself
        require(a.subsetOf(a), "A set must be the subset of itself")
        for(k <- 0 to i) {
          // k <= i, so b is definitely a subset
          val b = emptyB ++ (0 until k).map(mkKey)
          // c has less elements than a, but contains a value that is not in a
          // so it is not a subset, but that is not immediately obvious due to size
          val c = b + outside
          require(b.subsetOf(a), s"$b must be a subset of $a")
          require(!c.subsetOf(a), s"$c must not be a subset of $a")
        }
      }
    }

    // test the HashSet/HashSet case
    subsetTest(HashSet.empty[Int], HashSet.empty[Int], identity, 100)

    // test the HashSet/other set case
    subsetTest(HashSet.empty[Int], ListSet.empty[Int], identity, 100)

    // test the HashSet/HashSet case for Collision keys
    subsetTest(HashSet.empty[Collision], HashSet.empty[Collision], Collision, 100)

    // test the HashSet/other set case for Collision keys
    subsetTest(HashSet.empty[Collision], ListSet.empty[Collision], Collision, 100)
  }

  /**
   * A main performance benefit of the new subsetOf is that we do not have to call hashCode during subsetOf
   * since we already have the hash codes in the HashSet1 nodes.
   */
  def testNoHashCodeInvocationsDuringSubsetOf() = {
    var count = 0

    case class HashCodeCounter(i:Int) {
      override def hashCode = {
        count += 1
        i
      }
    }

    val a = HashSet.empty ++ (0 until 100).map(HashCodeCounter)
    val b = HashSet.empty ++ (0 until 50).map(HashCodeCounter)
    val count0 = count
    val result = b.subsetOf(a)
    require(count == count0, "key.hashCode must not be called during subsetOf of two HashSets")
    result
  }
  testCorrectness()
  testNoHashCodeInvocationsDuringSubsetOf()
}

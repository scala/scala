package scala.collection.immutable

// "Disabled string conversions so as not to get confused!"
import scala.Predef.{any2stringadd => _, _}

import org.junit.Assert.{assertEquals, assertNotSame, assertSame, assertTrue}
import org.junit.Test

class SetTest {
  @Test
  def test_SI8346_toSet_soundness(): Unit = {
    
    def any[A](set: Set[A]): Set[Any] = {
      val anyset = set.toSet[Any]
      assertTrue((anyset + "fish") contains "fish")
      anyset
    }

    // Make sure default immutable Set does not rebuild itself on widening with toSet
    // Need to cover 0, 1, 2, 3, 4 elements as special cases
    var si = Set.empty[Int]
    assert(si eq si.toSet[Any])
    for (i <- 1 to 5) {
      val s1 = Set(Array.range(1, i+1).toIndexedSeq: _*)
      val s2 = si + i
      val s1a = any(s1)
      val s2a = any(s2)
      assertSame(s1, s1a)
      assertSame(s2, s2a)
      si = s2
    }

    // Make sure BitSet correctly rebuilds itself on widening with toSet
    // Need to cover empty, values 0-63, values 0-127 as special cases
    val bitsets = Seq(BitSet.empty, BitSet(23), BitSet(23, 99), BitSet(23, 99, 141))
    bitsets.foreach{ b =>
      val ba = any(b)
      assertNotSame(b, ba)
      assertEquals(b, ba)
    }

    // Make sure HashSet (and by extension, its implementing class HashTrieSet)
    // does not rebuild itself on widening by toSet
    val hashset = HashSet(1, 3, 5, 7)
    val hashseta = any(hashset)
    assertSame(hashset, hashseta)

    // Make sure ListSet does not rebuild itself on widening by toSet
    // (Covers Node also, since it subclasses ListSet)
    val listset = ListSet(1, 3, 5, 7)
    val listseta = any(listset)
    assertSame(listset, listseta)

    // Make sure SortedSets correctly rebuild themselves on widening with toSet
    // Covers TreeSet and keySet of SortedMap also
    val sortedsets = Seq(
      SortedSet.empty[Int], SortedSet(5), SortedSet(1,2,3,5,4),
      SortedMap(1 -> "cod", 2 -> "herring").keySet
    )
    sortedsets.foreach{ set => 
      val seta = any(set)
      assertNotSame(set, seta)
      assertEquals(set, seta)
    }

    // Make sure ValueSets correctly rebuild themselves on widening with toSet
    object WeekDay extends Enumeration {
      type WeekDay = Value
      val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
    }
    val valuesa = any(WeekDay.values)
    assertNotSame(WeekDay.values, valuesa)
    assertEquals(WeekDay.values, valuesa)

    // Make sure regular Map keySets do not rebuild themselves on widening with toSet
    val mapset = Map(1 -> "cod", 2 -> "herring").keySet
    val mapseta = any(mapset)
    assertSame(mapset, mapseta) // WIP see Set.from
  }

  @deprecated("Uses deprecated API", since="2.13")
  @Test
  def testRemoveAll(): Unit = {
    val s0 = Set(1, 2, 3) -- List(1, 2)
    assertEquals(Set(3), s0)

    val s1 = Set(1, 2, 3) -- List(1, 2, 3)
    assertEquals(Set(), s1)

    val s2 = Set(1, 2, 3) -- List(1, 2, 2, 3, 4)
    assertEquals(Set(), s2)

    // deprecated
    val s3 = collection.Set(1, 2, 3) -- List(2, 3)
    assertEquals(Set(1), s3)

    // deprecated
    val s4 = collection.mutable.Set(1, 2, 3) -- List(2, 3)
    assertEquals(Set(1), s4)
  }

  @Test
  def t7326(): Unit = {
    def testCorrectness(): Unit = {
      // a key that has many hashCode collisions
      case class Collision(i: Int) { override def hashCode = i / 5 }

      def subsetTest[T](emptyA:Set[T], emptyB:Set[T], mkKey:Int => T, n:Int): Unit = {
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

  @Test def pr10238(): Unit = {
    assertEquals(BitSet(0), BitSet(0, 128) diff BitSet(128))

    val us = scala.collection.immutable.BitSet(39, 41, 44, 46, 256)
    val vs = scala.collection.immutable.BitSet(39, 41, 44, 46, 64, 256)
    val xs = scala.collection.immutable.BitSet.fromBitMask(us.toBitMask.take(3))
    val ys = scala.collection.immutable.BitSet.fromBitMask(vs.toBitMask.take(3))
    val diff = ys diff xs
    val expected = scala.collection.immutable.BitSet(64)
    assertEquals(diff, expected)
  }
}

package scala.collection.immutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class SetTests {
  @Test
  def test_SI8346_toSet_soundness(): Unit = {
    val any2stringadd = "Disabled string conversions so as not to get confused!"
    
    def any[A](set: Set[A]): Set[Any] = {
      val anyset = set.toSet[Any]
      assert((anyset + "fish") contains "fish")
      anyset
    }

    // Make sure default immutable Set does not rebuild itself on widening with toSet
    // Need to cover 0, 1, 2, 3, 4 elements as special cases
    var si = Set.empty[Int]
    assert(si eq si.toSet[Any])
    for (i <- 1 to 5) {
      val s1 = Set(Array.range(1, i+1): _*)
      val s2 = si + i
      val s1a = any(s1)
      val s2a = any(s2)
      assert(s1 eq s1a)
      assert(s2 eq s2a)
      si = s2
    }

    // Make sure BitSet correctly rebuilds itself on widening with toSet
    // Need to cover empty, values 0-63, values 0-127 as special cases
    val bitsets = Seq(BitSet.empty, BitSet(23), BitSet(23, 99), BitSet(23, 99, 141))
    bitsets.foreach{ b =>
      val ba = any(b)
      assert(b ne ba)
      assertEquals(b, ba)
    }

    // Make sure HashSet (and by extension, its implementing class HashTrieSet)
    // does not rebuild itself on widening by toSet
    val hashset = HashSet(1, 3, 5, 7)
    val hashseta = any(hashset)
    assert(hashset eq hashseta)

    // Make sure ListSet does not rebuild itself on widening by toSet
    // (Covers Node also, since it subclasses ListSet)
    val listset = ListSet(1, 3, 5, 7)
    val listseta = any(listset)
    assert(listset eq listseta)

    // Make sure SortedSets correctly rebuild themselves on widening with toSet
    // Covers TreeSet and keySet of SortedMap also
    val sortedsets = Seq(
      SortedSet.empty[Int], SortedSet(5), SortedSet(1,2,3,5,4),
      SortedMap(1 -> "cod", 2 -> "herring").keySet
    )
    sortedsets.foreach{ set => 
      val seta = any(set)
      assert(set ne seta)
      assertEquals(set, seta)
    }

    // Make sure ValueSets correctly rebuild themselves on widening with toSet
    object WeekDay extends Enumeration {
      type WeekDay = Value
      val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
    }
    val valuesa = any(WeekDay.values)
    assert(WeekDay.values ne valuesa)
    assertEquals(WeekDay.values, valuesa)

    // Make sure regular Map keySets do not rebuild themselves on widening with toSet
    val mapset = Map(1 -> "cod", 2 -> "herring").keySet
    val mapseta = any(mapset)
    assert(mapset eq mapseta)
  }
}

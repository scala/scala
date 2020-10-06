package scala.collection.immutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AllocationTest
import scala.util.Random

@RunWith(classOf[JUnit4])
class HashSetTest extends AllocationTest {

  def generate(): HashSet[String] = {
    (1 to 1000).map { i => s"key $i" }(scala.collection.breakOut)
  }

  @Test
  def nonAllocatingEqualsIdentical(): Unit = {
    val base = generate()
    assertTrue(nonAllocating {
      base == base
    })
  }

  @Test
  def nonAllocatingEqualsNotShared(): Unit = {
    val base = generate()
    val notShared = generate()

    assertTrue(nonAllocating {
      base == notShared
    })
    assertTrue(nonAllocating {
      notShared == base
    })
  }

  @Test
  def nonAllocatingEqualsShared(): Unit = {
    val base = generate()
    val shared = (base - base.head) + base.head

    assertTrue(nonAllocating {
      base == shared
    })
    assertTrue(nonAllocating {
      shared == base
    })
  }

  @Test
  def nonAllocatingUnionIdentical(): Unit = {
    val base = generate()
    assertSame(base, nonAllocating {
      base union base
    })
  }

  @Test
  def nonAllocatingPlusPlusIdentical(): Unit = {
    val base = generate()
    assertSame(base, nonAllocating {
      base ++ base
    })
  }
  @Test
  def nonAllocatingUnionEqual(): Unit = {
    val base1 = generate()
    val base2 = generate()
    assertSame(base1, nonAllocating {
      base1 union base2
    })
  }

  @Test
  def nonAllocatingPlusPlusEqual(): Unit = {
    val base1 = generate()
    val base2 = generate()
    assertSame(base1, nonAllocating {
      base1 ++ base2
    })
  }
  @Test
  def nonAllocatingUnionEmpty(): Unit = {
    val base1 = generate()
    val base2 = HashSet.empty[String]
    assertSame(base1, nonAllocating {
      base1 union base2
    })
  }

  @Test
  def nonAllocatingPlusPlusEmpty(): Unit = {
    val base1 = generate()
    val base2 = HashSet.empty[String]
    assertSame(base1, nonAllocating {
      base1 ++ base2
    })
  }
  @Test
  def nonAllocatingUnionEmptySet(): Unit = {
    val base1 = generate()
    val base2 = Set.empty[String]
    assertSame(base1, nonAllocating {
      base1 union base2
    })
  }

  @Test
  def nonAllocatingPlusPlusEmptySet(): Unit = {
    val base1 = generate()
    val base2 = Set.empty[String]
    assertSame(base1, nonAllocating {
      base1 ++ base2
    })
  }
  @Test
  def nonAllocatingUnionSubsetShared(): Unit = {
    val base1 = generate()
    val base2 = base1 - base1.head
    assertSame(base1, nonAllocating {
      base1 union base2
    })
  }

  @Test
  def nonAllocatingPlusPlusSubsetShared(): Unit = {
    val base1 = generate()
    val base2 = base1 - base1.head
    assertSame(base1, nonAllocating {
      base1 ++ base2
    })
  }
  @Test
  def nonAllocatingUnionSubsetUnshared(): Unit = {
    val base1 = generate()
    val base2 = generate() - base1.head
    assertSame(base1, nonAllocating {
      base1 union base2
    })
  }

  @Test
  def nonAllocatingPlusPlusSubsetUnshared(): Unit = {
    val base1 = generate()
    val base2 = generate() - base1.head
    assertSame(base1, nonAllocating {
      base1 ++ base2
    })
  }
  @Test
  def nonAllocatingUnionSupersetShared(): Unit = {
    val base1 = generate()
    val base2 = base1 + "Mike"
    assertSame(base2, nonAllocating {
      base1 union base2
    })
  }

  @Test
  def nonAllocatingPlusPlusSupersetShared(): Unit = {
    val base1 = generate()
    val base2 = base1 + "Mike"
    assertSame(base2, nonAllocating {
      base1 ++ base2
    })
  }
  @Test
  def nonAllocatingUnionSupersetUnshared(): Unit = {
    val base1 = generate()
    val base2 = generate() + "Mike"
    assertSame(base2, nonAllocating {
      base1 union base2
    })
  }

  @Test
  def nonAllocatingPlusPlusSupersetUnshared(): Unit = {
    val base1 = generate()
    val base2 = generate() + "Mike"
    assertSame(base2, nonAllocating {
      base1 ++ base2
    })
  }

  def generateWithCollisions(start:Int, end:Int): HashSet[Colliding] = {
    (start to end).map { i => new Colliding(i/10, s"key $i") }(scala.collection.breakOut)
  }

  class Colliding(override val hashCode: Int, val other:String) {
    override def equals(obj: Any): Boolean = obj match {
      case that:Colliding => this.hashCode == that.hashCode && this.other == that.other
      case _ => false
    }

    override def toString: String = s"$hashCode-$other"
  }
  @Test def collidingAdd: Unit = {
    val initial = generateWithCollisions(1, 1000)
    assertEquals(1000, initial.size)
    assertEquals(1000, initial.toList.size)

    val more = generateWithCollisions(1, 1000)
    assertSame(initial, nonAllocating {
      initial ++ more
    })
    assertSame(more, nonAllocating {
      more ++ initial
    })

    val first = initial.head
    assertSame(initial, initial + first)
    assertSame(initial, initial + new Colliding(first.hashCode, first.other))

  }

  @Test
  def optimizedAppendAllWorks(): Unit = {
    case class C(i: Int) {
      override def hashCode: Int = i % 1024
    }
    val setReference = collection.mutable.HashSet[C]()
    val builder0 = collection.immutable.HashSet.newBuilder[C];
    for (i <- 1 to 16) {
      val builder1 = collection.immutable.HashSet.newBuilder[C];
      for (i <- 1 to 8)
        builder1 += C(Random.nextInt());
      val s1 = builder1.result()
      builder0 ++= s1
      setReference ++= s1
    }
    val set0 = builder0.result()
    assertEquals(set0, setReference)
  }

  @Test
  def optimizedBuilderHandlesEmptyHashSetInstance(): Unit = {
    val s = new scala.collection.immutable.HashSet[Int]
    //      ^--- constructed with new is important here so we don't get EmptyHashSet
    val b = scala.collection.immutable.HashSet.newBuilder[Int]
    b ++= List(1, 2, 3, 4)
    b ++= s // was scala.MatchError: Set() (of class scala.collection.immutable.HashSet)... at ... addToTrieHashSet(HashSet.scala:1386)
    assertEquals(List(1, 2, 3, 4), b.result().toList.sorted)
  }

  @Test
  def mixTrie(): Unit = {
    for (start <- 1 to 10) {
      val m1: Set[Int] = ((start + 1) to (start + 20)).toSet
      val m2: Set[Int] = ((start + 101) to (start + 120)).toSet

      val b = HashSet.newBuilder[Int]
      b ++= m1
      b ++= m2
      val res = b.result()
      assertEquals(40, res.size)
      for (i <- (start + 1) to (start + 20)) {
        assertTrue(res.contains(i))
        assertTrue(res.contains(i + 100))
      }
    }
  }
}

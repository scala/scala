package scala.collection.immutable

import org.junit.Test
import org.junit.Assert.assertEquals

import scala.collection.mutable

class SeqMapTest {
  private def checkClass(map: SeqMap[_, _], simpleName: String): Unit = {
    assertEquals(simpleName, map.getClass.getSimpleName.stripSuffix("$"))
  }

  @Test
  def applyFromSmallSizeSpecialization(): Unit = {
    checkClass(SeqMap(), "EmptySeqMap")
    checkClass(SeqMap(1 -> 1), "SeqMap1")
    checkClass(SeqMap(1 -> 1, 2 -> 2), "SeqMap2")
    checkClass(SeqMap(1 -> 1, 2 -> 2, 3 -> 3), "SeqMap3")
    checkClass(SeqMap(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4), "SeqMap4")
    checkClass(SeqMap(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5), "VectorMap")

    // no knownSize
    checkClass(SeqMap.from(List(1 -> 1)), "SeqMap1")
  }

  @Test
  def newBuilderSmallSizeSpecialization(): Unit = {
    type Builder = mutable.Builder[(Int, Int), SeqMap[Int, Int]]
    def build(op: Builder => Builder): SeqMap[Int, Int] =
      op(SeqMap.newBuilder[Int, Int]).result()

    checkClass(build(identity), "EmptySeqMap")
    checkClass(build(_ += 1 -> 1), "SeqMap1")
    checkClass(build(_ += 1 -> 1 += 2 -> 2), "SeqMap2")
    checkClass(build(_ += 1 -> 1 += 2 -> 2 += 3 -> 3), "SeqMap3")
    checkClass(build(_ += 1 -> 1 += 2 -> 2 += 3 -> 3 += 4 -> 4), "SeqMap4")
    checkClass(build(_ += 1 -> 1 += 2 -> 2 += 3 -> 3 += 4 -> 4 += 5 -> 5), "VectorMap")

    // `addAll`
    checkClass(build(_ ++= List(1 -> 1)), "SeqMap1")
  }

  @Test def `keys are seqs`: Unit = {
    val pairs = List.tabulate(6)(i => s"k$i" -> i)
    for (elems <- pairs.tails) {
      val vm = elems.to(VectorMap)
      val sm = elems.to(SeqMap)
      val keys = elems.map(_._1)
      assertEquals(keys, vm.keys)
      assertEquals(keys, sm.keys)
      assertEquals(vm.keys, sm.keys)
    }
  }
}

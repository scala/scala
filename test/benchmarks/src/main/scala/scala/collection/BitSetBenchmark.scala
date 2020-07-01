package scala.collection

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.collection.generic.BitSetFactory
import scala.collection.immutable.{List, Range}

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class BitSetBenchmark {
  @Param(Array("0", "1", "10", "100", "1000", "10000"))
  var bitsetSize: Int = _

  @Param(Array("true", "false"))
  var mutable: Boolean = _

  @Param(Array("empty", "half", "full", "spare-low", "spare-high"))
  var fill: String = _

  var bitset: BitSet = _
  var bitsetCopy: BitSet = _
  var high: BitSet = _
  var sameValuesArray: Array[Int] = _
  var sameValuesList: List[Int] = _

  var differentBitset: BitSet = _
  var differentArray: Array[Int] = _
  var differentList: List[Int] = _

  var overlapBitset: BitSet = _
  var overlapArray: Array[Int] = _
  var overlapList: List[Int] = _

  var bitsetCompanion: BitSetFactory[_ <: BitSet] = _

  @Setup(Level.Trial) def init(): Unit = {
    val b1 = new collection.mutable.BitSet
    if (bitsetSize > 0) {
      b1 += bitsetSize - 1
      b1 -= bitsetSize - 1
    }

    fill match {
      case "empty" =>
      case "half" => b1 ++= Range(0, bitsetSize, 2)
      case "full" => b1 ++= Range(0, bitsetSize)
      case "spare-low" =>
        assert(bitsetSize > 0) //i.e. abort that specific combination
        b1 += 0
      case "spare-high" =>
        assert(bitsetSize > 0) //i.e. abort that specific combination
        b1 += bitsetSize - 1
    }
    sameValuesArray = b1.toArray
    sameValuesList = b1.toList

    differentBitset = (BitSet.newBuilder ++= Range(bitsetSize, bitsetSize * 2)).result
    differentArray = differentBitset.toArray
    differentList = b1.toList

    overlapBitset = (BitSet.newBuilder ++= Range(bitsetSize, bitsetSize * 2)).result
    overlapArray = overlapBitset.toArray
    overlapList = overlapBitset.toList

    //add half
    b1.iterator.grouped(2).foreach{ half =>
      val head = half.head
      overlapBitset += half.head
      overlapArray :+= half.head
      overlapList  :+= half.head
    }

    if (mutable) {
      bitset = b1.clone()
      bitsetCopy = b1.clone()
      bitsetCompanion = scala.collection.mutable.BitSet
    } else {
      bitset = b1.toImmutable
      bitsetCopy = b1.toImmutable
      bitsetCompanion = scala.collection.immutable.BitSet
    }
    high = bitsetCompanion(bitsetSize)
  }


  @Benchmark def iterator() = {
    bitset.iterator
  }

  @Benchmark def iterate(bh: Blackhole): Unit = {
    val it = bitset.iterator
    while (it.hasNext) {
      bh.consume(it.next)
    }
  }

  @Benchmark def hashcode = {
    bitset.hashCode()
  }

  @Benchmark def equals = {
    bitset.equals(bitsetCopy)
  }

  @Benchmark def apply = {
    bitsetCompanion(sameValuesArray: _*)
  }

  @Benchmark def buildSingle = {
    (bitsetCompanion.newBuilder += 5).result
  }

  @Benchmark def buildFromList = {
    (bitsetCompanion.newBuilder ++= sameValuesList).result
  }

  @Benchmark def buildFromWrapped = {
    (bitsetCompanion.newBuilder ++= sameValuesArray).result
  }

  @Benchmark def buildFromBitset = {
    (bitsetCompanion.newBuilder ++= bitset).result
  }

  @Benchmark def isEmpty = {
    bitset.isEmpty
  }

  object all extends Function1[Int, Unit] {
    var result = 0

    override def apply(v1: Int): Unit = result += v1
  }

  @Benchmark def foreachFn = {
    bitset foreach all
    all.result
  }

  @Benchmark def forallFn =
    bitset forall (_ >= 0)

  @Benchmark def existsFn = {
    bitset exists (_ < 0)
  }

  @Benchmark def findFn = {
    bitset find (_ == bitsetSize)
  }

  @Benchmark def filterFn = {
    bitset filter (_ / 8 == 0)
  }

  @Benchmark def filterNotFn =
    bitset filterNot (_ / 8 != 0)

  @Benchmark def forallBitset =
    bitset forall bitsetCopy

  @Benchmark def existsBitset =
    bitset exists high

  @Benchmark def findBitset =
    bitset find high

  @Benchmark def filterBitset =
    bitset filter high

  @Benchmark def filterNotBitset =
    bitset filterNot high

  @Benchmark def plus = {
    bitset + 5
  }

  @Benchmark def minus =
    bitset - 5

  //same
  @Benchmark def plusPlusSameList =
    bitset ++ sameValuesList

  @Benchmark def plusPlusSameBitSet =
    bitset ++ bitsetCopy

  @Benchmark def minusMinusSameList =
    bitset -- sameValuesList

  @Benchmark def unionSame =
    bitset union bitsetCopy

  @Benchmark def intersectSame =
    bitset intersect bitsetCopy

  @Benchmark def diffSame =
    bitset intersect bitsetCopy

  //diff
  @Benchmark def plusPlusDifferentList =
    bitset ++ differentList

  @Benchmark def plusPlusDifferentBitSet =
    bitset ++ differentBitset

  @Benchmark def minusMinusDifferentList =
    bitset -- differentList

  @Benchmark def unionDifferent =
    bitset union differentBitset

  @Benchmark def intersectDifferent =
    bitset intersect differentBitset

  @Benchmark def diffDifferent =
    bitset intersect differentBitset

  //overlap
  @Benchmark def plusPlusOverlapList =
    bitset ++ overlapList

  @Benchmark def plusPlusOverlapBitSet =
    bitset ++ overlapBitset

  @Benchmark def minusMinusOverlapList =
    bitset -- overlapList

  @Benchmark def unionOverlap =
    bitset union overlapBitset

  @Benchmark def intersectOverlap =
    bitset intersect overlapBitset

  @Benchmark def diffOverlap =
    bitset intersect overlapBitset

  @Benchmark def toBitMask =
    bitset.toBitMask

  @Benchmark def size_ =
    bitset.size

  @Benchmark def range =
    bitset.range(1, 200)
}

//useful for testing
//object Test_BitSetBenchmark extends App {
//  val bm = new BitSetBenchmark
//  bm.bitsetSize = 1000
//  bm.fill = "half"
//  bm.mutable = false
//  bm.init()
//  var i = 0
//  var start = System.currentTimeMillis()
//  while (true) {
//    bm.hash
//    i += 1
//    if (i == 1000000) {
//      val now = System.currentTimeMillis()
//      println(s"${now - start}")
//      i = 0
//      start = now
//    }
//  }
//
//}

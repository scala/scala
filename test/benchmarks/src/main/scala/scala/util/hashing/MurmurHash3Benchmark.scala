package scala.util.hashing

import java.util.concurrent.TimeUnit
import scala.collection.immutable.ArraySeq

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class MurmurHash3Benchmark {

  @Param(Array("0", "1", "2", "3", "10", "100", "1000", "10000"))
  var size: Int = _
  var ordered: Array[Int] = _
  var mixed1: Array[Int] = _
  var mixed2: Array[Int] = _
  var orderedL: List[Int] = _
  var mixed1L: List[Int] = _
  var mixed2L: List[Int] = _
  var orderedI: IndexedSeq[Int] = _
  var mixed1I: IndexedSeq[Int] = _
  var mixed2I: IndexedSeq[Int] = _
  var range: Range = _

  @Setup(Level.Trial) def initNumbers: Unit = {
    range = (1 to size)
    ordered = Array.iterate(1, size)(_ + 1)
    mixed1 = Array.copyOf(ordered, ordered.length)
    mixed2 = Array.copyOf(ordered, ordered.length)
    if(size > 1) {
      swap(mixed1, 0, 1)
      swap(mixed2, mixed2.length-1, mixed2.length-2)
    }
    orderedL = ordered.toList
    mixed1L = mixed1.toList
    mixed2L = mixed2.toList
    orderedI = ArraySeq.from(ordered)
    mixed1I = ArraySeq.from(mixed1)
    mixed2I = ArraySeq.from(mixed2)
  }

  def swap(a: Array[Int], i1: Int, i2: Int): Unit = {
    val tmp = a(i1)
    a(i1) = a(i2)
    a(i2) = tmp
  }

  @Benchmark def rangeHash(bh: Blackhole): Unit =
    bh.consume(MurmurHash3.rangeHash(1, 1, size, MurmurHash3.seqSeed))


  @Benchmark def oldArrayHashOrdered(bh: Blackhole): Unit =
    bh.consume(OldMurmurHash3.oldArrayHash(ordered, MurmurHash3.seqSeed))

  @Benchmark def rangeOptimizedArrayHashOrdered(bh: Blackhole): Unit =
    bh.consume(MurmurHash3.arrayHash(ordered, MurmurHash3.seqSeed))

  @Benchmark def rangeOptimizedArrayHashMixed1(bh: Blackhole): Unit =
    bh.consume(MurmurHash3.arrayHash(mixed1, MurmurHash3.seqSeed))

  @Benchmark def rangeOptimizedArrayHashMixed2(bh: Blackhole): Unit =
    bh.consume(MurmurHash3.arrayHash(mixed2, MurmurHash3.seqSeed))

  @Benchmark def oldOrderedHashListOrdered(bh: Blackhole): Unit =
    bh.consume(OldMurmurHash3.oldOrderedHash(orderedL, MurmurHash3.seqSeed))

  @Benchmark def rangeOptimizedOrderedHashListOrdered(bh: Blackhole): Unit =
    bh.consume(MurmurHash3.orderedHash(orderedL, MurmurHash3.seqSeed))

  @Benchmark def rangeOptimizedOrderedHashListMixed1(bh: Blackhole): Unit =
    bh.consume(MurmurHash3.orderedHash(mixed1L, MurmurHash3.seqSeed))

  @Benchmark def rangeOptimizedOrderedHashListMixed2(bh: Blackhole): Unit =
    bh.consume(MurmurHash3.orderedHash(mixed2L, MurmurHash3.seqSeed))


  @Benchmark def oldOrderedHashIndexedOrdered(bh: Blackhole): Unit =
    bh.consume(OldMurmurHash3.oldOrderedHash(orderedI, MurmurHash3.seqSeed))

  @Benchmark def rangeOptimizedOrderedHashIndexedOrdered(bh: Blackhole): Unit =
    bh.consume(MurmurHash3.orderedHash(orderedI, MurmurHash3.seqSeed))

  @Benchmark def rangeOptimizedOrderedHashIndexedMixed1(bh: Blackhole): Unit =
    bh.consume(MurmurHash3.orderedHash(mixed1I, MurmurHash3.seqSeed))

  @Benchmark def rangeOptimizedOrderedHashIndexedMixed2(bh: Blackhole): Unit =
    bh.consume(MurmurHash3.orderedHash(mixed2I, MurmurHash3.seqSeed))


  @Benchmark def rangeOptimizedIndexedHashOrdered(bh: Blackhole): Unit =
    bh.consume(MurmurHash3.indexedSeqHash(orderedI, MurmurHash3.seqSeed))

  @Benchmark def rangeOptimizedIndexedHashMixed1(bh: Blackhole): Unit =
    bh.consume(MurmurHash3.indexedSeqHash(mixed1I, MurmurHash3.seqSeed))

  @Benchmark def rangeOptimizedIndexedHashMixed2(bh: Blackhole): Unit =
    bh.consume(MurmurHash3.indexedSeqHash(mixed2I, MurmurHash3.seqSeed))


  @Benchmark def oldListHashOrdered(bh: Blackhole): Unit =
    bh.consume(OldMurmurHash3.oldListHash(orderedL, MurmurHash3.seqSeed))

  @Benchmark def rangeOptimizedListHashOrdered(bh: Blackhole): Unit =
    bh.consume(MurmurHash3.listHash(orderedL, MurmurHash3.seqSeed))

  @Benchmark def rangeOptimizedListHashMixed1(bh: Blackhole): Unit =
    bh.consume(MurmurHash3.listHash(mixed1L, MurmurHash3.seqSeed))

  @Benchmark def rangeOptimizedListHashMixed2(bh: Blackhole): Unit =
    bh.consume(MurmurHash3.listHash(mixed2L, MurmurHash3.seqSeed))
}

object OldMurmurHash3 {
  import MurmurHash3._

  /** Compute a hash that depends on the order of its arguments.
    */
  final def oldOrderedHash(xs: IterableOnce[Any], seed: Int): Int = {
    var n = 0
    var h = seed
    xs.iterator foreach { x =>
      h = mix(h, x.##)
      n += 1
    }
    finalizeHash(h, n)
  }

  /** Compute the hash of an array.
    */
  final def oldArrayHash[@specialized T](a: Array[T], seed: Int): Int = {
    var h = seed
    var i = 0
    while (i < a.length) {
      h = mix(h, a(i).##)
      i += 1
    }
    finalizeHash(h, a.length)
  }

  final def oldListHash(xs: scala.collection.immutable.List[_], seed: Int): Int = {
    var n = 0
    var h = seed
    var elems = xs
    while (!elems.isEmpty) {
      val head = elems.head
      val tail = elems.tail
      h = mix(h, head.##)
      n += 1
      elems = tail
    }
    finalizeHash(h, n)
  }
}

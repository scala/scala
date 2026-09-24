package scala.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

/** Benchmarks for the InlineVector1..4 change: a matrix of sizes x operations.
  *
  * Sizes 1-4 use the inline representation (when built incrementally or via builder),
  * 5 is the smallest array-backed Vector1 under the new scheme, 1000 is a Vector2+,
  * 100000 is a Vector3+ (regression check for the extra branches added to
  * length/head/last/foreach/filter and the iterator constructor).
  */
@BenchmarkMode(Array(Mode.AverageTime))
@Fork(1)
@Threads(1)
@Warmup(iterations = 8, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 7, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class SmallVectorBenchmark {

  @Param(Array("1", "2", "3", "4", "5", "1000", "100000"))
  var size: Int = _

  var v: Vector[Integer] = _
  var elem: Integer = _
  var list: List[Integer] = _
  var arraySeq: ArraySeq[Integer] = _
  var midIdx: Int = _

  private[this] var sink: Long = 0

  private[this] val inc: Integer => Integer = x => Integer.valueOf(x.intValue + 1)
  private[this] val add: (Int, Integer) => Int = (acc, x) => acc + x.intValue
  private[this] val isEven: Integer => Boolean = x => (x.intValue & 1) == 0
  private[this] val never: Integer => Boolean = x => x.intValue == Int.MinValue
  private[this] val consumeF: Integer => Unit = x => sink += x.intValue

  @Setup(Level.Trial) def setup(): Unit = {
    // build incrementally so small sizes actually use the representation produced by :+
    var b: Vector[Integer] = Vector.empty
    var i = 0
    while (i < size) { b = b :+ Integer.valueOf(i); i += 1 }
    v = b
    elem = Integer.valueOf(-1)
    list = List.tabulate(size)(Integer.valueOf(_))
    arraySeq = ArraySeq.tabulate(size)(Integer.valueOf(_))
    midIdx = size / 2
  }

  // --- reads ---

  @Benchmark def length: Int = v.length
  @Benchmark def head: Integer = v.head
  @Benchmark def last: Integer = v.last
  @Benchmark def applyMid: Integer = v(midIdx)

  // --- structural ops ---

  @Benchmark def tail: Vector[Integer] = v.tail
  @Benchmark def init: Vector[Integer] = v.init
  @Benchmark def appended: Vector[Integer] = v :+ elem
  @Benchmark def prepended: Vector[Integer] = elem +: v
  @Benchmark def updatedMid: Vector[Integer] = v.updated(midIdx, elem)
  @Benchmark def concatSelf: Vector[Integer] = v ++ v

  // --- traversals ---

  @Benchmark def map: Vector[Integer] = v.map(inc)
  @Benchmark def foldLeftSum: Int = v.foldLeft(0)(add)
  @Benchmark def foreachSum: Long = { sink = 0; v.foreach(consumeF); sink }
  @Benchmark def iteratorSum: Int = {
    var s = 0
    val it = v.iterator
    while (it.hasNext) s += it.next().intValue
    s
  }
  @Benchmark def filterHalf: Vector[Integer] = v.filter(isEven)
  @Benchmark def existsNotFound: Boolean = v.exists(never)

  // --- construction ---

  @Benchmark def fromArraySeq: Vector[Integer] = Vector.from(arraySeq)
  @Benchmark def fromList: Vector[Integer] = list.toVector
  @Benchmark def buildByAppend: Vector[Integer] = {
    var b: Vector[Integer] = Vector.empty
    var i = 0
    while (i < size) { b = b :+ elem; i += 1 }
    b
  }
  @Benchmark def drainByTail(bh: Blackhole): Unit = {
    var x = v
    while (x.nonEmpty) x = x.tail
    bh.consume(x)
  }
}

/** Call sites that see a mix of small-vector sizes: measures the cost of the
  * larger class polymorphism (Vector0 + 4 inline classes + Vector1 vs
  * Vector0 + Vector1 before the change).
  */
@BenchmarkMode(Array(Mode.AverageTime))
@Fork(1)
@Threads(1)
@Warmup(iterations = 8, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 7, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class SmallVectorMixedBenchmark {

  // 1024 vectors with sizes cycling through 0..6 (mix of representations)
  var vs: Array[Vector[Integer]] = _
  private[this] val inc: Integer => Integer = x => Integer.valueOf(x.intValue + 1)

  @Setup(Level.Trial) def setup(): Unit = {
    vs = Array.tabulate(1024) { i =>
      val n = i % 7
      var b: Vector[Integer] = Vector.empty
      var j = 0
      while (j < n) { b = b :+ Integer.valueOf(j); j += 1 }
      b
    }
  }

  @Benchmark def lengthSum: Int = {
    var s = 0
    var i = 0
    while (i < vs.length) { s += vs(i).length; i += 1 }
    s
  }

  @Benchmark def headSumNonEmpty: Int = {
    var s = 0
    var i = 0
    while (i < vs.length) {
      val v = vs(i)
      if (v.nonEmpty) s += v.head.intValue
      i += 1
    }
    s
  }

  @Benchmark def applyZeroSum: Int = {
    var s = 0
    var i = 0
    while (i < vs.length) {
      val v = vs(i)
      if (v.nonEmpty) s += v(0).intValue
      i += 1
    }
    s
  }

  @Benchmark def appendedAll(bh: Blackhole): Unit = {
    var i = 0
    while (i < vs.length) { bh.consume(vs(i) :+ i); i += 1 }
  }

  @Benchmark def mapAll(bh: Blackhole): Unit = {
    var i = 0
    while (i < vs.length) { bh.consume(vs(i).map(inc)); i += 1 }
  }

  @Benchmark def iterateAll: Int = {
    var s = 0
    var i = 0
    while (i < vs.length) {
      val it = vs(i).iterator
      while (it.hasNext) s += it.next().intValue
      i += 1
    }
    s
  }
}

/** Allocation-churn benchmarks, intended to be run with `-prof gc` and different
  * heap sizes (e.g. -Xmx4g low pressure vs -Xmx64m high pressure).
  * `retained` vectors keep a slice of the allocations alive so that some churn
  * is promoted out of the young generation.
  */
@BenchmarkMode(Array(Mode.AverageTime))
@Fork(1)
@Threads(1)
@Warmup(iterations = 6, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 6, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class SmallVectorChurnBenchmark {

  @Param(Array("4", "100000"))
  var size: Int = _

  var v: Vector[Integer] = _
  var elem: Integer = _
  var retained: Array[Vector[Integer]] = _
  private[this] var rIdx = 0
  private[this] val inc: Integer => Integer = x => Integer.valueOf(x.intValue + 1)

  @Setup(Level.Trial) def setup(): Unit = {
    var b: Vector[Integer] = Vector.empty
    var i = 0
    while (i < size) { b = b :+ Integer.valueOf(i); i += 1 }
    v = b
    elem = Integer.valueOf(-1)
    // retain up to ~8k small vectors / ~16 large ones to create old-gen pressure
    retained = new Array[Vector[Integer]](if (size <= 64) 8192 else 16)
  }

  @Benchmark def mapChurnRetained: Vector[Integer] = {
    val r = v.map(inc)
    retained(rIdx) = r
    rIdx += 1
    if (rIdx == retained.length) rIdx = 0
    r
  }

  @Benchmark def appendTailChurn: Vector[Integer] = {
    val r = (v :+ elem).tail
    retained(rIdx) = r
    rIdx += 1
    if (rIdx == retained.length) rIdx = 0
    r
  }
}

/** Dissects the mixed-dispatch cost: is it the number of receiver classes or the number of
  * distinct method implementations? Same call sites, different class mixes.
  * - m1:   size 2 only            -> 1 class  (InlineVector2), monomorphic
  * - m2i:  sizes 1,2              -> 2 inline classes, one shared implementation
  * - m4i:  sizes 1,2,3,4          -> 4 inline classes, one shared implementation
  * - m2x:  sizes 2,5              -> 2 classes (InlineVector2, Vector1), two implementations
  * - m5x:  sizes 1,2,3,4,5        -> 5 classes, two implementations
  * - mv:   sizes 5,6              -> 1 class (Vector1), monomorphic array-backed
  */
@BenchmarkMode(Array(Mode.AverageTime))
@Fork(1)
@Threads(1)
@Warmup(iterations = 8, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 7, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class SmallVectorDispatchBenchmark {

  @Param(Array("m1", "m2i", "m4i", "m2x", "m5x", "mv"))
  var mix: String = _

  var vs: Array[Vector[Integer]] = _

  @Setup(Level.Trial) def setup(): Unit = {
    val sizes: Array[Int] = mix match {
      case "m1"  => Array(2)
      case "m2i" => Array(1, 2)
      case "m4i" => Array(1, 2, 3, 4)
      case "m2x" => Array(2, 5)
      case "m5x" => Array(1, 2, 3, 4, 5)
      case "mv"  => Array(5, 6)
    }
    vs = Array.tabulate(1024) { i =>
      val n = sizes(i % sizes.length)
      var b: Vector[Integer] = Vector.empty
      var j = 0
      while (j < n) { b = b :+ Integer.valueOf(j); j += 1 }
      b
    }
  }

  @Benchmark def lengthSum: Int = {
    var s = 0
    var i = 0
    while (i < vs.length) { s += vs(i).length; i += 1 }
    s
  }

  @Benchmark def headSum: Int = {
    var s = 0
    var i = 0
    while (i < vs.length) { s += vs(i).head.intValue; i += 1 }
    s
  }

  @Benchmark def applyZeroSum: Int = {
    var s = 0
    var i = 0
    while (i < vs.length) { s += vs(i)(0).intValue; i += 1 }
    s
  }

  @Benchmark def iterateAll: Int = {
    var s = 0
    var i = 0
    while (i < vs.length) {
      val it = vs(i).iterator
      while (it.hasNext) s += it.next().intValue
      i += 1
    }
    s
  }
}

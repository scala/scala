package scala.collection.mutable

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._
import org.openjdk.jmh.runner.IterationType
import benchmark._
import java.util.concurrent.TimeUnit
import java.util.{ HashMap => JHashMap }

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class CollisionProofHashMapBenchmark {
  @Param(Array(/*"0", "1", "10",*/ "100", "1000", "10000"))
  var size: Int = _

  class Collider(val x: String, val h: Int) extends Comparable[Collider] {
    override def hashCode: Int = h
    override def equals(o: Any): Boolean = o match {
      case o: Collider => x == o.x
      case _ => false
    }
    def compareTo(o: Collider): Int = x.compareTo(o.x)
  }

  implicit val colliderOrdering: Ordering[Collider] = Ordering.String.on[Collider](_.x)

  var existingKeys: Array[String] = _
  var existingKVs: ArrayBuffer[(String, String)] = _
  var missingKeys: Array[String] = _
  var oa1: CollisionProofHashMap[String, String] = _
  var m1: HashMap[String, String] = _
  var j1: JHashMap[String, String] = new JHashMap[String, String]
  var colliders: Array[Collider] = _
  var dos: Array[Collider] = _

  @Setup(Level.Trial) def init: Unit = {
    existingKeys = (0 until size).map(_.toString).toArray
    existingKVs = ArrayBuffer.from(existingKeys.iterator.map(k => (k, k)))
    missingKeys = (size until (2 * size.max(100))).toArray.map(_.toString)
    oa1 = CollisionProofHashMap.from(existingKVs)
    m1 = HashMap.from(existingKVs)
    m1.foreach { case (k, v) => j1.put(k, v) }
    colliders = existingKeys.map(k => new Collider(k, k.hashCode & 0x1111))
    dos = existingKeys.map(k => new Collider(k, 42))
  }

  @Benchmark def oaFillRegular(bh: Blackhole): Unit = {
    val h = new CollisionProofHashMap[String, String]
    existingKeys.foreach(k => h.put(k, k))
    bh.consume(h)
  }

  @Benchmark def oaFillColliding(bh: Blackhole): Unit = {
    val h = new CollisionProofHashMap[Collider, Collider]
    colliders.foreach(k => h.put(k, k))
    bh.consume(h)
  }

  @Benchmark def oaFillDoS(bh: Blackhole): Unit = {
    val h = new CollisionProofHashMap[Collider, Collider]
    dos.foreach(k => h.put(k, k))
    bh.consume(h)
  }

  @Benchmark def oaBuild(bh: Blackhole): Unit =
    bh.consume(CollisionProofHashMap.from(existingKVs))

  @Benchmark def oaIterateKeys(bh: Blackhole): Unit = {
    val it = oa1.keysIterator
    while(it.hasNext) bh.consume(it.next())
  }

  @Benchmark def oaIterateEntries(bh: Blackhole): Unit = {
    val it = oa1.iterator
    while(it.hasNext) bh.consume(it.next())
  }

  @Benchmark def oaGetExisting(bh: Blackhole): Unit = {
    var i = 0
    while (i < size) {
      bh.consume(oa1.apply(existingKeys(i)))
      i += 1
    }
  }

  @Benchmark def oaGetNone(bh: Blackhole): Unit = {
    var i = 0
    while (i < size.max(100)) {
      bh.consume(oa1.get(missingKeys(i)))
      i += 1
    }
  }

  @Benchmark def oaClearRemove(bh: Blackhole): Unit = {
    val m = CollisionProofHashMap.from(oa1)
    val it = oa1.keysIterator
    while(it.hasNext) m.remove(it.next())
    assert(m.isEmpty)
    bh.consume(m)
  }

  @Benchmark def oaClearSubtractOne(bh: Blackhole): Unit = {
    val m = CollisionProofHashMap.from(oa1)
    val it = oa1.keysIterator
    while(it.hasNext) m.subtractOne(it.next())
    assert(m.isEmpty)
    bh.consume(m)
  }

  @Benchmark def hmFillRegular(bh: Blackhole): Unit = {
    val h = new HashMap[Any, Any]
    existingKeys.foreach(k => h.put(k, k))
    bh.consume(h)
  }

  @Benchmark def hmFillColliding(bh: Blackhole): Unit = {
    val h = new HashMap[Any, Any]
    colliders.foreach(k => h.put(k, k))
    bh.consume(h)
  }

  @Benchmark def hmBuild(bh: Blackhole): Unit =
    bh.consume(HashMap.from(existingKVs))

  @Benchmark def hmIterateKeys(bh: Blackhole): Unit = {
    val it = m1.keysIterator
    while(it.hasNext) bh.consume(it.next())
  }

  @Benchmark def hmIterateEntries(bh: Blackhole): Unit = {
    val it = m1.iterator
    while(it.hasNext) bh.consume(it.next())
  }

  @Benchmark def hmGetExisting(bh: Blackhole): Unit = {
    var i = 0
    while (i < size) {
      bh.consume(m1.apply(existingKeys(i)))
      i += 1
    }
  }

  @Benchmark def hmGetNone(bh: Blackhole): Unit = {
    var i = 0
    while (i < size.max(100)) {
      bh.consume(m1.get(missingKeys(i)))
      i += 1
    }
  }

  @Benchmark def javaFillRegular(bh: Blackhole): Unit = {
    val h = new JHashMap[Any, Any]
    existingKeys.foreach(k => h.put(k, k))
    bh.consume(h)
  }

  @Benchmark def javaFillColliding(bh: Blackhole): Unit = {
    val h = new JHashMap[Any, Any]
    colliders.foreach(k => h.put(k, k))
    bh.consume(h)
  }

  @Benchmark def javaFillDoS(bh: Blackhole): Unit = {
    val h = new JHashMap[Any, Any]
    dos.foreach(k => h.put(k, k))
    bh.consume(h)
  }

  @Benchmark def javaBuild(bh: Blackhole): Unit = {
    val h = new JHashMap[Any, Any](((existingKeys.length+1).toDouble/0.75).toInt, 0.75f)
    existingKeys.foreach(k => h.put(k, k))
    bh.consume(h)
  }

  @Benchmark def javaIterateKeys(bh: Blackhole): Unit = {
    val it = j1.keySet().iterator()
    while(it.hasNext) bh.consume(it.next())
  }

  @Benchmark def javaIterateEntries(bh: Blackhole): Unit = {
    val it = j1.entrySet().iterator()
    while(it.hasNext) bh.consume(it.next())
  }

  @Benchmark def javaGetExisting(bh: Blackhole): Unit = {
    var i = 0
    while (i < size) {
      bh.consume(j1.get(existingKeys(i)))
      i += 1
    }
  }

  @Benchmark def javaGetNone(bh: Blackhole): Unit = {
    var i = 0
    while (i < size.max(100)) {
      bh.consume(j1.get(missingKeys(i)))
      i += 1
    }
  }
}

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
@Warmup(iterations = 20)
@Measurement(iterations = 20)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class HashMapBenchmark2 {
  @Param(Array(/*"0", "1",*/ "10", "100", "1000", "10000"))
  var size: Int = _
  @Param(Array("true"))
  var stringsOnly = false

  class Collider(val x: Any, val h: Int) {
    override def hashCode: Int = h
    override def equals(o: Any): Boolean = o match {
      case o: Collider => x == o.x
      case _ => false
    }
  }

  var existingKeys: Array[Any] = _
  var existingKVs: ArrayBuffer[(Any, Any)] = _
  var missingKeys: Array[Any] = _
  var s1: HashSet[Any] = _
  var m1: HashMap[Any, Any] = _
  var j1: JHashMap[Any, Any] = new JHashMap[Any, Any]
  var colliders: Array[Collider] = _

  @Setup(Level.Trial) def init: Unit = {
    existingKeys = (0 until size).map(i => (i % 4) match {
      case _ if stringsOnly => i.toString
      case 0 => i.toString
      case 1 => i.toChar
      case 2 => i.toDouble
      case 3 => i.toInt
    }).toArray
    existingKVs = ArrayBuffer.from(existingKeys.iterator.map(k => (k, k)))
    missingKeys = (size until (2 * size.max(100))).toArray.map(_.toString)
    s1 = HashSet.from(existingKeys)
    m1 = HashMap.from(existingKVs)
    m1.foreach { case (k, v) => j1.put(k, v) }
    colliders = existingKeys.map(k => new Collider(k, k.hashCode & 0x1111))
  }

  @Benchmark def hsFillRegular(bh: Blackhole): Unit = {
    val h = new HashSet[Any]
    existingKeys.foreach(k => h.addOne(k))
    bh.consume(h)
  }

  @Benchmark def hsFillColliding(bh: Blackhole): Unit = {
    val h = new HashSet[Any]
    colliders.foreach(k => h.addOne(k))
    bh.consume(h)
  }

  @Benchmark def hsBuild(bh: Blackhole): Unit =
    bh.consume(HashSet.from(existingKeys))

  @Benchmark def hsIterate(bh: Blackhole): Unit = {
    val it = s1.iterator
    while(it.hasNext) bh.consume(it.next())
  }

  @Benchmark def hsContainsTrue(bh: Blackhole): Unit = {
    var i = 0
    while (i < size) {
      bh.consume(s1.contains(existingKeys(i)))
      i += 1
    }
  }

  @Benchmark def hsContainsFalse(bh: Blackhole): Unit = {
    var i = 0
    while (i < size.max(100)) {
      bh.consume(s1.contains(missingKeys(i)))
      i += 1
    }
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

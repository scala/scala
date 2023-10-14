package scala.collection.mutable

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._
import org.openjdk.jmh.runner.IterationType
import benchmark._
import java.util.concurrent.TimeUnit
import java.util.{ LinkedHashMap => JLHashMap, LinkedHashSet => JLHashSet }

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 20)
@Measurement(iterations = 20)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class LinkedHashMapBenchmark2 {
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
  var s1: LinkedHashSet[Any] = _
  var m1: LinkedHashMap[Any, Any] = _
  var j1: JLHashMap[Any, Any] = new JLHashMap[Any, Any]
  var j2: JLHashSet[Any] = new JLHashSet[Any]
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
    s1 = LinkedHashSet.from(existingKeys)
    m1 = LinkedHashMap.from(existingKVs)
    m1.foreach { case (k, v) => j1.put(k, v) }
    s1.foreach({case k => j2.add(k)})
    colliders = existingKeys.map(k => new Collider(k, k.hashCode & 0x1111))
  }

  @Benchmark def lhsFillRegular(bh: Blackhole): Unit = {
    val h = new LinkedHashSet[Any]
    existingKeys.foreach(k => h.addOne(k))
    bh.consume(h)
  }

  @Benchmark def lhsFillColliding(bh: Blackhole): Unit = {
    val h = new LinkedHashSet[Any]
    colliders.foreach(k => h.addOne(k))
    bh.consume(h)
  }

  @Benchmark def lhsBuild(bh: Blackhole): Unit =
    bh.consume(LinkedHashSet.from(existingKeys))

  @Benchmark def lhsIterate(bh: Blackhole): Unit = {
    val it = s1.iterator
    while(it.hasNext) bh.consume(it.next())
  }

  @Benchmark def lhsContainsTrue(bh: Blackhole): Unit = {
    var i = 0
    while (i < size) {
      bh.consume(s1.contains(existingKeys(i)))
      i += 1
    }
  }

  @Benchmark def lhsContainsFalse(bh: Blackhole): Unit = {
    var i = 0
    while (i < size.max(100)) {
      bh.consume(s1.contains(missingKeys(i)))
      i += 1
    }
  }

  @Benchmark def lhmFillRegular(bh: Blackhole): Unit = {
    val h = new LinkedHashMap[Any, Any]
    existingKeys.foreach(k => h.put(k, k))
    bh.consume(h)
  }

  @Benchmark def lhmFillColliding(bh: Blackhole): Unit = {
    val h = new LinkedHashMap[Any, Any]
    colliders.foreach(k => h.put(k, k))
    bh.consume(h)
  }

  @Benchmark def lhmBuild(bh: Blackhole): Unit =
    bh.consume(LinkedHashMap.from(existingKVs))

  @Benchmark def lhmIterateKeys(bh: Blackhole): Unit = {
    val it = m1.keysIterator
    while(it.hasNext) bh.consume(it.next())
  }

  @Benchmark def lhmIterateEntries(bh: Blackhole): Unit = {
    val it = m1.iterator
    while(it.hasNext) bh.consume(it.next())
  }

  @Benchmark def lhmGetExisting(bh: Blackhole): Unit = {
    var i = 0
    while (i < size) {
      bh.consume(m1.apply(existingKeys(i)))
      i += 1
    }
  }

  @Benchmark def lhmGetNone(bh: Blackhole): Unit = {
    var i = 0
    while (i < size.max(100)) {
      bh.consume(m1.get(missingKeys(i)))
      i += 1
    }
  }

  @Benchmark def javalhmFillRegular(bh: Blackhole): Unit = {
    val h = new JLHashMap[Any, Any]
    existingKeys.foreach(k => h.put(k, k))
    bh.consume(h)
  }

  @Benchmark def javalhmFillColliding(bh: Blackhole): Unit = {
    val h = new JLHashMap[Any, Any]
    colliders.foreach(k => h.put(k, k))
    bh.consume(h)
  }

  @Benchmark def javalhmBuild(bh: Blackhole): Unit = {
    val h = new JLHashMap[Any, Any](((existingKeys.length+1).toDouble/0.75).toInt, 0.75f)
    existingKeys.foreach(k => h.put(k, k))
    bh.consume(h)
  }

  @Benchmark def javalhmIterateKeys(bh: Blackhole): Unit = {
    val it = j1.keySet().iterator()
    while(it.hasNext) bh.consume(it.next())
  }

  @Benchmark def javalhmIterateEntries(bh: Blackhole): Unit = {
    val it = j1.entrySet().iterator()
    while(it.hasNext) bh.consume(it.next())
  }

  @Benchmark def javalhmGetExisting(bh: Blackhole): Unit = {
    var i = 0
    while (i < size) {
      bh.consume(j1.get(existingKeys(i)))
      i += 1
    }
  }

  @Benchmark def javalhmGetNone(bh: Blackhole): Unit = {
    var i = 0
    while (i < size.max(100)) {
      bh.consume(j1.get(missingKeys(i)))
      i += 1
    }
  }
    @Benchmark def javalhsFillRegular(bh: Blackhole): Unit = {
      val h = new JLHashSet[Any]
      existingKeys.foreach(k => h.add(k))
      bh.consume(h)
    }

    @Benchmark def javalhsFillColliding(bh: Blackhole): Unit = {
      val h = new JLHashSet[Any]
      colliders.foreach(k => h.add(k))
      bh.consume(h)
    }

    @Benchmark def javalhsBuild(bh: Blackhole): Unit = {
      val h = new JLHashSet[Any](((existingKeys.length+1).toDouble/0.75).toInt, 0.75f)
      existingKeys.foreach(k => h.add(k))
      bh.consume(h)
    }

    @Benchmark def javalhsIterate(bh: Blackhole): Unit = {
      val it = j2.iterator()
      while(it.hasNext) bh.consume(it.next())
    }


    @Benchmark def javalhsContainsTrue(bh: Blackhole): Unit = {
      var i = 0
      while (i < size) {
        bh.consume(j2.contains(existingKeys(i)))
        i += 1
      }
    }

    @Benchmark def javalhsContainsFalse(bh: Blackhole): Unit = {
      var i = 0
      while (i < size.max(100)) {
        bh.consume(j2.contains(missingKeys(i)))
        i += 1
      }
    }
}

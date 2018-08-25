package scala.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._

import scala.collection.mutable

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class SetBenchmark {
  @Param(Array("10", "100", "1000", "10000"))
  var size: Int = _

//  var base: Set[String] = _

  var existingKeys: Array[Any] = _
  var missingKeys: Array[Any] = _
  var allSameHash: Array[Any] = _
  var set1: collection.immutable.Set[Any] = _
  var set2: collection.immutable.Set[Any] = _

  @Setup(Level.Trial) def initKeys(): Unit = {
//    base = Set("a", "b", "c", "d")

    existingKeys = (0 to size)
      .map(i =>
        (i % 4) match {
          case 0 => i.toString
          case 1 => i.toChar
          case 2 => i.toDouble
          case 3 => i.toInt
      })
      .toArray
//    missingKeys = (size to 2 * size).toArray.map(_.toString)

    allSameHash = (1 to size)
      .map(_ =>
        new AnyRef {
          override def hashCode() = 12345
      })
      .toArray

  }

  // immutable map is implemented as EmptySet -> Set1 -> Set2 -> Set3 -> Set4 -> HashSet
  // add an extra entry to Set4 causes a lot of work, benchmark the transition
  //  @Benchmark def set4AddElement(bh: Blackhole): Unit = {
  //    bh.consume(base + "e")
  //  }
//
//  @Setup(Level.Trial) def initialize = {
//    set1 = existingKeys.toSet
//    set2 = (existingKeys.splitAt(10)._1 ++ missingKeys).toSet
//  }

  @Benchmark def hashsetBuilder_allNewElems(bh: Blackhole): Unit = {
    val b = HashSet.newBuilder[Any]
    b ++= existingKeys
    bh.consume(b.result())
  }
  @Benchmark def hashsetBuilder_allElemsAndThenRepeat(bh: Blackhole): Unit = {
    val b = HashSet.newBuilder[Any]
    b ++= existingKeys
    b ++= existingKeys
    bh.consume(b.result())
  }
  @Benchmark def hashsetBuilder_allSameHash(bh: Blackhole): Unit = {
    val b = HashSet.newBuilder[Any]
    b ++= allSameHash
    bh.consume(b.result())
  }

  def newImmutableBuilder = new mutable.ImmutableBuilder(Set.empty[Any]) {
    override def addOne(elem: Any) = { elems += elem; this }
  }

  @Benchmark def immutableBuilder_allNewElems(bh: Blackhole): Unit = {
    val b = newImmutableBuilder
    b ++= existingKeys
    bh.consume(b.result())
  }
  @Benchmark def immutableBuilder_allElemsAndThenRepeat(bh: Blackhole): Unit = {
    val b = newImmutableBuilder
    b ++= existingKeys
    b ++= existingKeys
    bh.consume(b.result())
  }
  @Benchmark def immutableBuilder_allSameHash(bh: Blackhole): Unit = {
    val b = newImmutableBuilder
    b ++= allSameHash
    bh.consume(b.result())
  }


  @Benchmark def smartBuilder_allNewElems(bh: Blackhole): Unit = {
    val b = new SetBuilderImpl[Any]
    b ++= existingKeys
    bh.consume(b.result())
  }
  @Benchmark def smartBuilder_allElemsAndThenRepeat(bh: Blackhole): Unit = {
    val b = new SetBuilderImpl[Any]
    b ++= existingKeys
    b ++= existingKeys
    bh.consume(b.result())
  }
  @Benchmark def smartBuilder_allSameHash(bh: Blackhole): Unit = {
    val b = new SetBuilderImpl[Any]
    b ++= allSameHash
    bh.consume(b.result())
  }


}

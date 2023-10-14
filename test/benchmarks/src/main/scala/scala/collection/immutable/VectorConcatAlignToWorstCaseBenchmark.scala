package scala.collection.immutable

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(1)
@Threads(1)
@Warmup(iterations = 4)
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class VectorConcatAlignToWorstCaseBenchmark {
  @Param(Array("1", "15", "31", "33", "63", "65", "127", "255", "513", "1023", "1025", "2047")) // should not be divisible by 32
  var size: Int = _
  @Param(Array("1", "32", "64", "128", "256"))
  var sizeDifference: Int = _

  val o = new AnyRef

  var shorter: Vector[String] = _
  var longer: Vector[String] = _

  @Setup(Level.Trial) def init(): Unit = {
    shorter = Vector.fill(size)("s")
    longer = Vector.fill(size + sizeDifference)("l")
  }

  @Benchmark def withoutAlignTo(bh: Blackhole): Any =
    bh.consume(new VectorBuilder[String]().addAll(shorter).addAll(longer).result())

  @Benchmark def withAlignTo(bh: Blackhole): Any =
    bh.consume(new VectorBuilder[String]().alignTo(shorter.length, longer).addAll(shorter).addAll(longer).result())

  @Benchmark def concat(bh: Blackhole): Any =
    bh.consume(shorter ++ longer)
}

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
class VectorConcatBigVectorsBenchmark {
  val size: Int = 1000000

  @Param(Array("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
  var tenthsSplit: Int = _
  val o = new AnyRef

  var l: Vector[AnyRef] = _
  var lShifted: Vector[AnyRef] = _
  var r: Vector[AnyRef] = _
  var rNew: Vector[AnyRef] = _

  @Setup(Level.Trial) def init(): Unit = {
    val split = size * tenthsSplit / 10

    val (a, b) = Vector.fillSparse(size)(o).splitAt(split)
    l = a; r = b
    rNew = Vector.fillSparse(size - split)(o)
    lShifted = Vector.fillSparse(split + 5)(o).drop(5)
  }

  @Benchmark def concatAligned(bh: Blackhole): Any =
    bh.consume(l ++ r)

  @Benchmark def concatSemiAligned(bh: Blackhole): Any =
    bh.consume(l ++ rNew)

  @Benchmark def concatMisaligned(bh: Blackhole): Any =
    bh.consume(lShifted ++ r)

}

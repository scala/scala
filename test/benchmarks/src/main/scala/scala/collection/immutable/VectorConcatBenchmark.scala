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
class VectorConcatBenchmark {
  @Param(Array("30", "32", "1000", "1024", "30000", "32000", "30720", "32768", "1048576", "33554432"))
  var size: Int = _

  val o = new AnyRef

  var vAligned: Vector[AnyRef] = _
  var vShifted: Vector[AnyRef] = _

  @Setup(Level.Trial) def init: Unit = {
    vAligned = Vector.fillSparse(size)(o)
    vShifted = Vector.fillSparse(size + 5)(o).drop(5)
  }

  def concat(bh: Blackhole, a: Vector[AnyRef], b: Vector[AnyRef], times: Int = 10): Any = {
    var coll = a
    val coll1 = b
    var i = 0
    while(i < 10) {
      coll = coll.appendedAll(coll1)
      i += 1
    }
    bh.consume(coll)
  }

  @Benchmark def concatAlignedAligned(bh: Blackhole): Any =
    concat(bh, vAligned, vAligned)

  @Benchmark def concatAlignedShifted(bh: Blackhole): Any =
    concat(bh, vShifted, vShifted)


  @Benchmark def concatMisalignedAligned(bh: Blackhole): Any =
    concat(bh, vAligned, vShifted)

  @Benchmark def concatMisalignedShifted(bh: Blackhole): Any =
    concat(bh, vShifted, vAligned)
}

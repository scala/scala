package scala.util.hashing

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole


object MurmurHash3Benchmark {
  case class Content(value: Int)
}

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class MurmurHash3Benchmark {
  import MurmurHash3Benchmark._

  @Param(Array("0", "1", "10", "100", "1000"))
  var size: Int = _

  var values: List[Content] = _
  var seed: Content = _

  @Setup(Level.Trial) def initKeys(): Unit = {
    values = List.tabulate(size)(v => Content(v))
    seed = Content(size / 2)
  }

  @Benchmark def call_listHash : Any = {
    val murmur = new MurmurHash3
    murmur.listHash(values.map(_.value), seed.value)
  }
}



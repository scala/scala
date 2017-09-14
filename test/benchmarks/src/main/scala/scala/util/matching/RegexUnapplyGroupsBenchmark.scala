package scala.util.matching

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class RegexUnapplyGroupsBenchmark {

  @Param(Array("1", "10", "100"))
  var groupCount: Int = _
  var groupCorpus: String = _
  var groupPattern: Regex = _

  @Setup(Level.Trial) def initKeys(): Unit = {
    groupCorpus = List.tabulate(groupCount)(idx => s"$idx:$idx").mkString(" ")
    groupPattern = List.tabulate(groupCount)(_ => """(\d+:\d+)""").mkString(" ").r
  }

  @Benchmark def groupingBenchmark(bh: Blackhole) = {
    val r = groupPattern

    val res = groupCorpus match {
      case r(all @ _*) => all
      case _ => null
    }

    bh.consume(res)
  }
}
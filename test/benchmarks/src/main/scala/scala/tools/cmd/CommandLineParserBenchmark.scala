
package scala.tools.cmd

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
class CommandLineParserBenchmark {

  import CommandLineParser.tokenize

  @Param(Array("1000", "10000", "100000"))
  var argCount: Int = _
  var line: String = _
  var quotyline: String = _
  var embedded: String = _

  @Setup(Level.Trial) def init(): Unit = {
    line = List.tabulate(argCount)(n => s"arg$n").mkString(" ")
    val q = "\""
    quotyline = List.tabulate(argCount)(n => s"${q}arg${n}${q}").mkString(" ")
    embedded  = List.tabulate(argCount)(n => s"${n}${q}arg${q}${n}").mkString(" ")
  }
  @Benchmark def parsingBenchmark              = tokenize(line)
  @Benchmark def quoteUnquoteParsingBenchmark  = tokenize(quotyline)
  @Benchmark def embeddedQuoteParsingBenchmark = tokenize(embedded)
}

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
class RegexUnapplyBenchmark {

  /**
    * pre-compile regex at class constructor to save iteration time
    */
  val t8022CharSequenceRegex = """.*: (.)$""".r
  val t8022MatchRegex = """(\d)""".r
  val t8787nullMatchRegex = """\d+""".r
  val t8787nullMatcherRegex = """(\d+):(\d+)""".r

  @Benchmark def t8022CharSequence(bh: Blackhole): Unit = {
    val full = t8022CharSequenceRegex
    val text = "   When I use this operator: *"
    // Testing 2.10.x compatibility of the return types of unapplySeq
    val x :: Nil = full.unapplySeq(text: Any).get
    val y :: Nil = full.unapplySeq(text: CharSequence).get
    bh.consume(x)
    bh.consume(y)
  }

  @Benchmark def t8022Match(bh: Blackhole): Unit = {
    val R = t8022MatchRegex
    val matchh = R.findFirstMatchIn("a1").get
    // Testing 2.10.x compatibility of the return types of unapplySeq
    val x :: Nil = R.unapplySeq(matchh: Any).get
    val y :: Nil = R.unapplySeq(matchh).get
    bh.consume(x)
    bh.consume(y)
  }

  @Benchmark def t8787nullMatch(bh: Blackhole) = {
    val r = t8022MatchRegex
    val s: String = null
    val x = s match { case r() => 1 ; case _ => 2 }
    bh.consume(x)
  }

  @Benchmark def t8787nullMatcher(bh: Blackhole) = {
    val r = t8787nullMatcherRegex
    val s = "1:2 3:4 5:6"
    val z = ((r findAllMatchIn s).toList :+ null) flatMap {
      case r(x, y) => Some((x.toInt, y.toInt))
      case _       => None
    }

    bh.consume(z)
  }
}
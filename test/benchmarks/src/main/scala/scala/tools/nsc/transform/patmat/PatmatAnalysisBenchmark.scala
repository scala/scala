package scala.tools.nsc.transform.patmat

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

/** Time to compile a generated match of `n` cases up to and including `patmat`, with a new `Global`
 *  for each compile (see [[PatmatCompiler]]) and the shapes of [[PatmatShapes]]. The parser and typer
 *  are linear in `n` for these shapes, so `patmat` dominates as `n` grows.
 *
 *  Compare the scores across `n` to see how the analysis scales: each shape over a sealed type used to take time cubic in
 *  `n` (44 s for 1024 constructor patterns) and now takes time between linear and quadratic.
 *
 *  {{{
 *  bench/Jmh/run PatmatAnalysisBenchmark -p shape=tup,ctor -p n=64,256,1024
 *  }}}
 */
@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 5)
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class PatmatAnalysisBenchmark {
  @Param(Array("ctor", "typ", "tup", "opt", "guard", "seq", "ints")) var shape: String = _
  @Param(Array("64", "256")) var n: Int = _

  private var compiler: PatmatCompiler = _
  private var code: String = _

  @Setup(Level.Trial) def setup(): Unit = {
    compiler = new PatmatCompiler
    code = PatmatShapes.source(shape, n)
  }

  @Benchmark def patmat(): Long = compiler.patmatNanos(code)
}

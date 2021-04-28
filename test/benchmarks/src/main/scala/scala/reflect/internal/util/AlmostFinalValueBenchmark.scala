package scala.reflect.internal.util

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

class AlmostFinalValueBenchSettings extends scala.reflect.runtime.Settings {
  val flag = new BooleanSetting(false)

  @inline final def isTrue2: Boolean = AlmostFinalValueBenchmarkStatics.isTrue && flag
}

object AlmostFinalValueBenchSettings {
  implicit class SettingsOps(private val settings: AlmostFinalValueBenchSettings) extends AnyVal {
    @inline final def isTrue3: Boolean = AlmostFinalValueBenchmarkStatics.isTrue && settings.flag
  }

  @inline def isTrue4(settings: AlmostFinalValueBenchSettings): Boolean =
    AlmostFinalValueBenchmarkStatics.isTrue && settings.flag
}

@Warmup(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(3)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class AlmostFinalValueBenchmark {
  import AlmostFinalValueBenchmarkStatics.STATIC_FINAL_FALSE
  val settings = new AlmostFinalValueBenchSettings(); import settings._

  private def pretendToWorkHard() = Blackhole.consumeCPU(3)

  @Benchmark def bench0_unit                  = ()
  @Benchmark def bench0_usingStaticFinalFalse = if (STATIC_FINAL_FALSE && flag) pretendToWorkHard()
  @Benchmark def bench0_workingHard           = pretendToWorkHard()

  @Benchmark def bench1_usingAlmostFinalFalse = if (AlmostFinalValueBenchmarkStatics.isTrue && flag) pretendToWorkHard()
  @Benchmark def bench2_usingInlineMethod     = if (settings.isTrue2) pretendToWorkHard()
  @Benchmark def bench3_usingExtMethod        = if (settings.isTrue3) pretendToWorkHard()
  @Benchmark def bench4_usingObjectMethod     = if (AlmostFinalValueBenchSettings.isTrue4(settings)) pretendToWorkHard()

/*
  This benchmark is measuring two things:
  1. verifying that using AlmostFinalValue in an if block makes the block a no-op
  2. verifying and comparing which ergonomic wrapper around AlmostFinalValue maintains that

  The first point is satisfied.

  For the second:
  1. inline instance methods add a null-check overhead, slowing it down
  2. extension methods perform as quickly, are very ergonomic and so are the best choice
  3. object methods also perform as quickly, but can be less ergonomic if it requires an import
*/
}

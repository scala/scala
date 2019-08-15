package scala.tools.nsc

import java.nio.file.{Files, Paths}
import java.util.concurrent.TimeUnit

import org.openjdk.jmh
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.tools.nsc
import scala.tools.nsc.util.ClassPath
import scala.tools.util.PathResolver

@BenchmarkMode(Array(jmh.annotations.Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class AggregateClassPathBenchmark {
  @Param(Array(""))
  var classpathString: String = _
  var classpath: ClassPath = _
  val closeableRegistry = new CloseableRegistry
  @Setup def setup(): Unit = {
    val settings = new nsc.Settings()
    if (classpathString.startsWith("@"))
      classpathString = new String(Files.readAllBytes(Paths.get(classpathString.drop(1))))
    settings.classpath.value = classpathString
    val resolver = new PathResolver(settings, closeableRegistry)
    classpath = resolver.result
    classpath.list("")
  }

  @TearDown def teardown(): Unit = {
    closeableRegistry.close()
  }

  @Benchmark def test(bh: Blackhole) = {
    classpath.list("")._1.foreach(entry => bh.consume(classpath.list(entry.name)))
  }
}

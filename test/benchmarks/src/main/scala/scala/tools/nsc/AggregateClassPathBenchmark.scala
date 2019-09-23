package scala.tools.nsc

import java.nio.file.{Files, Paths}
import java.util.concurrent.TimeUnit

import org.openjdk.jmh
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.tools.nsc
import scala.tools.nsc.util.ClassPath
import scala.tools.util.{PathResolver, ReuseAllPathResolver}

@BenchmarkMode(Array(jmh.annotations.Mode.AverageTime))
@Fork(0)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class AggregateClassPathBenchmark {
  @Param(Array("@s:/scala/classpath.txt"))
  var classpathString: String = _
  @Param(Array("false", "true"))
  var reuse: Boolean = _
  var classpath: ClassPath = _
  val closeableRegistry = new CloseableRegistry
  val settings = new nsc.Settings()

  @Setup def setup(): Unit = {
    if (classpathString.startsWith("@"))
      classpathString = new String(Files.readAllBytes(Paths.get(classpathString.drop(1))))
    settings.classpath.value = classpathString
    if (reuse)
      settings.pathResolverFactory = ReuseAllPathResolver.create _
    else
      settings.pathResolverFactory = PathResolver.apply

    val resolver = settings.pathResolver(closeableRegistry)
    classpath = resolver.result
    classpath.list("")
  }

  @TearDown def teardown(): Unit = {
    closeableRegistry.close()
  }

  @Benchmark def testCreate(bh: Blackhole) = {
    val resolver = settings.pathResolver(closeableRegistry)
    classpath = resolver.result
    bh.consume (classpath.list(""))
    closeableRegistry.close()
  }

  @Benchmark def testUse(bh: Blackhole) = {
    classpath.list("")._1.foreach(entry => bh.consume(classpath.list(entry.name)))
  }
}


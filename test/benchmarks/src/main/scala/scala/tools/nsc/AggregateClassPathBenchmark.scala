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
@Fork(0)
@Threads(1)
@Warmup(iterations = 0)
@Measurement(iterations = 1)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class AggregateClassPathBenchmark {
  @Param(Array("@s:/scala/classpath.txt"))
  var classpathString: String = _
  var classpath: ClassPath = _
  val closeableRegistry = new CloseableRegistry
  @Setup def setup(): Unit = {
    val settings = new nsc.Settings()
    if (classpathString.startsWith("@"))
      classpathString = new String(Files.readAllBytes(Paths.get(classpathString.drop(1))))
    settings.classpath.value = classpathString
    val resolver = settings.pathResolver(closeableRegistry)
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
@BenchmarkMode(Array(jmh.annotations.Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10000)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class AggregateClassPathBenchmark2 {
  @Param(Array("@s:/scala/classpath.txt"))
  var classpathString: String = _
  var classpath: ClassPath = _
  val closeableRegistry = new CloseableRegistry
  val settings = new nsc.Settings()

  @Setup def setup(): Unit = {
    if (classpathString.startsWith("@"))
      classpathString = new String(Files.readAllBytes(Paths.get(classpathString.drop(1))))
    settings.classpath.value = classpathString
  }

  @Benchmark def test(bh: Blackhole) = {
    val resolver = new PathResolver(settings, closeableRegistry)
    classpath = resolver.result
    bh.consume (classpath.list(""))
    closeableRegistry.close()
  }
}
object Mike extends App {
  var classpathString: String = "@s:/scala/classpath.txt"
  var classpath: ClassPath = _
  val closeableRegistry = new CloseableRegistry
  val settings = new nsc.Settings()

  if (classpathString.startsWith("@"))
    classpathString = new String(Files.readAllBytes(Paths.get(classpathString.drop(1))))
  settings.classpath.value = classpathString

  while (true) {
    val resolver = new PathResolver(settings, closeableRegistry)
    classpath = resolver.result
    println(classpath.list(""))
    closeableRegistry.close()
  }
}

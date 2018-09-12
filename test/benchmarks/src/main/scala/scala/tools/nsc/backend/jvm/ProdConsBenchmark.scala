package scala.tools.nsc
package backend.jvm

import java.util.concurrent.TimeUnit

import scala.tools.asm.tree.ClassNode
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.collection.JavaConverters.asScalaIteratorConverter
import scala.tools.asm.tree.ClassNode

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class ProdConsBenchmark {
  type G <: Global
  var global: G = _
  private var classNode: ClassNode = _

  @Setup(Level.Trial) def setup(): Unit = {
    val settings = new Settings()
    settings.usejavacp.value = true
    val global = new Global(settings)
    import global._
    this.global = global.asInstanceOf[G]
    classNode = AsmUtils.readClass(global.classPath.findClassFile("scala.tools.nsc.typechecker.Implicits$ImplicitSearch").get.toByteArray)
  }

  @Benchmark
  def prodCons(bh: Blackhole): Unit = {
    val global: G = this.global
    import global.genBCode.postProcessor.backendUtils._
    for (m <- classNode.methods.iterator().asScala) {
      bh.consume(new ProdConsAnalyzer(m, classNode.name))
    }
  }
}


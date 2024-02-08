package scala.tools.nsc

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh

import scala.tools.nsc.plugins.{Plugin, PluginComponent}

@BenchmarkMode(Array(jmh.annotations.Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 5)
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class PhaseAssemblyBenchmark {
  class Data[G <: Global with Singleton](val global: G, val components: List[SubComponent { val global: G}])
  var data: Data[_] = _

  @Param(Array("1", "4", "8", "16"))
  var size: Int = 16

  @Setup
  def setup(): Unit = {
    val global = new Global(new Settings)
    case class component[G <: Global with Singleton](val global: G, val phaseName: String, override val runsRightAfter: Option[String], override val runsAfter: List[String], override val runsBefore: List[String]) extends SubComponent {
        override def newPhase(prev: Phase): Phase = ???

    }
    object component {
      def apply(phaseName: String, runsRightAfter: Option[String], runsAfter: List[String], runsBefore: List[String]): component[global.type] = {
        new component[global.type](global, phaseName, runsRightAfter, runsAfter, runsBefore)
      }
    }
    val N = size
    val components = List.tabulate(N){ i =>
      component(i.toString, None, if (i == 0) List("parser") else List.tabulate(2)(j => i - j - 1).filter(_ >= 0).map(_.toString), List("terminal"))
    } ::: List(component("parser", None, Nil, Nil), component("terminal", None, Nil, List(N.toString)))


    data = new Data[global.type](global, components )
  }

  @Benchmark def assemble(): Object = {
    val s = data.asInstanceOf[Data[Global with Singleton]]
    val g = s.global
    implicit val messaging: DependencyGraph.Messaging = DependencyGraph.Messaging.silent
    val graph = DependencyGraph(s.components.reverse)
    graph.removeDanglingNodes()
    graph.validateAndEnforceHardlinks()
    graph.collapseHardLinksAndLevels(graph.getNodeByPhase("parser"), 1)
    graph
  }
}

object PhaseAssemblyBenchmark {
  def main(args: Array[String]): Unit = {
    val bench = new PhaseAssemblyBenchmark
    bench.setup()
    bench.assemble()
  }
}

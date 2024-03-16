package scala.tools.nsc

import scala.collection.mutable

import org.scalacheck._
import Prop._
//import Gen._
//import Arbitrary._

case class Component[G <: Global with Singleton](
  global: G,
  phaseName: String,
  override val runsRightAfter: Option[String] = None,
  override val runsAfter: List[String] = Nil,
  override val runsBefore: List[String] = Nil,
  override val initial: Boolean = false,
  override val terminal: Boolean = false,
) extends SubComponent {
  override def newPhase(prev: Phase): Phase = new Phase(prev) {
    override def name = phaseName
    override def run() = ()
  }
}

object PhaseAssemblyTest extends Properties("PhaseAssembly constraints") {
  val genTrivialInt: Gen[Int] = Gen.choose(min = 1, max = 2)
  val genSmallInt: Gen[Int] = Gen.choose(min = 2, max = 20)
  val random = new scala.util.Random(123502L)
  property("one or two vertices") = forAllNoShrink(genTrivialInt) { N =>
    val settings = new Settings
    val global = new Global(settings)
    val names = Array.tabulate(N)(n => s"phase_${n+1}_${random.nextInt(1024)}")
    val components = (0 until N).map(i => Component(
      global,
      phaseName = names(i),
      initial = i == 0,
      terminal = i == N-1,
    ))
    val inputs = random.shuffle(components)
    val graph = DependencyGraph(inputs)
    val phases: List[SubComponent] = graph.compilerPhaseList()
    components(0) == phases.head
    components(N-1) == phases.last
  }
  property("small graph with follow constraints") = forAllNoShrink(genSmallInt) { N =>
    val settings = new Settings
    val global = new Global(settings)
    val names = Array.tabulate(N)(n => s"phase_${n+1}_${random.nextInt(1024)}")
    def randomBefore(n: Int): List[String] =
      if (n == 0) Nil
      else (1 to 3).map(_ => names(random.nextInt(n))).distinct.toList
    val components = (0 until N).map(i => Component(
      global,
      phaseName = names(i),
      runsAfter = randomBefore(i),
      initial = i == 0,
      terminal = i == N-1,
    ))
    val inputs = random.shuffle(components)
    val graph = DependencyGraph(inputs)
    val phases: List[SubComponent] = graph.compilerPhaseList()
    val (_, fails) = phases.foldLeft((mutable.Set.empty[String],mutable.Set.empty[SubComponent])) { case ((seen,fails), comp) => 
      if (!comp.runsAfter.forall(seen)) fails.addOne(comp)
      (seen.addOne(comp.phaseName), fails)
    }
    if (fails.nonEmpty) println {
      fails.map(comp => s"${comp.phaseName} runs after ${comp.runsAfter.mkString(",")}")
        .mkString("failures\n", "\n", s"\n${phases.map(_.phaseName).mkString(",")}")
    }
    components(0) == phases.head && fails.isEmpty
  }
}

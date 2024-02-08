/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc

import org.junit.Assert.assertEquals
import org.junit.Test

class PhaseAssemblyTest {
  @Test
  def multipleRunsRightAfter(): Unit = {
    val settings = new Settings
    //settings.verbose.tryToSet(Nil)
    val global = new Global(settings)
    case class component[G <: Global with Singleton](global: G, phaseName: String, override val runsRightAfter: Option[String], override val runsAfter: List[String], override val runsBefore: List[String]) extends SubComponent {
      override def newPhase(prev: Phase): Phase = ???
    }
    //val N = 16 * 4096 // 65536 ~ 11-21 secs, 256 ~ 1-2 secs
    //val N = 256
    val N = 16
    val random = new scala.util.Random(123502L)
    val names = Array.tabulate(N)(n => s"phase_${n+1}_${random.nextInt(1024)}")
    val parserAndTerminal = List(
      component(global, "parser", None, Nil, Nil),
      component(global,"terminal", None, Nil, List(N.toString))
    )
    val beforeTerminal = List("terminal")
    val components = names.foldLeft(parserAndTerminal) { (comps, nm) =>
      component(global, nm, runsRightAfter = comps.headOption.map(_.phaseName), runsAfter = Nil, runsBefore = beforeTerminal) :: comps
    }
    val inputs = random.shuffle(components)
    //implicit val messaging: DependencyGraph.Messaging = DependencyGraph.Messaging.throws
    implicit val messaging: DependencyGraph.Messaging = DependencyGraph.Messaging.silent
    //implicit val messaging: DependencyGraph.Messaging = DependencyGraph.Messaging.stdout
    val graph = DependencyGraph(inputs)
    graph.removeDanglingNodes()
    graph.validateAndEnforceHardlinks()
    graph.collapseHardLinksAndLevels(graph.getNodeByPhase("parser"), 1)
    val result: List[String] = graph.compilerPhaseList().map(_.phaseName).filter(_.startsWith("phase_"))
    //println(graph.compilerPhaseList().mkString("PHASE LIST\n", "\n", "\n"))
    assertEquals(names.toList, result)
  }
}

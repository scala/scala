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

import scala.reflect.internal.FatalError
import scala.tools.testkit.AssertUtil.assertThrows

class PhaseAssemblyTest {
  case class component[G <: Global with Singleton](
    global: G,
    phaseName: String,
    override val runsRightAfter: Option[String],
    override val runsAfter: List[String],
    override val runsBefore: List[String],
  ) extends SubComponent {
    override val initial: Boolean = phaseName == "parser"
    override val terminal: Boolean = phaseName == "terminal"
    override def newPhase(prev: Phase): Phase = ???
  }
  def parserAndTerminal[G <: Global with Singleton](global: G) = List(
    component(global, "parser", None, Nil, Nil),
    component(global,"terminal", None, Nil, Nil),
  )
  case class Komponent(phaseName: String, runsRightAfter: String = null, runsAfter: String = "", runsBefore: String = "")
  def komponents[G <: Global with Singleton](global: G)(ks: Komponent*): List[component[global.type]] =
    ks.iterator.map(k => component(global, k.phaseName, Option(k.runsRightAfter), List(k.runsAfter).filter(_.nonEmpty), List(k.runsBefore).filter(_.nonEmpty))).toList

  @Test def multipleRunsRightAfter: Unit = {
    val settings = new Settings
    //settings.verbose.tryToSet(Nil)
    val global = new Global(settings)
    val N = 16 * 4096 // 65536 ~ 11-21 secs, 256 ~ 1-2 secs
    //val N = 256
    //val N = 16
    val random = new scala.util.Random(123502L)
    val names = Array.tabulate(N)(n => s"phase_${n+1}_${random.nextInt(1024)}")
    val beforeTerminal = List("terminal")
    val components = names.foldLeft(parserAndTerminal(global)) { (comps, nm) =>
      component(global, nm, runsRightAfter = comps.headOption.map(_.phaseName), runsAfter = Nil, runsBefore = beforeTerminal) :: comps
    }
    val inputs = random.shuffle(components)
    val graph = DependencyGraph(inputs)
    val phases: List[SubComponent] = graph.compilerPhaseList()
    val result: List[String] = phases.map(_.phaseName).filter(_.startsWith("phase_"))
    assertEquals("parser", phases.head.phaseName)
    assertEquals("terminal", phases.last.phaseName)
    assertEquals(names.toList, result)
  }
  @Test def trivial: Unit = {
    val settings = new Settings
    val global = new Global(settings)
    val beforeTerminal = List("terminal")
    val names = Array("phooey", "kerfuffle")
    val components = names.foldLeft(parserAndTerminal(global)) { (comps, nm) =>
      component(global, nm, runsRightAfter = None, runsAfter = comps.headOption.map(_.phaseName).toList, runsBefore = beforeTerminal) :: comps
    }
    val inputs = components
    val graph = DependencyGraph(inputs)
    val result: List[SubComponent] = graph.compilerPhaseList()
    assertEquals("parser", result.head.phaseName)
    assertEquals("terminal", result.last.phaseName)
    assertEquals(names.toList, result.init.tail.map(_.phaseName))
  }
  @Test def `trivial conflict`: Unit = {
    val settings = new Settings
    val global = new Global(settings)
    val beforeTerminal = List("terminal")
    val names = Array("phooey", "kerfuffle", "konflikt")
    def rra(nm: String) = nm match { case "kerfuffle"|"konflikt" => Some("phooey") case _ => None }
    def ra(comps: List[component[global.type]], nm: String) = nm match { case "kerfuffle"|"konflikt" => Nil case _ => comps.headOption.map(_.phaseName).toList }
    val components = names.foldLeft(parserAndTerminal(global)) { (comps, nm) =>
      component(global, nm, rra(nm), ra(comps, nm), runsBefore = beforeTerminal) :: comps
    }
    val graph = DependencyGraph(components)
    assertThrows[FatalError](graph.compilerPhaseList(), _ == "Phases kerfuffle and konflikt both immediately follow phooey")
  }
  @Test def `trivial cycle`: Unit = {
    val settings = new Settings
    val global = new Global(settings)
    val beforeTerminal = List("terminal")
    val names = Array("phooey", "kerfuffle", "konflikt")
    def rra(nm: String) = None
    def ra(comps: List[component[global.type]], nm: String) = nm match {
      case "phooey" => List("parser", "konflikt")
      case "konflikt" => List("kerfuffle")
      case "kerfuffle" => List("phooey")
      case _ => comps.headOption.map(_.phaseName).toList
    }
    val components = names.foldLeft(parserAndTerminal(global)) { (comps, nm) =>
      component(global, nm, rra(nm), ra(comps, nm), runsBefore = beforeTerminal) :: comps
    }
    val graph = DependencyGraph(components)
    assertThrows[FatalError](graph.compilerPhaseList(), _ == "Phases form a cycle: phooey -> kerfuffle -> konflikt -> phooey")
  }
  @Test def `run before tightly bound phases`: Unit = {
    val settings = new Settings
    val global = new Global(settings)
    val components =
      component(global, "phooey", None, List("parser"), List("terminal")) ::
      component(global, "kerfuffle", None, List("phooey"), List("erasure")) ::
      component(global, "konflikt", None, List("phooey"), List("terminal")) ::
      component(global, "erasure", Some("konflikt"), Nil, List("terminal")) ::
      component(global, "posterasure", Some("erasure"), Nil, List("terminal")) ::
      parserAndTerminal(global)
    val graph = DependencyGraph(components)
    val result: List[SubComponent] = graph.compilerPhaseList()
    assertEquals(List("parser", "phooey", "kerfuffle", "konflikt", "erasure", "posterasure", "terminal"), result.map(_.phaseName))
  }
  //phaseList: List(parser, namer, packageobjects, typer, superaccessors, extmethods,
  //pickler, xsbt-api, xsbt-dependency, refchecks, patmat, uncurry, fields, tailcalls,
  //specialize, explicitouter, erasure, posterasure, lambdalift, constructors, flatten,
  //mixin, cleanup, delambdafy, jvm, xsbt-analyzer, terminal)
  // phasesSet is a hash set, so order of inputs should not matter.
  // this test was to debug ths initial CI failure, a bug in handling runsRightAfter.
  @Test def `constraints under sbt`: Unit = {
    val settings = new Settings
    val global = new Global(settings)
    val components = komponents(global)(
      Komponent("parser"),
      Komponent("namer", runsAfter = "parser"),
      Komponent("packageobjects", runsRightAfter = "namer"),
      Komponent("typer", runsRightAfter = "packageobjects"),
      Komponent("superaccessors", runsAfter = "typer"),
      Komponent("extmethods", runsAfter = "superaccessors"),
      Komponent("pickler", runsAfter = "extmethods"),
      Komponent("refchecks", runsAfter = "pickler"),
      Komponent("patmat", runsAfter = "refchecks"),
      Komponent("uncurry", runsAfter = "patmat"),
      Komponent("fields", runsAfter = "uncurry"),
      Komponent("tailcalls", runsAfter = "fields"),
      Komponent("specialize", runsAfter = "tailcalls"),
      Komponent("explicitouter", runsAfter = "specialize"),
      Komponent("erasure", runsAfter = "explicitouter"),
      Komponent("posterasure", runsRightAfter = "erasure"),
      Komponent("async", runsAfter = "posterasure"),
      Komponent("lambdalift", runsAfter = "async"),
      Komponent("constructors", runsAfter = "lambdalift"),
      Komponent("flatten", runsAfter = "constructors"),
      Komponent("mixin", runsAfter = "flatten"),
      Komponent("cleanup", runsAfter = "mixin"),
      Komponent("delambdafy", runsAfter = "cleanup"),
      Komponent("jvm", runsAfter = "delambdafy"),
      Komponent("terminal", runsAfter = "jvm"),
      Komponent("xsbt-api", runsRightAfter = "pickler", runsAfter = "typer", runsBefore = "erasure"),
      Komponent("xsbt-dependency", runsRightAfter = "xsbt-api", runsBefore = "refchecks"),
      Komponent("xsbt-analyzer", runsAfter = "jvm", runsBefore = "terminal"),
    )
    val graph = DependencyGraph(components)
    val result: List[SubComponent] = graph.compilerPhaseList()
    assertEquals(List(
      "parser",
      "namer",
      "packageobjects",
      "typer",
      "superaccessors",
      "extmethods",
      "pickler",
      "xsbt-api",
      "xsbt-dependency",
      "refchecks",
      "patmat",
      "uncurry",
      "fields",
      "tailcalls",
      "specialize",
      "explicitouter",
      "erasure",
      "posterasure",
      "async",
      "lambdalift",
      "constructors",
      "flatten",
      "mixin",
      "cleanup",
      "delambdafy",
      "jvm",
      "xsbt-analyzer",
      "terminal",
      ),
      result.map(_.phaseName))
  }
}

class SubComponentTest {
  @Test def `SubComponent has consistent hashCode and equals`: Unit = {
    var counter = 0
    def next() = { counter += 1; counter }
    case class MyComponent(id: Int) extends SubComponent {
      val global: scala.tools.nsc.Global = null
      def newPhase(prev: scala.tools.nsc.Phase): scala.tools.nsc.Phase = ???
      val phaseName: String = s"c${next()}"
      val runsAfter: List[String] = Nil
      val runsRightAfter: Option[String] = None
    }
    val c0 = MyComponent(0) // inadvertently equal
    val c1 = MyComponent(0)
    assert(c0 != c1 || c0.hashCode == c1.hashCode)
  }
}

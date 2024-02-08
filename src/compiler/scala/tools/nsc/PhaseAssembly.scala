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

import scala.collection.mutable
import scala.reflect.io.{File, Path}
import scala.tools.nsc.Reporting.WarningCategory

import DependencyGraph.Messaging

/** Converts an unordered morass of components into an order that
 *  satisfies their mutual constraints.
 */
trait PhaseAssembly {
  self: Global =>

  /** Called by Global#computePhaseDescriptors to compute phase order. */
  def computePhaseAssembly(): List[SubComponent] = {

    implicit val messaging: Messaging = Messaging(informProgress,
      runReporting.warning(NoPosition, _, WarningCategory.Other, site=""),
      globalError,
      (g, f) => for (d <- settings.outputDirs.getSingleOutput if !d.isVirtual) DependencyGraph.graphToDotFile(g, Path(d.file) / File(f))
    )

    val graph = DependencyGraph(phasesSet)

    // Output the phase dependency graph at this stage
    def dump(stage: Int) = settings.genPhaseGraph.valueSetByUser.foreach(n => graph.dump(s"$n-$stage.dot"))

    dump(1)

    // Remove nodes without phaseobj
    graph.removeDanglingNodes()

    dump(2)

    // Validate and Enforce hardlinks / runsRightAfter and promote nodes down the tree
    graph.validateAndEnforceHardlinks()

    dump(3)

    // test for cycles, assign levels and collapse hard links into nodes
    graph.collapseHardLinksAndLevels(graph.getNodeByPhase("parser"), 1)

    dump(4)

    graph.compilerPhaseList()
  }
}

/**
 * Aux data structure for solving the constraint system
 * The dependency graph container with helper methods for node and edge creation
 */
private class DependencyGraph {

  /** Simple edge with to and from refs */
  case class Edge(var frm: Node, var to: Node, var hard: Boolean)

  /**
   * Simple node with name and object ref for the phase object,
   * also sets of incoming and outgoing dependencies.
   */
  case class Node(phasename: String) {
    var phaseobj = Option.empty[List[SubComponent]]
    var before   = new mutable.HashSet[Edge]()
    val after    = new mutable.HashSet[Edge]()
    var visited  = false
    var level    = 0

    def allPhaseNames: String = phaseobj match {
      case None => phasename
      case Some(lst) => lst.map(_.phaseName).mkString(",")
    }
  }

  val nodes = new mutable.HashMap[String, Node]()
  val edges = new mutable.HashSet[Edge]()

  /** Given the name of a phase object, get the node for that name.
   *  If the node object does not exist, then create it.
   */
  def getNodeByPhase(name: String): Node      = nodes.getOrElseUpdate(name, Node(name))
  def getNodeByPhase(phs: SubComponent): Node = {
    val node: Node = getNodeByPhase(phs.phaseName)
    if (node.phaseobj.isEmpty)
      node.phaseobj = Some(List(phs))
    node
  }

  def softConnectNodes(frm: Node, to: Node) = connectNodes(Edge(frm, to, hard = false))
  def hardConnectNodes(frm: Node, to: Node) = connectNodes(Edge(frm, to, hard = true))

  // Connect the frm and to nodes in the edge and add it to the set of edges.
  private def connectNodes(e: Edge): Unit = {
    edges += e
    e.frm.after += e
    e.to.before += e
  }

  /* Given the entire graph, collect the phase objects at each level, where the phase
   * names are sorted alphabetical at each level, into the compiler phase list
   */
  def compilerPhaseList(): List[SubComponent] =
    nodes.values.toList.filter(_.level > 0).sortBy(x => (x.level, x.phasename)).flatMap(_.phaseobj).flatten

  // Test for graph cycles, assign levels to the nodes & collapse hard links into nodes.
  def collapseHardLinksAndLevels(node: Node, lvl: Int)(implicit messaging: Messaging): Unit = {
    if (node.visited) {
      dump("phase-cycle")
      throw new FatalError(s"Cycle in phase dependencies detected at ${node.phasename}, created phase-cycle.dot")
    }

    if (node.level < lvl)
      node.level = lvl // level up
    else if (node.level != 0) {
      node.visited = false
      return // no need to revisit
    }

    var hardlinks = node.before.filter(_.hard)
    while (hardlinks.nonEmpty) {
      for (hl <- hardlinks) {
        node.phaseobj = Some(node.phaseobj.get ++ hl.frm.phaseobj.get)
        node.before = hl.frm.before
        nodes -= hl.frm.phasename
        edges -= hl
        for (edge <- node.before)
          edge.to = node
      }
      hardlinks = node.before.filter(_.hard)
    }

    node.visited = true

    for (edge <- node.before) {
      collapseHardLinksAndLevels(edge.frm, lvl + 1)
    }

    node.visited = false
  }

  /* Find all edges in the given graph that are hard links.
   * For each hard link we need to check that it's the only dependency.
   * If not, then we will promote the other dependencies down.
   */
  def validateAndEnforceHardlinks()(implicit messaging: Messaging): Unit = {
    for (hl <- edges if hl.hard) {
      if (hl.frm.after.sizeIs > 1) {
        dump("phase-order")
        throw new FatalError(s"Phase ${hl.frm.phasename} can't follow ${hl.to.phasename}, created phase-order.dot")
      }
    }

    var rerun = true
    while (rerun) {
      rerun = false
      for (hl <- edges if hl.hard) {
        hl.to.before.filter(_.hard).toList match {
          case Seq() =>
            throw new FatalError("There is no runs right after dependency, where there should be one! This is not supposed to happen!")
          case asm @ (head :: _ :: _) =>
            dump("phase-order")
            val following = asm.map(_.frm.phasename).sorted mkString ","
            throw new FatalError(s"Multiple phases want to run right after ${head.to.phasename}; followers: $following; created phase-order.dot")
          case asm =>
            val promote = hl.to.before.filter(e => !e.hard)
            hl.to.before.clear()
            asm.foreach(edge => hl.to.before += edge)
            for (edge <- promote) {
              rerun = true
              val msg = s"promote the dependency of ${edge.frm.phasename}: ${edge.to.phasename} => ${hl.frm.phasename}"
              messaging.informProgress(msg)
              edge.to = hl.frm
              hl.frm.before += edge
            }
        }
      }
    }
  }

  /** Remove all nodes in the given graph, that have no phase object.
   *  Make sure to clean up all edges when removing the node object.
   *  `Inform` with warnings, if an external phase has a dependency on something that is dropped.
   */
  def removeDanglingNodes()(implicit messaging: Messaging): Unit = {
    for (node <- nodes.values if node.phaseobj.isEmpty) {
      val msg = s"dropping dependency on node with no phase object: ${node.phasename}"
      messaging.informProgress(msg)
      nodes -= node.phasename

      for (edge <- node.before) {
        edges -= edge
        edge.frm.after -= edge
        if (edge.frm.phaseobj exists (lsc => !lsc.head.internal))
          messaging.warning(msg)
      }
    }
  }

  def dump(title: String)(implicit messaging: Messaging): Unit = messaging.dump(this, s"$title.dot")
}
private object DependencyGraph {

  /** Given the phases set, will build a dependency graph from the phases set
   *  Using the aux. method of the DependencyGraph to create nodes and edges.
   */
  def apply(phasesSet: Iterable[SubComponent])(implicit messaging: Messaging): DependencyGraph = {
    val graph = new DependencyGraph

    for (phs <- phasesSet) {
      val fromnode = graph.getNodeByPhase(phs)

      phs.runsRightAfter match {
        case None =>
          for (phsname <- phs.runsAfter) {
            if (phsname != "terminal") {
              val tonode = graph.getNodeByPhase(phsname)
              graph.softConnectNodes(fromnode, tonode)
            } else {
              messaging.error(s"[phase assembly, after dependency on terminal phase not allowed: ${fromnode.phasename} => $phsname]")
            }
          }
          for (phsname <- phs.runsBefore) {
            if (phsname != "parser") {
              val tonode = graph.getNodeByPhase(phsname)
              graph.softConnectNodes(tonode, fromnode)
            } else {
              messaging.error(s"[phase assembly, before dependency on parser phase not allowed: $phsname => ${fromnode.phasename}]")
            }
          }
        case Some(phsname) =>
          if (phsname != "terminal") {
            val tonode = graph.getNodeByPhase(phsname)
            graph.hardConnectNodes(fromnode, tonode)
          } else {
            messaging.error(s"[phase assembly, right after dependency on terminal phase not allowed: ${fromnode.phasename} => $phsname]")
          }
      }
    }

    graph
  }

  /* This is a helper method, that given a dependency graph will generate a graphviz dot
   * file showing its structure.
   * Plug-in supplied phases are marked as green nodes and hard links are marked as blue edges.
   */
  def graphToDotFile(graph: DependencyGraph, file: File): Unit = {
    val edges    = graph.edges.toSeq
    val extnodes = edges.map(_.frm).filter(!_.phaseobj.get.head.internal)
    val fatnodes = edges.flatMap(e => List(e.frm, e.to)).filter(_.phaseobj.exists(_.sizeIs > 1))

    def color(hex: String)  = s""" [color="#$hex"]"""
    def node(n: graph.Node) = s""""${n.allPhaseNames}(${n.level})""""

    val buf = mutable.ListBuffer.empty[String]
    buf += "digraph G {"
    buf ++= edges.map(e => s"${node(e.frm)}->${node(e.to)}" + color(if (e.hard) "0000ff" else "000000"))
    buf ++= extnodes.distinct.map(n => node(n) + color("00ff00"))
    buf ++ fatnodes.distinct.map(n => node(n) + color("0000ff"))
    buf += "}"

    import scala.reflect.io._
    val f = Path(d.file) / File(filename)
    f.printlnAll(buf.toList: _*)
  }

  case class Messaging(informProgress: String => Unit, warning: String => Unit, error: String => Unit, dump: (DependencyGraph, String) => Unit)
  object Messaging {
    val silent = Messaging(_ => (), _ => (), _ => (), (_, _) => ())
    val stdout = Messaging(s => println(s), s => println(s), s => println(s), (_, _) => ())
    val throws = Messaging(s => fail(s), s => fail(s), s => fail(s), (_, _) => ())
    private def fail(s: String) = throw new Exception(s)
  }
}

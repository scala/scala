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
import scala.language.postfixOps

/** Converts an unordered morass of components into an order that
 *  satisfies their mutual constraints.
 *  @see SIP 00002. You have read SIP 00002?
 */
trait PhaseAssembly {
  self: Global =>

  /**
   * Aux data structure for solving the constraint system
   * The dependency graph container with helper methods for node and edge creation
   */
  private[nsc] class DependencyGraph {

    /** Simple edge with to and from refs */
    case class Edge(var frm: Node, var to: Node, var hard: Boolean)

    /**
     * Simple node with name and object ref for the phase object,
     * also sets of in and out going dependencies
     */
    case class Node(name: String) {
      val phasename = name
      var phaseobj: Option[List[SubComponent]] = None
      val after = new mutable.HashSet[Edge]()
      var before = new mutable.HashSet[Edge]()
      var visited: VisitStatus = NotVisited
      var level = 0

      def allPhaseNames(): String = phaseobj match {
        case None => phasename
        case Some(lst) => lst.map(_.phaseName).reduceLeft(_+","+_)
      }
    }
    sealed abstract class VisitStatus
    case object NotVisited extends VisitStatus
    case object Visiting extends VisitStatus
    case object Visited extends VisitStatus

    val nodes = new mutable.HashMap[String,Node]()
    val edges = new mutable.HashSet[Edge]()

    /** Given a phase object, get the node for this phase object. If the
     *  node object does not exist, then create it.
     */
    def getNodeByPhase(phs: SubComponent): Node = {
      val node: Node = getNodeByPhase(phs.phaseName)
      node.phaseobj match {
        case None =>
          node.phaseobj = Some(List[SubComponent](phs))
        case _ =>
      }
      node
    }

    /* Given the name of a phase object, get the node for that name. If the
     * node object does not exits, then create it.
     */
    def getNodeByPhase(name: String): Node =
      nodes.getOrElseUpdate(name, Node(name))

    /* Connect the frm and to nodes with an edge and make it soft.
     * Also add the edge object to the set of edges, and to the dependency
     * list of the nodes
     */
    def softConnectNodes(frm: Node, to: Node) {
      val e = Edge(frm, to, false)
      this.edges += e

      frm.after += e
      to.before += e
    }

    /* Connect the frm and to nodes with an edge and make it hard.
     * Also add the edge object to the set of edges, and to the dependency
     * list of the nodes
     */
    def hardConnectNodes(frm: Node, to: Node) {
      val e = Edge(frm, to, true)
      this.edges += e

      frm.after += e
      to.before += e
    }

    /* Given the entire graph, collect the phase objects at each level, where the phase
     * names are sorted alphabetical at each level, into the compiler phase list
     */
    def compilerPhaseList(): List[SubComponent] =
      nodes.values.toList filter (_.level > 0) sortBy (x => (x.level, x.phasename)) flatMap (_.phaseobj) flatten

    /* Test if there are cycles in the graph, assign levels to the nodes
     * and collapse hard links into nodes
     */
    def collapseHardLinks() {
      for (node <- nodes.valuesIterator.toList) {
        val hardBefores = node.before.iterator.filter(_.hard).toList
        for (hl <- hardBefores) {
          val effectiveNode: Node = if (nodes.contains(node.name)) node else {
            nodes.find(_._2.phaseobj.exists(_.exists(_.phaseName == node.name))).get._2
          }
          effectiveNode.phaseobj = Some(effectiveNode.phaseobj.get ++ hl.frm.phaseobj.get)
          effectiveNode.before = hl.frm.before
          nodes -= hl.frm.phasename
          edges -= hl
        }
      }
    }

    /* Test if there are cycles in the graph, assign levels to the nodes
     * and collapse hard links into nodes
     */
    def assignLevelsAndDetectCycles(node: Node) {
      val stack = mutable.ArrayStack[Node]()
      def visitNode(node: Node): Unit = {
        node.visited = Visiting
        for (edge <- node.before) {
          val from = edge.frm
          from.visited match {
            case NotVisited =>
              visitNode(edge.frm)
            case Visiting =>
              dump("phase-cycle")
              throw new FatalError(s"Cycle in phase dependencies detected at ${node.phasename}, created phase-cycle.dot")
            case Visited =>
          }
        }
        node.visited = Visited
        stack.push(node)
      }
      try {
        visitNode(node)
      } finally {
        nodes.values.foreach(_.visited = NotVisited)
      }

      val topoSort: Map[Node, Int] = stack.zipWithIndex.toMap
      val root = node
      assert(stack.head == root, stack)
      root.level = 1

      // Nodes that have been collapsed into their hard-linked predecessor
      val collapsed: Map[String, Node] = stack.iterator.flatMap(p => p.phaseobj.toList.flatMap(_.map(x => (x.phaseName, p)))).toMap
      def followHard(node: Node): Node = collapsed.getOrElse(node.phasename, node)

      // find the longest path to the root node to assign as the level.
      stack.iterator.drop(1).foreach { node =>
        var n = node
        var level = 1
        while (n != root && n.after.nonEmpty) {
          n = n.after.maxBy(edge => topoSort.get(followHard(edge.to))).to
          level += 1
        }
        node.level = level
      }
      def phaseDebug = stack.toSeq.groupBy(_.level).toList.sortBy(_._1).map { case (level, nodes) => (level,  nodes.sortBy(_.phasename).map(_.phaseobj.map(_.map(_.phaseName).mkString(":")).mkString(" ")).mkString(" "))}.mkString("\n")
      debuglog("Phase assembly levels: " + phaseDebug)
    }

    /* Find all edges in the given graph that are hard links. For each hard link we
     * need to check that it's the only dependency. If not, then we will promote the
     * other dependencies down
     */
    def validateAndEnforceHardlinks() {
      var hardlinks = edges.filter(_.hard)
      for (hl <- hardlinks) {
        if (hl.frm.after.size > 1) {
          dump("phase-order")
          throw new FatalError(s"Phase ${hl.frm.phasename} can't follow ${hl.to.phasename}, created phase-order.dot")
        }
      }

      var rerun = true
      while (rerun) {
        rerun = false
        hardlinks = edges.filter(_.hard)
        for (hl <- hardlinks) {
          val sanity = Nil ++ hl.to.before.filter(_.hard)
          if (sanity.length == 0) {
            throw new FatalError("There is no runs right after dependency, where there should be one! This is not supposed to happen!")
          } else if (sanity.length > 1) {
            dump("phase-order")
            val following = (sanity map (_.frm.phasename)).sorted mkString ","
            throw new FatalError(s"Multiple phases want to run right after ${sanity.head.to.phasename}; followers: $following; created phase-order.dot")
          } else {

            val promote = hl.to.before.filter(e => (!e.hard))
            hl.to.before.clear()
            sanity foreach (edge => hl.to.before += edge)
            for (edge <- promote) {
              rerun = true
              informProgress(
                "promote the dependency of " + edge.frm.phasename +
                ": "  + edge.to.phasename + " => " + hl.frm.phasename)
              edge.to = hl.frm
              hl.frm.before += edge
            }
          }
        }
      }
    }

    /** Remove all nodes in the given graph, that have no phase object
     *  Make sure to clean up all edges when removing the node object
     *  `Inform` with warnings, if an external phase has a
     *  dependency on something that is dropped.
     */
    def removeDanglingNodes() {
      for (node <- nodes.values filter (_.phaseobj.isEmpty)) {
        val msg = "dropping dependency on node with no phase object: "+node.phasename
        informProgress(msg)
        nodes -= node.phasename

        for (edge <- node.before) {
          edges -= edge
          edge.frm.after -= edge
          if (edge.frm.phaseobj exists (lsc => !lsc.head.internal))
            warning(msg)
        }
      }
    }

    def dump(title: String = "phase-assembly") = graphToDotFile(this, s"$title.dot")
  }


  /** Called by Global#computePhaseDescriptors to compute phase order. */
  def computePhaseAssembly(): List[SubComponent] = {

    // Add all phases in the set to the graph
    val graph = phasesSetToDepGraph(phasesSet)

    val dot = settings.genPhaseGraph.valueSetByUser

    // Output the phase dependency graph at this stage
    def dump(stage: Int) = dot foreach (n => graphToDotFile(graph, s"$n-$stage.dot"))

    dump(1)

    // Remove nodes without phaseobj
    graph.removeDanglingNodes()

    dump(2)

    // Validate and Enforce hardlinks / runsRightAfter and promote nodes down the tree
    graph.validateAndEnforceHardlinks()

    dump(3)

    // collapse hard links into nodes
    graph.collapseHardLinks()

    dump(4)

    // test for cycles, assign levels
    graph.assignLevelsAndDetectCycles(graph.getNodeByPhase("parser"))

    dump(5)

    // assemble the compiler
    graph.compilerPhaseList()
  }

  /** Given the phases set, will build a dependency graph from the phases set
   *  Using the aux. method of the DependencyGraph to create nodes and edges.
   */
  private[nsc] def phasesSetToDepGraph(phsSet: Iterable[SubComponent]): DependencyGraph = {
    val graph = new DependencyGraph()

    for (phs <- phsSet) {

      val fromnode = graph.getNodeByPhase(phs)

      phs.runsRightAfter match {
        case None =>
          for (phsname <- phs.runsAfter) {
            if (phsname != "terminal") {
              val tonode = graph.getNodeByPhase(phsname)
              graph.softConnectNodes(fromnode, tonode)
            } else {
              globalError("[phase assembly, after dependency on terminal phase not allowed: " + fromnode.phasename + " => "+ phsname + "]")
            }
          }
          for (phsname <- phs.runsBefore) {
            if (phsname != "parser") {
              val tonode = graph.getNodeByPhase(phsname)
              graph.softConnectNodes(tonode, fromnode)
            } else {
              globalError("[phase assembly, before dependency on parser phase not allowed: " + phsname + " => "+ fromnode.phasename + "]")
            }
          }
        case Some(phsname) =>
          if (phsname != "terminal") {
            val tonode = graph.getNodeByPhase(phsname)
            graph.hardConnectNodes(fromnode, tonode)
          } else {
            globalError("[phase assembly, right after dependency on terminal phase not allowed: " + fromnode.phasename + " => "+ phsname + "]")
          }
      }
    }
    graph
  }

  /* This is a helper method, that given a dependency graph will generate a graphviz dot
   * file showing its structure.
   * Plug-in supplied phases are marked as green nodes and hard links are marked as blue edges.
   */
  private def graphToDotFile(graph: DependencyGraph, filename: String) {
    val sbuf = new StringBuilder
    val extnodes = new mutable.HashSet[graph.Node]()
    val fatnodes = new mutable.HashSet[graph.Node]()
    sbuf.append("digraph G {\n")
    for (edge <- graph.edges) {
      sbuf.append("\"" + edge.frm.allPhaseNames + "(" + edge.frm.level + ")" + "\"->\"" + edge.to.allPhaseNames + "(" + edge.to.level + ")" + "\"")
      if (!edge.frm.phaseobj.get.head.internal) extnodes += edge.frm
      edge.frm.phaseobj foreach (phobjs => if (phobjs.tail.nonEmpty) fatnodes += edge.frm )
      edge.to.phaseobj foreach (phobjs => if (phobjs.tail.nonEmpty) fatnodes += edge.to )
      val color = if (edge.hard) "#0000ff" else "#000000"
      sbuf.append(s""" [color="$color"]\n""")
    }
    for (node <- extnodes) {
      sbuf.append("\"" + node.allPhaseNames + "(" + node.level + ")" + "\" [color=\"#00ff00\"]\n")
    }
    for (node <- fatnodes) {
      sbuf.append("\"" + node.allPhaseNames + "(" + node.level + ")" + "\" [color=\"#0000ff\"]\n")
    }
    sbuf.append("}\n")
    import reflect.io._
    for (d <- settings.outputDirs.getSingleOutput if !d.isVirtual) Path(d.file) / File(filename) writeAll sbuf.toString
  }
}

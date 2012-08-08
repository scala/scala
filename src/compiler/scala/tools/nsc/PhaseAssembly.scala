/* NSC -- new Scala compiler
 * Copyright 2007-2012 LAMP/EPFL
 * @author Anders Bach Nielsen
 * @version 1.0
 */

package scala.tools.nsc

import java.io.{ BufferedWriter, FileWriter }
import scala.collection.mutable
import language.postfixOps

/**
 * PhaseAssembly
 * Trait made to separate the constraint solving of the phase order from
 * the rest of the compiler. See SIP 00002
 *
 */
trait PhaseAssembly {
  self: Global =>

  /**
   * Aux datastructure for solving the constraint system
   * The depency graph container with helper methods for node and edge creation
   */
  class DependencyGraph {

    /**
     * Simple edge with to and from refs
     */
    class Edge(var frm: Node, var to: Node, var hard: Boolean)

    /**
     * Simple node with name and object ref for the phase object,
     * also sets of in and out going dependencies
     */
    class Node(name: String) {
      val phasename = name
      var phaseobj: Option[List[SubComponent]] = None
      val after = new mutable.HashSet[Edge]()
      var before = new mutable.HashSet[Edge]()
      var visited = false
      var level = 0

      def allPhaseNames(): String = phaseobj match {
        case None => phasename
        case Some(lst) => lst.map(_.phaseName).reduceLeft(_+","+_)
      }
    }

    val nodes = new mutable.HashMap[String,Node]()
    val edges = new mutable.HashSet[Edge]()

    /* Given a phase object, get the node for this phase object. If the
     * node object does not exist, then create it.
     */
    def getNodeByPhase(phs: SubComponent): Node = {
      var node: Node = getNodeByPhase(phs.phaseName)
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
      nodes.getOrElseUpdate(name, new Node(name))

    /* Connect the frm and to nodes with an edge and make it soft.
     * Also add the edge object to the set of edges, and to the dependency
     * list of the nodes
     */
    def softConnectNodes(frm: Node, to: Node) {
      var e = new Edge(frm, to, false)
      this.edges += e

      frm.after += e
      to.before += e
    }

    /* Connect the frm and to nodes with an edge and make it hard.
     * Also add the edge object to the set of edges, and to the dependency
     * list of the nodes
     */
    def hardConnectNodes(frm: Node, to: Node) {
      var e = new Edge(frm, to, true)
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
    def collapseHardLinksAndLevels(node: Node, lvl: Int) {
      if (node.visited) {
        throw new FatalError(
          "Cycle in compiler phase dependencies detected, phase " +
          node.phasename + " reacted twice!")
      }

      if (node.level < lvl) node.level = lvl

      var hls = Nil ++ node.before.filter(_.hard)
      while (hls.size > 0) {
        for (hl <- hls) {
          node.phaseobj = Some(node.phaseobj.get ++ hl.frm.phaseobj.get)
          node.before = hl.frm.before
          nodes -= hl.frm.phasename
          edges -= hl
          for (edge <- node.before) edge.to = node
        }
        hls = Nil ++ node.before.filter(_.hard)
      }
      node.visited = true

      for (edge <- node.before) {
        collapseHardLinksAndLevels( edge.frm, lvl + 1)
      }

      node.visited = false
    }

    /* Find all edges in the given graph that are hard links. For each hard link we
     * need to check that its the only dependency. If not, then we will promote the
     * other dependencies down
     */
    def validateAndEnforceHardlinks() {
      var hardlinks = edges.filter(_.hard)
      for (hl <- hardlinks) {
        if (hl.frm.after.size > 1) {
          throw new FatalError("phase " + hl.frm.phasename + " want to run right after " + hl.to.phasename + ", but some phase has declared to run before " + hl.frm.phasename + ". Re-run with -Xgenerate-phase-graph <filename> to better see the problem.")
        }
      }

      var rerun = true
      while (rerun) {
        rerun = false
        hardlinks = edges.filter(_.hard)
        for (hl <- hardlinks) {
          var sanity = Nil ++ hl.to.before.filter(_.hard)
          if (sanity.length == 0) {
            throw new FatalError("There is no runs right after dependency, where there should be one! This is not supposed to happen!")
          } else if (sanity.length > 1) {
            var msg = "Multiple phases want to run right after the phase " + sanity.head.to.phasename + "\n"
            msg += "Phases: "
            sanity = sanity sortBy (_.frm.phasename)
            for (edge <- sanity) {
              msg += edge.frm.phasename + ", "
            }
            msg += "\nRe-run with -Xgenerate-phase-graph <filename> to better see the problem."
            throw new FatalError(msg)

          } else {

            var promote = hl.to.before.filter(e => (!e.hard))
            hl.to.before.clear
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
     *  <code>Inform</code> with warnings, if an external phase has a
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
  }

  /* Method called from computePhaseDescriptors in class Global
   */
  def buildCompilerFromPhasesSet(): List[SubComponent] = {

    // Add all phases in the set to the graph
    val graph = phasesSetToDepGraph(phasesSet)

    // Output the phase dependency graph at this stage
    if (settings.genPhaseGraph.value != "")
      graphToDotFile(graph, settings.genPhaseGraph.value + "1.dot")

    // Remove nodes without phaseobj
    graph.removeDanglingNodes()

    // Output the phase dependency graph at this stage
    if (settings.genPhaseGraph.value != "")
      graphToDotFile(graph, settings.genPhaseGraph.value + "2.dot")

    // Validate and Enforce hardlinks / runsRightAfter and promote nodes down the tree
    graph.validateAndEnforceHardlinks()

    // Output the phase dependency graph at this stage
    if (settings.genPhaseGraph.value != "")
      graphToDotFile(graph, settings.genPhaseGraph.value + "3.dot")

    // test for cycles, assign levels and collapse hard links into nodes
    graph.collapseHardLinksAndLevels(graph.getNodeByPhase("parser"), 1)

    // Output the phase dependency graph at this stage
    if (settings.genPhaseGraph.value != "")
      graphToDotFile(graph, settings.genPhaseGraph.value + "4.dot")

    // assemble the compiler
    graph.compilerPhaseList()
  }

  /** Given the phases set, will build a dependency graph from the phases set
   *  Using the aux. method of the DependencyGraph to create nodes and egdes.
   */
  private def phasesSetToDepGraph(phsSet: mutable.HashSet[SubComponent]): DependencyGraph = {
    val graph = new DependencyGraph()

    for (phs <- phsSet) {

      var fromnode = graph.getNodeByPhase(phs)

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
      if (! edge.frm.phaseobj.get.head.internal) {
               extnodes += edge.frm
      }
      edge.frm.phaseobj match { case None => null case Some(ln) => if(ln.size > 1) fatnodes += edge.frm }
      edge.to.phaseobj match { case None => null case Some(ln) => if(ln.size > 1) fatnodes += edge.to }
      if (edge.hard) {
        sbuf.append(" [color=\"#0000ff\"]\n")
      } else {
        sbuf.append(" [color=\"#000000\"]\n")
      }
    }
    for (node <- extnodes) {
      sbuf.append("\"" + node.allPhaseNames + "(" + node.level + ")" + "\" [color=\"#00ff00\"]\n")
    }
    for (node <- fatnodes) {
      sbuf.append("\"" + node.allPhaseNames + "(" + node.level + ")" + "\" [color=\"#0000ff\"]\n")
    }
    sbuf.append("}\n")
    var out = new BufferedWriter(new FileWriter(filename))
    out.write(sbuf.toString)
    out.flush()
    out.close()
  }

}

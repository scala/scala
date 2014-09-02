/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 * @author Anders Bach Nielsen
 * @version 1.0
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
   * Aux datastructure for solving the constraint system
   * The depency graph container with helper methods for node and edge creation
   */
  private class DependencyGraph {

    /** Simple edge with to and from refs */
    class Edge(var frm: Node, var to: Node, var hard: Boolean)

    /**
     * Simple node with name and object ref for the phase object,
     * also sets of in and out going dependencies
     */
    class Node(val phaseName: String) {
      var phaseObjs: List[SubComponent] = Nil
      val after : mutable.Set[Edge] = new mutable.HashSet()
      val before : mutable.Set[Edge] = new mutable.HashSet()
      var level = 0
      var color : Int = _ // DFS color, initialized in checkForCycles()
      var unsat : Int = _ // # of unsatisfied deps, initialized in computePhaseOrder()

      def internal: Boolean = phaseObjs.exists(_.internal)
      def allPhaseNames: String = phaseObjs.map(_.phaseName).mkString(",")
    }

    val nodes = mutable.Map.empty[String,Node]

    /** Checks if the graph contains cycles by performing a DFS. If there
     *  are cycles, a FatalError is thrown.
     */
    def checkForCycles() {
      nodes.values.foreach(_.color = 0)
      nodes.values.toList.sortBy(_.phaseName).foreach(dfs)
    }

    /* DFS implementation. */
    private def dfs(n: Node) {
      if (n.color == 1) {
        dump("phase-cycle")
        throw new FatalError(s"Cycle in phase dependencies detected at ${n.phaseName}, created phase-cycle.dot")
      }
      else if(n.color == 0) {
        n.color = 1
        n.after.toList.sortBy(_.to.phaseName).foreach(e => dfs(e.to))
        n.color = 2
      }
    }


    /** Given a phase object, get the node for this phase object. If the
     *  node object does not exist, then create it.
     */
    def getNodeByPhase(phs: SubComponent): Node = {
      val node: Node = getNodeByPhase(phs.phaseName)
      node.phaseObjs match {
        case Nil =>
          node.phaseObjs = List(phs)
        case _ =>
      }
      node
    }

    /** Given the name of a phase object, get the node for that name. If the
     *  node object does not exits, then create it.
     */
    def getNodeByPhase(name: String): Node =
      nodes.getOrElseUpdate(name, new Node(name))

    /** Connect the frm and to nodes with an edge and make it soft.
     *  Also add the edge object to the set of edges, and to the dependency
     *  list of the nodes
     */
    def softConnectNodes(frm: Node, to: Node) {
      val e = new Edge(frm, to, false)

      frm.after += e
      to.before += e
    }

    /** Connect the frm and to nodes with an edge and make it hard.
     *  Also add the edge object to the set of edges, and to the dependency
     *  list of the nodes
     */
    def hardConnectNodes(frm: Node, to: Node) {
      val e = new Edge(frm, to, true)

      frm.after += e
      to.before += e
    }

    /** Ensure that every node in the graph is a (transitive) successor of
     *  in and a (transitive) predecessor of out.
     */
    def enclose(in: Node, out: Node) {
      for (n <- nodes.values filter { n => n != in && n.before.isEmpty})
        softConnectNodes(in, n)
      for (n <- nodes.values filter { n => n != out && n.after.isEmpty })
        softConnectNodes(n, out)
    }

    /** Computes the order of compiler phases by performing a topological sort.
     *  As a side effect, also computes the level for each node (this is used
     *  solely for rendering purposes).
     */
    def computePhaseOrder(): List[SubComponent] = {
      var level = 1
      // Initialize the unsat counter
      for (n <- nodes.values) {
        n.unsat = n.after.size
      }
      var (next,remaining) = nodes.values.toList.partition(_.unsat == 0)
      val out = mutable.ListBuffer.empty[SubComponent]

      // Process nodes at the next level
      while (!next.isEmpty) {
        // SID-2 mandates that phases at the same level are sorted alphabetically
        out ++= next sortBy (_.phaseName) flatMap { _.phaseObjs }
        for (n <- next) {
          n.level = level
          for (e <- n.before) {
            e.frm.unsat -= 1
          }
        }
        level += 1
        // Continue with the next level
        val (n,r) = remaining.partition(_.unsat == 0)
        next = n
        remaining = r
      }
      // Since the graph is acyclic, this should NOT happen
      assert(remaining.isEmpty, "The following nodes in the phase dependency graph " +
                                "have not been processed: " + remaining.mkString(","))

      out toList
    }

    /** Verifies that there are no unsatisfiable rightAfter dependencies, and collapses maximal
     *  chains of hardlinks into single nodes.
     */
    def collapseHardlinks() {
      // Search for nodes that have 2 or more phases that want to run right after it
      val hlnodes = nodes.values.map(n => (n, n.before.filter(_.hard))).filterNot(_._2.isEmpty)
      hlnodes.find(_._2.size > 1) match {
        case Some((n,hls)) =>
          val following = hls.map(_.frm.phaseName).toList.sorted.mkString(",")
          throw new FatalError(s"Multiple phases want to run right after ${n.phaseName}; followers: $following; created phase-order.dot")
        case None =>
      }

      // Find a node with a rightAfter dependency
      var hlnode = nodes.values.find(n => n.after.exists(_.hard))
      for ((tgt,hllist) <- hlnodes) {
        val n = hllist.head.frm
        nodes -= n.phaseName
        // Merge the next node into `tgt`
        // This is critical. Since we only retain the node we merge into
        // in the map, merging into the source might result in us losing
        // the `parser` node.
        tgt.phaseObjs = tgt.phaseObjs ++ n.phaseObjs
        // Redirect edges
        n.before foreach { _.to = tgt }
        n.after foreach { _.frm = tgt }
        tgt.before ++= n.before
        tgt.after ++= n.after
        // We have verified that the graph was cycle-free before, so every selfloop that might have
        // been introduced by the above two lines can safely be removed
        tgt.before.retain(e => e.frm != e.to)
        tgt.after.retain(e => e.frm != e.to)

        // Next iteration
        hlnode = nodes.values.find(n => n.after.exists(_.hard))
      }
    }

    /** Remove all nodes in the given graph, that have no phase object
     *  Make sure to clean up all edges when removing the node object
     *  `Inform` with warnings, if an external phase has a
     *  dependency on something that is dropped.
     */
    def removeDanglingNodes() {
      for (node <- nodes.values filter (_.phaseObjs.isEmpty)) {
        val msg = "dropping dependency on node with no phase object: "+node.phaseName
        informProgress(msg)
        nodes -= node.phaseName

        for (edge <- node.before) {
          edge.frm.after -= edge
          if (!edge.frm.internal)
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

    graph.checkForCycles()

    val dot = if (settings.genPhaseGraph.isSetByUser) Some(settings.genPhaseGraph.value) else None

    // Output the phase dependency graph at this stage
    def dump(stage: Int) = dot foreach (n => graphToDotFile(graph, s"$n-$stage.dot"))

    dump(1)

    // Remove nodes without phaseobj
    graph.removeDanglingNodes()

    dump(2)

    graph.collapseHardlinks()

    // Second check for cycles, as collapsing nodes might have introduced cycles
    // (this is the case if there is a component specifies to run after phase1
    // and before phase2, and phase2 specifies to run right after phase1).
    graph.checkForCycles()

    dump(3)

    // Ensure that every node is on a path between terminal and parser
    graph.enclose(graph.getNodeByPhase("terminal"), graph.getNodeByPhase("parser"))
    
    val phases = graph.computePhaseOrder

    dump(4)

    // return the computed phase order
    phases
  }

  /** Given the phases set, will build a dependency graph from the phases set
   *  Using the aux. method of the DependencyGraph to create nodes and egdes.
   */
  private def phasesSetToDepGraph(phsSet: mutable.HashSet[SubComponent]): DependencyGraph = {
    val graph = new DependencyGraph()

    for (phs <- phsSet) {
      val fromnode = graph.getNodeByPhase(phs)

      for (phsname <- phs.runsRightAfter) {
        if (phsname != "terminal") {
          val tonode = graph.getNodeByPhase(phsname)
          graph.hardConnectNodes(fromnode, tonode)
        } else {
          globalError(s"[phase assembly, right after dependency on terminal phase not allowed: ${fromnode.phaseName} => ${phsname}]")
        }
      }
      for (phsname <- phs.runsAfter) {
        if (phsname != "terminal") {
          val tonode = graph.getNodeByPhase(phsname)
          graph.softConnectNodes(fromnode, tonode)
        } else {
          globalError(s"[phase assembly, after dependency on terminal phase not allowed: ${fromnode.phaseName} => ${phsname}]")
        }
      }
      for (phsname <- phs.runsBefore) {
        if (phsname != "parser") {
          val tonode = graph.getNodeByPhase(phsname)
          graph.softConnectNodes(tonode, fromnode)
        } else {
          globalError(s"[phase assembly, before dependency on parser phase not allowed: ${phsname} => ${fromnode.phaseName}]")
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
    def node2str(n: graph.Node) = {
      val escaped = n.allPhaseNames.replace("\"", "\\\"")
      s""""${escaped}(${n.level})""""
    }

    val sbuf = new StringBuilder
    sbuf.append("digraph G {\n")
    for (n <- graph.nodes.values) {
      sbuf.append(node2str(n))
      if (!n.internal) sbuf.append(""" [color="#00ff00"]""")
      else if (n.phaseObjs.tail.nonEmpty) sbuf.append(""" [color="#0000ff"]""")
      sbuf.append("\n")
    } 
    for (n <- graph.nodes.values; e <- n.after) {
      sbuf.append(s"${node2str(e.frm)} -> ${node2str(e.to)}")
      if (e.hard) sbuf.append(""" [color="#0000ff"]""")
      sbuf.append("\n")
    }
    sbuf.append("}\n")
    import reflect.io._
    for (d <- settings.outputDirs.getSingleOutput if !d.isVirtual) Path(d.file) / File(filename) writeAll sbuf.toString
  }
}

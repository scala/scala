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

import scala.collection.mutable, mutable.ArrayDeque, mutable.ListBuffer
import scala.reflect.io.{File, Path}
import scala.util.chaining._

/** Sorts the global phasesSet according to SubComponent constraints.
 */
trait PhaseAssembly {
  this: Global =>

  /** Called by Global#computePhaseDescriptors to compute phase order.
   *
   *  The phases to assemble are provided by `phasesSet`, which must contain
   *  an `initial` phase. If no phase is `terminal`, then `global.terminal` is added.
   */
  def computePhaseAssembly(): List[SubComponent] = {
    require(phasesSet.exists(phase => phase.initial || phase.phaseName == DependencyGraph.Parser), "Missing initial phase")
    if (!phasesSet.exists(phase => phase.terminal || phase.phaseName == DependencyGraph.Terminal)) {
      phasesSet.add(terminal)
      reporter.warning(NoPosition, "Added default terminal phase")
    }
    val graph = DependencyGraph(phasesSet, warnAll = !settings.isScaladoc || settings.isDebug)
    for (n <- settings.genPhaseGraph.valueSetByUser; d <- settings.outputDirs.getSingleOutput if !d.isVirtual)
      DependencyGraph.graphToDotFile(graph, Path(d.file) / File(s"$n.dot"))
    graph.compilerPhaseList().tap(_ => graph.warnings.foreach(msg => reporter.warning(NoPosition, msg)))
  }
}

/** A graph with the given number of vertices.
 *
 *  Each vertex is labeled with its phase name.
 */
class DependencyGraph(start: String, val components: Map[String, SubComponent]) {
  import DependencyGraph.{FollowsNow, Weight, weightedArrow}

  require(components.contains(start), s"Start $start is not a component in ${components.keys.mkString(", ")}")

  private final val debugging = false

  private val order = components.size

  private var messages: List[String] = Nil
  def warning(message: String): Unit = messages ::= message
  def warnings: List[String] = messages.reverse.tap(_ => messages = Nil)

  /** For ith vertex, its outgoing edges. */
  private val adjacency: Array[List[Edge]] = Array.fill(order)(Nil)

  /** Directed edge. */
  private case class Edge(from: Int, to: Int, weight: Weight)

  // phase names and their vertex index
  private val names = components.keys.toArray
  private val nodes = names.zipWithIndex.toMap

  /** Add the edge between named phases, where `to` follows `from`.
   */
  private def addEdge(from: String, to: String, weight: Weight): Unit = {
    val v = nodes(from)
    val w = nodes(to)
    if (debugging) println(s"add edge $from ${weightedArrow(weight)} $to")
    adjacency(v).find(_.to == w) match {
      case None =>
        adjacency(v) ::= Edge(from = v, to = w, weight)
      case Some(_) if weight == FollowsNow => // retain runsRightAfter if there is a competing constraint
        adjacency(v) = Edge(from = v, to = w, weight) :: adjacency(v).filterNot(_.to == w)
      case _ => // ignore duplicate
    }
  }

  /** Find unreachable vertices.
   *
   *  Input must be acyclic and a vertex can have only one outgoing FollowsNow edge.
   */
  private def validated(): List[String] = {
    def checkFollowsNow(v: Int): Unit =
      adjacency(v).foldLeft(-1) { (w, e) =>
        if (e.weight != FollowsNow) w
        else if (w == -1) e.to
        else throw new FatalError(s"Phases ${names(w)} and ${names(e.to)} both immediately follow ${names(v)}")
      }
    val seen = Array.ofDim[Boolean](order)
    val onPath = Array.ofDim[Boolean](order)
    val stack = mutable.Stack.empty[(Int, List[Edge])] // a vertex and list of edges remaining to follow
    def walk(): Unit = {
      nodes(start).tap { start =>
        stack.push(start -> adjacency(start))
      }
      while (!stack.isEmpty) {
        val (v, edges) = stack.pop()
        if (!seen(v)) {
          checkFollowsNow(v)
          seen(v) = true
        }
        onPath(v) = true
        edges match {
          case Edge(_, to, _) :: edges =>
            if (onPath(to)) {
              var path = v :: to :: Nil
              while (path.head != to)
                path ::= stack.pop()._1
              throw new FatalError(s"Phases form a cycle: ${path.map(names(_)).mkString(" -> ")}")
            }
            stack.push(v -> edges)
            if (!seen(to))
              stack.push(to -> adjacency(to))
          case _ => onPath(v) = false
        }
      }
    }
    walk()
    val unseen = ListBuffer.empty[String]
    for (i <- 0.until(order) if !seen(i))
      unseen.addOne(names(i))
    unseen.toList
  }

  def compilerPhaseList(): List[SubComponent] = if (order == 1) List(components(start)) else {

    // distance from source to each vertex
    val distance = Array.fill[Int](order)(Int.MinValue)

    // incoming edge terminating in each vertex for the current path
    val edgeTo = Array.ofDim[Edge](order)

    // whether vertex is on the queue
    val enqueued = Array.ofDim[Boolean](order)

    // vertices to process
    val queue = mutable.Queue.empty[Int]

    def enqueue(v: Int): Unit = if (!enqueued(v)) queue.enqueue(v).tap(_ => enqueued(v) = true)

    def dequeue(): Int = queue.dequeue().tap(v => enqueued(v) = false)

    def namedEdge(e: Edge): String =
      if (e == null) "[no edge]"
      else s"${names(e.from)}<${distance(e.from)}> ${weightedArrow(e.weight)} ${names(e.to)}<${distance(e.to)}>"

    /** Remove a vertex from the queue and check outgoing edges:
     *  if an edge improves (increases) the distance at the terminus of the edge,
     *  record that as the new incoming edge to the terminus and enqueue that vertex
     *  to propagate updates.
     */
    def relax(): Unit = {
      nodes(start).tap { start =>
        distance(start) = 0
        enqueue(start)
      }
      while (!queue.isEmpty) {
        val v = dequeue()
        if (debugging) println(s"deq ${names(v)}")
        for (e <- adjacency(v)) {
          val w = e.to
          /* cannot happen as `runsRightAfter: Option[String]` is the only way to introduce a `FollowsNow`
          val e2 = edgeTo(w)
          if (e.weight == FollowsNow && e2 != null && e2.weight == FollowsNow && e.from != e2.from)
            throw new FatalError(s"${names(w)} cannot follow right after both ${names(e.from)} and ${names(e2.from)}")
          */
          if (distance(w) < distance(v) + e.weight) {
            distance(w) = distance(v) + e.weight
            edgeTo(w) = e
            enqueue(w)
            if (debugging) println(s"update ${namedEdge(e)} dist = ${distance(w)}, enq ${names(w)}")
          }
        }
      }
      if (debugging) edgeTo.foreach(e => println(namedEdge(e)))
    }

    /** Put the vertices in a linear order.
     *
     *  `Follows` edges increase the "level", `FollowsNow` don't.
     *  Partition by "level" or distance from start.
     *  Partition the level into "anchors" that follow a node in the previous level, and "followers" (nodes
     *    with a `FollowsNow` edge).
     *  Starting at the "ends", build the chains of `FollowsNow` nodes within the level. Each chain leads to an anchor.
     *  The anchors are sorted by name, then the chains are flattened.
     */
    def traverse(): List[SubComponent] = {
      def componentOf(i: Int) = components(names(i))
      def sortComponents(c: SubComponent, d: SubComponent): Boolean =
        // sort by name only, like the old implementation (pre scala/scala#10687)
        /*c.internal && !d.internal ||*/ c.phaseName.compareTo(d.phaseName) < 0
      def sortVertex(i: Int, j: Int): Boolean = sortComponents(componentOf(i), componentOf(j))

      val res = ListBuffer.empty[SubComponent]
      val levels = distance.zipWithIndex.groupBy(_._1) // distance -> (distance, node)
      for (level <- levels.keysIterator.filter(_ >= 0).toArray.sorted) {
        val vs = levels(level).map(_._2)
        val (anchors, followers) = vs.partition(v => edgeTo(v) == null || edgeTo(v).weight != FollowsNow)
        if (debugging) println(s"d=$level, anchors={${anchors.map(names(_)).mkString(",")}}, followers={${followers.map(names(_)).mkString(",")}}")
        if (followers.isEmpty)
          anchors.toArray.map(componentOf).sortWith(sortComponents).foreach(res.addOne(_))
        else {
          val froms = followers.map(v => edgeTo(v).from).toSet
          val ends = followers.iterator.filterNot(froms).toList
          val chains: Array[ArrayDeque[Int]] = anchors.map(ArrayDeque(_))
          def drill(v: Int, path: List[Int]): Unit =
            edgeTo(v) match {
              case e if e != null && e.weight == FollowsNow => drill(e.from, v :: path)
              case _ => chains.find(_.apply(0) == v).foreach(deque => path.foreach(deque.append))
            }
          ends.foreach(drill(_, Nil))
          for (chain <- chains.sortWith((p, q) => sortVertex(p(0), q(0))); v <- chain)
            res.addOne(componentOf(v))
        }
      }
      res.toList
    }
    for (p <- validated()) warning(s"Phase $p is not reachable from $start")
    relax()
    traverse()
  }
}
object DependencyGraph {

  type Weight = Int
  final val FollowsNow = 0
  final val Follows = 1
  def weightedArrow(w: Weight) = w match {
    case FollowsNow => "=>"
    case Follows    => "->"
    case _          => "?>"
  }

  final val Parser = "parser"
  final val Terminal = "terminal"

  private val compilerPhases = List(
    "parser", "namer", "packageobjects", "typer", "superaccessors", "extmethods", "pickler",
    "refchecks", "patmat", "uncurry", "fields", "tailcalls", "specialize", "explicitouter",
    "erasure", "posterasure", "lambdalift", "constructors", "flatten", "mixin", "cleanup",
    "delambdafy", "jvm", "terminal",
  )

  /** Create a DependencyGraph from the given phases. The graph must be acyclic.
   *
   *  A component must be declared as "initial".
   *  If no phase is "initial" but a phase is named "parser", it is taken as initial.
   *  If no phase is "terminal" but a phase is named "terminal", it is taken as terminal.
   *
   *  If a component declares that it follows (runsAfter or runsRightAfter)
   *  a phase that is not named in this run, then the component is omitted.
   *
   *  If a component declares that it precedes (runsBefore)
   *  a phase that is not named in this run, then the constraint is dropped.
   *
   *  Therefore, the graph contains only edges between the input components.
   *
   *  On construction (compilerPhaseList), unreachable components will incur warnings.
   *
   *  Apparent typos of phase names incur warnings.
   */
  def apply(components: Iterable[SubComponent], warnAll: Boolean = true): DependencyGraph = {
    val warnings = ListBuffer.empty[String]
    val phases = components.iterator.filter { p =>
      p.phaseName.nonEmpty.tap(b => if (!b) warnings.addOne(s"Dropped component with empty name (class ${p.getClass})"))
    }.toArray
    val phaseNames = phases.iterator.map(_.phaseName).toSet
    def isPhaseName(s: String): Boolean = phaseNames.contains(s)
    val start = phases.find(_.initial)
      .orElse(phases.find(_.phaseName == Parser))
      .getOrElse(throw new AssertionError("Missing initial component"))
    val end = phases.find(_.terminal)
      .orElse(phases.find(_.phaseName == Terminal))
      .getOrElse(throw new AssertionError("Missing terminal component"))
    var dropped = Set.empty[String]
    val constraints = ListBuffer.empty[(String, String, Weight)]
    def addConstraint(from: String, to: String, weight: Weight): Unit = constraints.addOne((from, to, weight))
    def checkConstraint(p: SubComponent)(name: String, constraint: String, warn: Boolean): Boolean = isPhaseName(name) || {
      if (warn) {
        val knownNames = phaseNames ++ compilerPhases
        val help = if (knownNames.contains(name)) "" else
          knownNames.filter(util.EditDistance.levenshtein(name, _) < 3).toList match {
            case Nil => ""
            case close => s" - did you mean ${util.StringUtil.oxford(close, "or")}?"
          }
        warnings.addOne(s"No phase `$name` for ${p.phaseName}.$constraint$help")
      }
      false
    }
    for ((p, i) <- phases.zipWithIndex) {
      for (after <- p.runsRightAfter if after.nonEmpty)
        if (checkConstraint(p)(after, "runsRightAfter", warn=warnAll)) addConstraint(after, p.phaseName, FollowsNow)
        else dropped += p.phaseName
      for (after <- p.runsAfter if after.nonEmpty && !p.runsRightAfter.contains(after))
        if (checkConstraint(p)(after, "runsAfter", warn=warnAll)) addConstraint(after, p.phaseName, Follows)
        else dropped += p.phaseName
      for (before <- p.runsBefore if before.nonEmpty)
        if (checkConstraint(p)(before, "runsBefore", warn=warnAll)) addConstraint(p.phaseName, before, Follows)
      // terminal follows every phase; parser is not before every phase because traverse uses edgeTo to find "anchors"
      if (p != end || p == end && p == start)
        addConstraint(p.phaseName, end.phaseName, Follows)
    }
    if (warnAll)
      dropped.foreach(p => warnings.addOne(s"Component ${p} dropped due to bad constraint"))
    val purgedConstraints = constraints.filterInPlace {
      case (from, to, w) => !dropped.contains(from) && !dropped.contains(to)
    }.toList
    new DependencyGraph(start.phaseName, phases.iterator.map(p => p.phaseName -> p).toMap).tap { graph =>
      for ((from, to, weight) <- purgedConstraints)
        graph.addEdge(from, to, weight)
      for (warning <- warnings)
        graph.warning(warning)
    }
  }

  /** Emit a graphviz dot file for the graph.
   *  Plug-in supplied phases are marked as green nodes and hard links are marked as blue edges.
   */
  def graphToDotFile(graph: DependencyGraph, file: File): Unit = {
    def color(hex: String) = s""" [color="#$hex"]"""
    val sb = ListBuffer.empty[String]
    sb.addOne("digraph G {")
    for (edges <- graph.adjacency; e <- edges)
      sb.addOne(s"${graph.names(e.from)} -> ${graph.names(e.to)}${if (e.weight == FollowsNow) color("0000ff") else ""}")
    for (n <- graph.names)
      sb.addOne(s"${n}${if (graph.components(n).internal) "" else color("00ff00")}")
    sb.addOne("}")
    file.printlnAll(sb.toList: _*)
  }
}

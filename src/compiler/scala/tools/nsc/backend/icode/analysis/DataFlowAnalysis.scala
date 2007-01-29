/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc.backend.icode.analysis

import scala.collection.mutable.{Map, HashMap, Set, HashSet, LinkedHashSet}

/** A generic framework for data flow analysis.
 */
trait DataFlowAnalysis[L <: CompleteLattice] {
  /** A type for program points. */
  type P <: ProgramPoint[P]
  val  lattice: L

  val worklist: Set[P] = new LinkedHashSet

  val in:  Map[P, lattice.Elem] = new HashMap
  val out: Map[P, lattice.Elem] = new HashMap
  val visited: HashSet[P] = new HashSet

  /* Implement this function to initialize the worklist.  */
  def init(f: => Unit): Unit = {
    in.clear; out.clear; worklist.clear; visited.clear;
    f
  }

  def run: Unit

  /** Implements forward dataflow analysis: the transfer function is
   *  applied when inputs to a Program point change, to obtain the new
   *  output value.
   *
   *  @param f the transfer function.
   */
  def forwardAnalysis(f: (P, lattice.Elem) => lattice.Elem): Unit =
    while (!worklist.isEmpty) {
//      Console.println("worklist in: " + worklist);
      val point = worklist.elements.next; worklist -= point; visited += point;
      val output = f(point, in(point))
//      Console.println("taking out point: " + point + " worklist out: " + worklist);

      if ((lattice.bottom == out(point)) || output != out(point)) {
//        Console.println("Output changed at " + point + " added to worklist: ")
//        Console.println("\t" + worklist)
        out(point) = output
        val succs = point.successors
        succs foreach { p =>
          if (!worklist.contains(p))
            worklist += p;
          in(p) = lattice.lub(in(p) :: (p.predecessors map out.apply))
        }
      }
    }

  /** ...
   *
   *  @param f ...
   */
  def backwardAnalysis(f: (P, lattice.Elem) => lattice.Elem): Unit =
    while (!worklist.isEmpty) {
      val point = worklist.elements.next; worklist -= point

      out(point) = lattice.lub(point.successors map in.apply)
      val input = f(point, out(point))

      if ((lattice.bottom == in(point)) || input != in(point)) {
        in(point) = input
        point.predecessors foreach { p =>
          if (!worklist.contains(p))
            worklist += p;
        }
      }
    }

}

/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc
package backend.icode.analysis

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

  /** collect statistics? */
  var stat = true

  /** the number of times we iterated before reaching a fixpoint. */
  var iterations = 0

  /* Implement this function to initialize the worklist.  */
  def init(f: => Unit): Unit = {
    iterations = 0
    in.clear; out.clear; worklist.clear; visited.clear;
    f
  }

  /** Reinitialize, but keep the old solutions. Should be used when reanalyzing the
   *  same method, after some code transformation.
   */
  def reinit(f: => Unit): Unit = {
    iterations = 0
    worklist.clear; visited.clear;
    f
  }

  def run: Unit

  /** Implements forward dataflow analysis: the transfer function is
   *  applied when inputs to a Program point change, to obtain the new
   *  output value.
   *
   *  @param f the transfer function.
   */
  def forwardAnalysis(f: (P, lattice.Elem) => lattice.Elem): Unit = try {
    while (!worklist.isEmpty) {
      if (stat) iterations += 1
      //Console.println("worklist in: " + worklist);
      val point = worklist.iterator.next; worklist -= point; visited += point;
      //Console.println("taking out point: " + point + " worklist out: " + worklist);
      val output = f(point, in(point))

      if ((lattice.bottom == out(point)) || output != out(point)) {
//        Console.println("Output changed at " + point
//                        + " from: " + out(point) + " to: " + output
//                        + " for input: " + in(point) + " and they are different: " + (output != out(point)))
        out(point) = output
        val succs = point.successors
        succs foreach { p =>
          if (!worklist.contains(p))
            worklist += p;
          in(p) = lattice.lub(/*in(p) :: */(p.predecessors map out.apply))
        }
      }
    }
  } catch {
    case e: NoSuchElementException =>
      Console.println("in: " + in.mkString("", "\n", ""))
      Console.println("out: " + out.mkString("", "\n", ""))
      e.printStackTrace
      Predef.error("Could not find element " + e.getMessage)
  }

  /** ...
   *
   *  @param f ...
   */
  def backwardAnalysis(f: (P, lattice.Elem) => lattice.Elem): Unit =
    while (!worklist.isEmpty) {
      if (stat) iterations += 1
      val point = worklist.iterator.next; worklist -= point

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

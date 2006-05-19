package scala.tools.nsc.backend.icode.analysis;

import scala.collection.mutable.{Map, HashMap, Set, HashSet};

/** A generic framework for data flow analysis.
 */
trait DataFlowAnalysis[L <: CompleteLattice] {
  /** A type for program points. */
  type P <: ProgramPoint[P];
  val  lattice: L;

  val worklist: Set[P] = new HashSet;

  val in:  Map[P, lattice.Elem] = new HashMap;
  val out: Map[P, lattice.Elem] = new HashMap;

  /* Implement this function to initialize the worklist.  */
  def init(f: => Unit): Unit = {
    in.clear; out.clear; worklist.clear;
    f;
  }

  def run: Unit;

  /** Implement forward dataflow analysis: the transfer function is
   * applied when inputs to a Program point change, to obtain the new
   * output value.
   */
  def forwardAnalysis(f: (P, lattice.Elem) => lattice.Elem): Unit = {
    while (!worklist.isEmpty) {
      val point = worklist.elements.next; worklist -= point;
      val output = f(point, in(point));

      if (out(point) == (lattice.bottom) || output != out(point)) {
        out(point) = output;
        val succs = point.successors;
        succs foreach { p =>
          if (!worklist.contains(p))
            worklist += p;
          in(p) = lattice.lub(in(p) :: (p.predecessors map out.apply))
        }
      }
    }
  }
}

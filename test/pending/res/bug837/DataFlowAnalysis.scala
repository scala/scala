package test;
import scala.collection.mutable._;
trait CompleteLattice {
  trait Elem;
}
trait DataFlowAnalysis[L <: CompleteLattice] {
  type P;
  val lattice : L;
  val out: Map[P, lattice.Elem] = new HashMap;
}
abstract class Liveness {
  object livenessLattice extends CompleteLattice;
  final class LivenessAnalysis extends DataFlowAnalysis[livenessLattice.type] {
    type P = String;
    val lattice = livenessLattice;
  }
}
package scala.tools.nsc;

abstract class StdPhase(prev: Phase) extends Phase(prev) {
  val global: Global;
  def run: unit =
    for (val unit <- global.units) apply(unit);
  def apply(unit: global.CompilationUnit): unit;
}



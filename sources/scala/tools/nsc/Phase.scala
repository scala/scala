package scala.tools.nsc;

abstract class Phase(val prev: Phase) {
  val id: int = if (prev == null) 0 else prev.id + 1;

  private var nx: Phase = NoPhase;
  if (prev != null) prev.nx = this;

  def next: Phase = nx;

  def name: String;
  def description: String = name;

  val flagMask: long = if (prev == null) 0L else prev.flagMask;
  def exactMatch: boolean = false;

  def run: unit;

  override def toString() = name;

  //  def check(units: List[CompilationUnit]): unit =
  //    for (val unit <- units; val checker <- checkers) checker.traverse(unit);  //  def checkers: List[Checker] = List();

}



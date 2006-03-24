package scala.tools.nsc.backend.icode.analysis;

/** Program points are locations in the program where we want to
 *  assert certain properties through data flow analysis, e.g.
 *  basic blocks.
 */
trait ProgramPoint[a <: ProgramPoint[a]] {
	def predecessors: List[a];
  def successors: List[a];
}

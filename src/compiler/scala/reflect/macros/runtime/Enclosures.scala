package scala.reflect.macros
package runtime

trait Enclosures {
  self: Context =>

  import universe._
  import mirror._

  private def site       = callsiteTyper.context
  private def enclTrees  = site.enclosingContextChain map (_.tree)
  private def enclPoses  = enclosingMacros map (_.macroApplication.pos) filterNot (_ eq NoPosition)

  // vals are eager to simplify debugging
  // after all we wouldn't save that much time by making them lazy
  val macroApplication: Tree                      = expandee
  val enclosingClass: Tree                        = enclTrees collectFirst { case x: ImplDef => x } getOrElse EmptyTree
  val enclosingImplicits: List[ImplicitCandidate] = site.openImplicits.map(_.toImplicitCandidate)
  val enclosingMacros: List[Context]              = this :: universe.analyzer.openMacros // include self
  val enclosingMethod: Tree                       = site.enclMethod.tree
  val enclosingPosition: Position                 = if (enclPoses.isEmpty) NoPosition else enclPoses.head.pos
  val enclosingUnit: CompilationUnit              = universe.currentRun.currentUnit
  val enclosingRun: Run                           = universe.currentRun
}

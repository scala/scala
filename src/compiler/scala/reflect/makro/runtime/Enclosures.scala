package scala.reflect.makro
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
  val macroApplication: Tree                 = expandee
  val enclosingApplication: Tree             = enclTrees collectFirst { case t: Apply => t } getOrElse EmptyTree
  val enclosingClass: Tree                   = site.enclClass.tree
  val enclosingImplicits: List[(Type, Tree)] = site.openImplicits
  val enclosingMacros: List[Context]         = this :: universe.analyzer.openMacros // include self
  val enclosingMethod: Tree                  = site.enclMethod.tree
  val enclosingPosition: Position            = if (enclPoses.isEmpty) NoPosition else enclPoses.head.pos
  val enclosingUnit: CompilationUnit         = currentRun.currentUnit
}

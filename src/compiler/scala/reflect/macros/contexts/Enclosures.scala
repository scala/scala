package scala.reflect.macros
package contexts

import scala.reflect.{ClassTag, classTag}

trait Enclosures {
  self: Context =>

  import universe._

  private lazy val site       = callsiteTyper.context
  private lazy val enclTrees  = site.enclosingContextChain map (_.tree)
  private lazy val enclPoses  = enclosingMacros map (_.macroApplication.pos) filterNot (_ eq NoPosition)

  private def lenientEnclosure[T <: Tree : ClassTag]: Tree = enclTrees collectFirst { case x: T => x } getOrElse EmptyTree
  private def strictEnclosure[T <: Tree : ClassTag]: T = enclTrees collectFirst { case x: T => x } getOrElse (throw new EnclosureException(classTag[T].runtimeClass, enclTrees))

  // vals are eager to simplify debugging
  // after all we wouldn't save that much time by making them lazy
  val macroApplication: Tree                      = expandee
  def enclosingPackage: PackageDef                = strictEnclosure[PackageDef]
  val enclosingClass: Tree                        = lenientEnclosure[ImplDef]
  def enclosingImpl: ImplDef                      = strictEnclosure[ImplDef]
  def enclosingTemplate: Template                 = strictEnclosure[Template]
  val enclosingImplicits: List[ImplicitCandidate] = site.openImplicits.map(_.toImplicitCandidate)
  val enclosingMacros: List[Context]              = this :: universe.analyzer.openMacros // include self
  val enclosingMethod: Tree                       = lenientEnclosure[DefDef]
  def enclosingDef: DefDef                        = strictEnclosure[DefDef]
  val enclosingPosition: Position                 = if (enclPoses.isEmpty) NoPosition else enclPoses.head.pos
  val enclosingUnit: CompilationUnit              = universe.currentRun.currentUnit
  val enclosingRun: Run                           = universe.currentRun
}

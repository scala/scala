package scala.reflect.macros
package contexts

trait Typers {
  self: Context =>

  def openMacros: List[Context] = this :: universe.analyzer.openMacros

  def openImplicits: List[ImplicitCandidate] = callsiteTyper.context.openImplicits.map(_.toImplicitCandidate)

  type TypecheckMode = scala.reflect.internal.Mode
  val TypecheckMode = scala.reflect.internal.Mode
  val TERMmode = TypecheckMode.EXPRmode
  val TYPEmode = TypecheckMode.TYPEmode | TypecheckMode.FUNmode
  val PATTERNmode = TypecheckMode.PATTERNmode

  /**
   * @see [[scala.tools.reflect.ToolBox.typeCheck]]
   */
  def typecheck(tree: Tree, mode: TypecheckMode = TERMmode, pt: Type = universe.WildcardType, silent: Boolean = false, withImplicitViewsDisabled: Boolean = false, withMacrosDisabled: Boolean = false): Tree = {
    macroLogVerbose("typechecking %s with expected type %s, implicit views = %s, macros = %s".format(tree, pt, !withImplicitViewsDisabled, !withMacrosDisabled))
    val context = callsiteTyper.context
    val withImplicitFlag = if (!withImplicitViewsDisabled) (context.withImplicitsEnabled[Tree] _) else (context.withImplicitsDisabled[Tree] _)
    val withMacroFlag = if (!withMacrosDisabled) (context.withMacrosEnabled[Tree] _) else (context.withMacrosDisabled[Tree] _)
    def withContext(tree: => Tree) = withImplicitFlag(withMacroFlag(tree))
    def withWrapping(tree: Tree)(op: Tree => Tree) = if (mode == TERMmode) universe.wrappingIntoTerm(tree)(op) else op(tree)
    def typecheckInternal(tree: Tree) = callsiteTyper.silent(_.typed(universe.duplicateAndKeepPositions(tree), mode, pt), reportAmbiguousErrors = false)
    withWrapping(tree)(wrappedTree => withContext(typecheckInternal(wrappedTree) match {
      case universe.analyzer.SilentResultValue(result) =>
        macroLogVerbose(result)
        result
      case error @ universe.analyzer.SilentTypeError(_) =>
        macroLogVerbose(error.err.errMsg)
        if (!silent) throw new TypecheckException(error.err.errPos, error.err.errMsg)
        universe.EmptyTree
    }))
  }

  def inferImplicitValue(pt: Type, silent: Boolean = true, withMacrosDisabled: Boolean = false, pos: Position = enclosingPosition): Tree = {
    macroLogVerbose("inferring implicit value of type %s, macros = %s".format(pt, !withMacrosDisabled))
    universe.analyzer.inferImplicit(universe.EmptyTree, pt, false, callsiteTyper.context, silent, withMacrosDisabled, pos, (pos, msg) => throw TypecheckException(pos, msg))
  }

  def inferImplicitView(tree: Tree, from: Type, to: Type, silent: Boolean = true, withMacrosDisabled: Boolean = false, pos: Position = enclosingPosition): Tree = {
    macroLogVerbose("inferring implicit view from %s to %s for %s, macros = %s".format(from, to, tree, !withMacrosDisabled))
    val viewTpe = universe.appliedType(universe.definitions.FunctionClass(1).toTypeConstructor, List(from, to))
    universe.analyzer.inferImplicit(tree, viewTpe, true, callsiteTyper.context, silent, withMacrosDisabled, pos, (pos, msg) => throw TypecheckException(pos, msg))
  }

  def resetLocalAttrs(tree: Tree): Tree = universe.resetAttrs(universe.duplicateAndKeepPositions(tree))

  def untypecheck(tree: Tree): Tree = resetLocalAttrs(tree)
}

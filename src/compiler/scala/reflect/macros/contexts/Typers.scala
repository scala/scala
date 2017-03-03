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
    macroLogVerbose(s"typechecking $tree with expected type $pt, implicit views = ${!withImplicitViewsDisabled}, macros = ${!withMacrosDisabled}")
    import callsiteTyper.context
    def doTypecheck(wrapped: Tree): Tree =
      context.withImplicits(enabled = !withImplicitViewsDisabled) {
        context.withMacros(enabled = !withMacrosDisabled) {
          callsiteTyper.silent(_.typed(universe.duplicateAndKeepPositions(wrapped), mode, pt), reportAmbiguousErrors = false) match {
            case universe.analyzer.SilentResultValue(result) =>
              macroLogVerbose(result)
              result
            case error@universe.analyzer.SilentTypeError(_) =>
              macroLogVerbose(error.err.errMsg)
              if (!silent) throw new TypecheckException(error.err.errPos, error.err.errMsg)
              universe.EmptyTree
          }
        }
      }

    if (mode == TERMmode) universe.wrappingIntoTerm(tree)(doTypecheck)
    else doTypecheck(tree)
  }

  def inferImplicitValue(pt: Type, silent: Boolean = true, withMacrosDisabled: Boolean = false, pos: Position = enclosingPosition): Tree = {
    macroLogVerbose(s"inferring implicit value of type $pt, macros = ${!withMacrosDisabled}")
    universe.analyzer.inferImplicit(universe.EmptyTree, pt, false, callsiteTyper.context, silent, withMacrosDisabled, pos, (pos, msg) => throw TypecheckException(pos, msg))
  }

  def inferImplicitView(tree: Tree, from: Type, to: Type, silent: Boolean = true, withMacrosDisabled: Boolean = false, pos: Position = enclosingPosition): Tree = {
    macroLogVerbose(s"inferring implicit view from $from to $to for $tree, macros = ${!withMacrosDisabled}")
    val viewTpe = universe.appliedType(universe.definitions.FunctionClass(1).toTypeConstructor, List(from, to))
    universe.analyzer.inferImplicit(tree, viewTpe, true, callsiteTyper.context, silent, withMacrosDisabled, pos, (pos, msg) => throw TypecheckException(pos, msg))
  }

  def resetLocalAttrs(tree: Tree): Tree = universe.resetAttrs(universe.duplicateAndKeepPositions(tree))

  def untypecheck(tree: Tree): Tree = resetLocalAttrs(tree)
}

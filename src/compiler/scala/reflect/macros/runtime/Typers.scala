package scala.reflect.macros
package runtime

trait Typers {
  self: Context =>

  def openMacros: List[Context] = this :: universe.analyzer.openMacros

  def openImplicits: List[ImplicitCandidate] = callsiteTyper.context.openImplicits.map(_.toImplicitCandidate)

  /**
   * @see [[scala.tools.reflect.Toolbox.typeCheck]]
   */
  def typeCheck(tree: Tree, pt: Type = universe.WildcardType, silent: Boolean = false, withImplicitViewsDisabled: Boolean = false, withMacrosDisabled: Boolean = false): Tree = {
    macroLogVerbose("typechecking %s with expected type %s, implicit views = %s, macros = %s".format(tree, pt, !withImplicitViewsDisabled, !withMacrosDisabled))
    val context = callsiteTyper.context
    val wrapper1 = if (!withImplicitViewsDisabled) (context.withImplicitsEnabled[Tree] _) else (context.withImplicitsDisabled[Tree] _)
    val wrapper2 = if (!withMacrosDisabled) (context.withMacrosEnabled[Tree] _) else (context.withMacrosDisabled[Tree] _)
    def wrapper (tree: => Tree) = wrapper1(wrapper2(tree))
    // if you get a "silent mode is not available past typer" here
    // don't rush to change the typecheck not to use the silent method when the silent parameter is false
    // typechecking uses silent anyways (e.g. in typedSelect), so you'll only waste your time
    // I'd advise fixing the root cause: finding why the context is not set to report errors
    // (also see reflect.runtime.ToolBoxes.typeCheckExpr for a workaround that might work for you)
    wrapper(callsiteTyper.silent(_.typed(tree, universe.analyzer.EXPRmode, pt), reportAmbiguousErrors = false) match {
      case universe.analyzer.SilentResultValue(result) =>
        macroLogVerbose(result)
        result
      case error @ universe.analyzer.SilentTypeError(_) =>
        macroLogVerbose(error.err.errMsg)
        if (!silent) throw new TypecheckException(error.err.errPos, error.err.errMsg)
        universe.EmptyTree
    })
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

  def resetAllAttrs(tree: Tree): Tree = universe.resetAllAttrs(tree)

  def resetLocalAttrs(tree: Tree): Tree = universe.resetLocalAttrs(tree)
}

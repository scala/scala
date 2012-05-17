package scala.reflect.makro
package runtime

trait Typers {
  self: Context =>

  def openMacros: List[Context] = this :: mirror.analyzer.openMacros

  def openImplicits: List[(Type, Tree)] = callsiteTyper.context.openImplicits

  def typeCheck(tree: Tree, pt: Type = mirror.WildcardType, silent: Boolean = false, withImplicitViewsDisabled: Boolean = false, withMacrosDisabled: Boolean = false): Tree = {
    macroLogVerbose("typechecking %s with expected type %s, implicit views = %s, macros = %s".format(tree, pt, !withImplicitViewsDisabled, !withMacrosDisabled))
    val wrapper1 = if (!withImplicitViewsDisabled) (callsiteTyper.context.withImplicitsEnabled[Tree] _) else (callsiteTyper.context.withImplicitsDisabled[Tree] _)
    val wrapper2 = if (!withMacrosDisabled) (callsiteTyper.context.withMacrosEnabled[Tree] _) else (callsiteTyper.context.withMacrosDisabled[Tree] _)
    def wrapper (tree: => Tree) = wrapper1(wrapper2(tree))
    // if you get a "silent mode is not available past typer" here
    // don't rush to change the typecheck not to use the silent method when the silent parameter is false
    // typechecking uses silent anyways (e.g. in typedSelect), so you'll only waste your time
    // I'd advise fixing the root cause: finding why the context is not set to report errors
    // (also see reflect.runtime.ToolBoxes.typeCheckExpr for a workaround that might work for you)
    wrapper(callsiteTyper.silent(_.typed(tree, mirror.analyzer.EXPRmode, pt)) match {
      case mirror.analyzer.SilentResultValue(result) =>
        macroLogVerbose(result)
        result
      case error @ mirror.analyzer.SilentTypeError(_) =>
        macroLogVerbose(error.err.errMsg)
        if (!silent) throw new mirror.TypeError(error.err.errPos, error.err.errMsg)
        mirror.EmptyTree
    })
  }

  def inferImplicitValue(pt: Type, silent: Boolean = true, withMacrosDisabled: Boolean = false, pos: Position = enclosingPosition): Tree = {
    macroLogVerbose("inferring implicit value of type %s, macros = %s".format(pt, !withMacrosDisabled))
    import mirror.analyzer.SearchResult
    val context = callsiteTyper.context
    val wrapper1 = if (!withMacrosDisabled) (context.withMacrosEnabled[SearchResult] _) else (context.withMacrosDisabled[SearchResult] _)
    def wrapper (inference: => SearchResult) = wrapper1(inference)
    wrapper(mirror.analyzer.inferImplicit(mirror.EmptyTree, pt, true, false, context, !silent, pos)) match {
      case failure if failure.tree.isEmpty =>
        macroLogVerbose("implicit search has failed. to find out the reason, turn on -Xlog-implicits")
        if (context.hasErrors) throw new mirror.TypeError(context.errBuffer.head.errPos, context.errBuffer.head.errMsg)
        mirror.EmptyTree
      case success =>
        success.tree
    }
  }

  def inferImplicitView(tree: Tree, from: Type, to: Type, silent: Boolean = true, withMacrosDisabled: Boolean = false, reportAmbiguous: Boolean = true, pos: Position = enclosingPosition): Tree = {
    macroLogVerbose("inferring implicit view from %s to %s for %s, macros = %s, reportAmbiguous = %s".format(from, to, tree, !withMacrosDisabled, reportAmbiguous))
    import mirror.analyzer.SearchResult
    val context = callsiteTyper.context
    val wrapper1 = if (!withMacrosDisabled) (context.withMacrosEnabled[SearchResult] _) else (context.withMacrosDisabled[SearchResult] _)
    def wrapper (inference: => SearchResult) = wrapper1(inference)
    val fun1 = mirror.definitions.FunctionClass(1)
    val viewTpe = mirror.TypeRef(fun1.typeConstructor.prefix, fun1, List(from, to))
    wrapper(mirror.analyzer.inferImplicit(tree, viewTpe, reportAmbiguous, true, context, !silent, pos)) match {
      case failure if failure.tree.isEmpty =>
        macroLogVerbose("implicit search has failed. to find out the reason, turn on -Xlog-implicits")
        if (context.hasErrors) throw new mirror.TypeError(context.errBuffer.head.errPos, context.errBuffer.head.errMsg)
        mirror.EmptyTree
      case success =>
        success.tree
    }
  }

  type TypeError = mirror.TypeError

  object TypeError extends TypeErrorExtractor {
    def unapply(error: TypeError): Option[(Position, String)] = Some((error.pos, error.msg))
  }

  def resetAllAttrs(tree: Tree): Tree = mirror.resetAllAttrs(tree)

  def resetLocalAttrs(tree: Tree): Tree = mirror.resetLocalAttrs(tree)
}
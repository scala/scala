package scala.reflect.makro
package runtime

trait Typers {
  self: Context =>

  val openMacros: List[Context] = this :: mirror.analyzer.openMacros

  val openImplicits: List[(Type, Tree)] = callsiteTyper.context.openImplicits

  def typeCheck(tree: Tree, pt: Type = mirror.WildcardType, silent: Boolean = false, withImplicitViewsDisabled: Boolean = false, withMacrosDisabled: Boolean = false): Tree = {
    def trace(msg: Any) = if (mirror.settings.Ymacrodebug.value) println(msg)
    trace("typechecking %s with expected type %s, implicit views = %s, macros = %s".format(tree, pt, !withImplicitViewsDisabled, !withMacrosDisabled))
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
        trace(result)
        result
      case error @ mirror.analyzer.SilentTypeError(_) =>
        trace(error.err.errMsg)
        if (!silent) throw new mirror.TypeError(error.err.errPos, error.err.errMsg)
        mirror.EmptyTree
    })
  }

  def inferImplicitValue(pt: Type, silent: Boolean = true, withMacrosDisabled: Boolean = false, pos: Position = enclosingPosition): Tree = {
    def trace(msg: Any) = if (mirror.settings.Ymacrodebug.value) println(msg)
    trace("inferring implicit value of type %s, macros = %s".format(pt, !withMacrosDisabled))
    import mirror.analyzer.SearchResult
    val wrapper1 = if (!withMacrosDisabled) (callsiteTyper.context.withMacrosEnabled[SearchResult] _) else (callsiteTyper.context.withMacrosDisabled[SearchResult] _)
    def wrapper (inference: => SearchResult) = wrapper1(inference)
    val context = callsiteTyper.context.makeImplicit(true)
    wrapper(mirror.analyzer.inferImplicit(mirror.EmptyTree, pt, true, false, context, !silent, pos)) match {
      case failure if failure.tree.isEmpty =>
        trace("implicit search has failed. to find out the reason, turn on -Xlog-implicits")
        if (context.hasErrors) throw new mirror.TypeError(context.errBuffer.head.errPos, context.errBuffer.head.errMsg)
        mirror.EmptyTree
      case success =>
        success.tree
    }
  }

  def inferImplicitView(tree: Tree, from: Type, to: Type, silent: Boolean = true, withMacrosDisabled: Boolean = false, reportAmbiguous: Boolean = true, pos: Position = enclosingPosition): Tree = {
    def trace(msg: Any) = if (mirror.settings.Ymacrodebug.value) println(msg)
    trace("inferring implicit view from %s to %s for %s, macros = %s, reportAmbiguous = %s".format(from, to, tree, !withMacrosDisabled, reportAmbiguous))
    import mirror.analyzer.SearchResult
    val wrapper1 = if (!withMacrosDisabled) (callsiteTyper.context.withMacrosEnabled[SearchResult] _) else (callsiteTyper.context.withMacrosDisabled[SearchResult] _)
    def wrapper (inference: => SearchResult) = wrapper1(inference)
    val fun1 = mirror.definitions.FunctionClass(1)
    val viewTpe = mirror.TypeRef(fun1.typeConstructor.prefix, fun1, List(from, to))
    val context = callsiteTyper.context.makeImplicit(reportAmbiguous)
    wrapper(mirror.analyzer.inferImplicit(mirror.EmptyTree, viewTpe, reportAmbiguous, true, context, !silent, pos)) match {
      case failure if failure.tree.isEmpty =>
        trace("implicit search has failed. to find out the reason, turn on -Xlog-implicits")
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

  def resetAllAttrs[T <: Tree](tree: T): T = mirror.resetAllAttrs(tree)

  def resetLocalAttrs[T <: Tree](tree: T): T = mirror.resetLocalAttrs(tree)
}
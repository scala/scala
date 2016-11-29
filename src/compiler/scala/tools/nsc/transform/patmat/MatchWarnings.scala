/* NSC -- new Scala compiler
 *
 * Copyright 2011-2013 LAMP/EPFL
 * @author Adriaan Moors
 */

package scala.tools.nsc.transform.patmat

trait MatchWarnings {
  self: PatternMatching =>

  import global._

  trait TreeMakerWarnings {
    self: MatchTranslator =>

    import typer.context

    // Why is it so difficult to say "here's a name and a context, give me any
    // matching symbol in scope" ? I am sure this code is wrong, but attempts to
    // use the scopes of the contexts in the enclosing context chain discover
    // nothing. How to associate a name with a symbol would would be a wonderful
    // linkage for which to establish a canonical acquisition mechanism.
    private def matchingSymbolInScope(pat: Tree): Symbol = {
      def declarationOfName(tpe: Type, name: Name): Symbol = tpe match {
        case PolyType(tparams, restpe)  => tparams find (_.name == name) getOrElse declarationOfName(restpe, name)
        case MethodType(params, restpe) => params find (_.name == name) getOrElse declarationOfName(restpe, name)
        case ClassInfoType(_, _, clazz) => clazz.rawInfo member name
        case _                          => NoSymbol
      }
      pat match {
        case Bind(name, _) =>
          context.enclosingContextChain.foldLeft(NoSymbol: Symbol)((res, ctx) =>
            res orElse declarationOfName(ctx.owner.rawInfo, name))
        case _ => NoSymbol
      }
    }

    // Issue better warnings than "unreachable code" when people misuse
    // variable patterns thinking they bind to existing identifiers.
    //
    // Possible TODO: more deeply nested variable patterns, like
    //   case (a, b) => 1 ; case (c, d) => 2
    // However this is a pain (at least the way I'm going about it)
    // and I have to think these detailed errors are primarily useful
    // for beginners, not people writing nested pattern matches.
    def checkMatchVariablePatterns(cases: List[CaseDef]) {
      // A string describing the first variable pattern
      var vpat: String = null
      // Using an iterator so we can recognize the last case
      val it = cases.iterator

      def addendum(pat: Tree) = {
        matchingSymbolInScope(pat) match {
          case NoSymbol   => ""
          case sym        =>
            val desc = if (sym.isParameter) s"parameter ${sym.nameString} of" else sym + " in"
            s"\nIf you intended to match against $desc ${sym.owner}, you must use backticks, like: case `${sym.nameString}` =>"
        }
      }

      while (it.hasNext) {
        val cdef = it.next()
        // If a default case has been seen, then every succeeding case is unreachable.
        if (vpat != null)
          reporter.warning(cdef.body.pos, "unreachable code due to " + vpat + addendum(cdef.pat)) // TODO: make configurable whether this is an error
        // If this is a default case and more cases follow, warn about this one so
        // we have a reason to mention its pattern variable name and any corresponding
        // symbol in scope.  Errors will follow from the remaining cases, at least
        // once we make the above warning an error.
        else if (it.hasNext && (treeInfo isDefaultCase cdef)) {
          val vpatName = cdef.pat match {
            case Bind(name, _)   => s" '$name'"
            case _               => ""
          }
          vpat = s"variable pattern$vpatName on line ${cdef.pat.pos.line}"
          reporter.warning(cdef.pos, s"patterns after a variable pattern cannot match (SLS 8.1.1)" + addendum(cdef.pat))
        }
      }
    }
  }
}

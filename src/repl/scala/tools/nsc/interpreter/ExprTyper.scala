/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

trait ExprTyper {
  val repl: IMain

  import repl._
  import global.{ reporter => _, Import => _, _ }
  import naming.freshInternalVarName
  import global.definitions.{ MaxFunctionArity, NothingTpe }

  private def doInterpret(code: String): IR.Result = {
    // interpret/interpretSynthetic may change the phase, which would have unintended effects on types.
    val savedPhase = phase
    try interpretSynthetic(code) finally phase = savedPhase
  }

  def symbolOfLine(code: String): Symbol = {
    def asExpr(): Symbol = {
      val name  = freshInternalVarName()
      // Typing it with a lazy val would give us the right type, but runs
      // into compiler bugs with things like existentials, so we compile it
      // behind a def and strip the NullaryMethodType which wraps the expr.
      val line = "def " + name + " = " + code

      doInterpret(line) match {
        case IR.Success =>
          val sym0 = symbolOfTerm(name)
          // drop NullaryMethodType
          sym0.cloneSymbol setInfo exitingTyper(sym0.tpe_*.finalResultType)
        case _          => NoSymbol
      }
    }
    def asDefn(): Symbol = {
      val old = repl.definedSymbolList.toSet

      doInterpret(code) match {
        case IR.Success =>
          repl.definedSymbolList filterNot old match {
            case Nil        => NoSymbol
            case sym :: Nil => sym
            case syms       => NoSymbol.newOverloaded(NoPrefix, syms)
          }
        case _ => NoSymbol
      }
    }
    def asError(): Symbol = {
      doInterpret(code)
      NoSymbol
    }
    beSilentDuring(asExpr()) orElse beSilentDuring(asDefn()) orElse asError()
  }

  private var typeOfExpressionDepth = 0
  def typeOfExpression(expr: String, silent: Boolean = true): Type = {
    if (typeOfExpressionDepth > 2) {
      repldbg("Terminating typeOfExpression recursion for expression: " + expr)
      return NoType
    }
    typeOfExpressionDepth += 1
    // Don't presently have a good way to suppress undesirable success output
    // while letting errors through, so it is first trying it silently: if there
    // is an error, and errors are desired, then it re-evaluates non-silently
    // to induce the error message.
    try beSilentDuring(symbolOfLine(expr).tpe) match {
      case NoType if !silent => symbolOfLine(expr).tpe // generate error
      case tpe               => tpe
    }
    finally typeOfExpressionDepth -= 1
  }

  // Try typeString[Nothing], typeString[Nothing, Nothing], etc.
  def typeOfTypeString(typeString: String): Type = {
    val properTypeOpt = typeOfProperTypeString(typeString)
    def typeFromTypeString(n: Int): Option[Type] = {
      val ts = typeString + List.fill(n)("_root_.scala.Nothing").mkString("[", ", ", "]")
      val tpeOpt = typeOfProperTypeString(ts)
      tpeOpt map {
        // Type lambda is detected. Substitute Nothing with WildcardType.
        case TypeRef(pre, sym, args) if args.size != n =>
          TypeRef(pre, sym, args map {
            case NothingTpe => WildcardType
            case t          => t
          })
        case TypeRef(pre, sym, args) => TypeRef(pre, sym, Nil)
        case tpe                     => tpe
      }
    }
    val typeOpt = (properTypeOpt /: (1 to MaxFunctionArity)) {
      (acc, n: Int) => acc orElse typeFromTypeString(n) }
    typeOpt getOrElse NoType
  }

  // This only works for proper types.
  private[interpreter] def typeOfProperTypeString(typeString: String): Option[Type] = {
    def asProperType(): Option[Type] = {
      val name = freshInternalVarName()
      val line = s"def $name: $typeString = ???"
      doInterpret(line) match {
        case IR.Success =>
          val tpe0 = exitingTyper {
            symbolOfTerm(name).asMethod.returnType
          }
          Some(tpe0)
        case _          => None
      }
    }
    beSilentDuring(asProperType())
  }
}

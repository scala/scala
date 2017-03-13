package scala.tools.nsc
package ast.parser

class MatchRewriter[G <: scala.tools.nsc.Global](val global: G) {
  import global._

  /** Entry point. */
  object rewrite extends Transformer {
    def apply(t: Tree): Tree = {
      logResult("Transforming: " + t)(transform(t))
    }
    override def transform(t: Tree): Tree = t match {
      case m: Match => new TranslateMatch(super.transform(m).asInstanceOf[Match]) resultTree
      case _        => super.transform(t)
    }
  }

  object CaseDecompose {
    /** That's right, the compiler generates a wrong AppliedTypeTree
     *  instead of a TypeApply tree when there are type args in a pattern.
     *  "We can rebuild it... we have the technology."
     *
     *  Unfortunately fixing it at the source revealed many other
     *  points in the compiler which depend on things happening as
     *  they presently do. You, dear reader, are welcome to try it.
     */
    def makeTermTree(t: Tree): Tree = t match {
      case AppliedTypeTree(fun, args) => TypeApply(makeTermTree(fun), args)
      case Select(qual, name)         => Select(makeTermTree(qual), name.toTermName)
      case Ident(name)                => Ident(name.toTermName)
    }
    def unapply(t: Tree) = t match {
      case CaseDef(Apply(AppliedTypeTree(fun, targs), patternVars), guard, expr) =>
        Some((makeTermTree(fun), targs, patternVars, guard, expr))
      case CaseDef(Apply(fn, patternVars), guard, expr) =>
        Some((fn, Nil, patternVars, guard, expr))
      case CaseDef(pat, guard, expr) =>
        Some((pat, Nil, Nil, guard, expr))
      case _ =>
        None
    }
  }

  class TranslateMatch(m: Match) {
    val Match(expr, cases) = m

    /** The list of valdefs to prepend to the rewritten match. */
    var valDefs = Vector[Tree]()

    /** Memoize the scrutinee expression. */
    lazy val exprVal = currentUnit.freshTermName("Scrutinee")
    lazy val expr1   = q"val $exprVal = $expr"

    /** The transformed cases, side effecting valDefs as we go. */
    val cases1 = cases map (x => (TranslateCaseDefs transform x).asInstanceOf[CaseDef])

    /** The rewritten pattern match, if any type argument patterns were pulled out. */
    val resultTree = valDefs match {
      case Seq() => m
      case _     => q"{ $expr1 ; ..$valDefs ; $exprVal match { case ..$cases1 } }"
    }

    object TranslateCaseDefs extends Transformer {
      def apply(cd: CaseDef): CaseDef = transform(cd).asInstanceOf[CaseDef]

      override def transform(t: Tree): Tree = t match {
        case CaseDecompose(selector, targs, patternVars, guard, rhs) if targs.nonEmpty =>
          log(s"CaseDecompose($selector, $targs, $patternVars, $guard, $rhs")

          val stableName   = currentUnit.freshTermName("TypeArgsPattern")
          val unapplyCall  = q"$selector.unapply[..$targs]($exprVal)"
          val stableValDef = q"lazy val $stableName = $unapplyCall"

          def isMatch: Tree = patternVars.size match {
            case 0 => q"$stableName"
            case _ => q"!$stableName.isEmpty"
          }
          def accessorTree(n: Int): Tree = patternVars match {
            case Seq()  => q"()"
            case Seq(_) => q"$stableName.get"
            case _      => val acc = TermName("_" + ( n + 1 )) ; q"$stableName.get.$acc"
          }

          val newGuard = if (guard.isEmpty) isMatch else q"$isMatch && $guard"
          val preRhs   = (
            for ((pat, i) <- patternVars.zipWithIndex) yield
              unBlockOne(q"val $pat = ${ accessorTree(i) }")
          )
          val allRhs = preRhs :+ rhs flatMap unBlock
          valDefs    = valDefs :+ stableValDef

          logResult("Rewrote CaseDef")(cq"_ if $newGuard => ..$allRhs")

        case _ => super.transform(t)
      }
    }
  }

  object BlockStats {
    def unapply(t: Tree): Option[List[Tree]] = t match {
      case Block(stats, EmptyTree | Literal(Constant(()))) => Some(stats)
      case _                                               => None
    }
  }
  def unBlockOne(t: Tree): Tree = t match {
    case BlockStats(stat :: Nil) => stat
    case _                       => t
  }
  def unBlock(t: Tree): List[Tree] = t match {
    case BlockStats(stats) => stats
    case t                 => List(t)
  }
}

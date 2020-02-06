/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.transform.async

import scala.reflect.internal.Flags

private[async] trait AnfTransform extends TransformUtils {
  import global._
  final def anfTransform(tree: Tree, owner: Symbol): Block = {
    val trans = new AnfTransform(owner)
    // Must prepend the () for issue #31.
    val block = typecheck(atPos(tree.pos)(Block(List(literalUnit), tree))).setType(tree.tpe)
    val tree1 = adjustTypeOfTranslatedPatternMatches(block, owner)
    trans.transformAtOwner(owner, tree1).asInstanceOf[Block]
  }

  class AnfTransform(owner: Symbol) extends TypingTransformer(currentTransformState.unit) {

    sealed abstract class AnfMode

    case object Anf extends AnfMode

    case object Linearizing extends AnfMode

    var mode: AnfMode = Anf

    object trace {
      private var indent = -1

      private def indentString = "  " * indent

      def apply[T](args: Any)(t: => T): T = {
        def prefix = mode.toString.toLowerCase

        indent += 1

        def oneLine(s: Any) = s.toString.replaceAll("""\n""", "\\\\n").take(127)

        try {
          if (AsyncUtils.trace)
            AsyncUtils.trace(s"$indentString$prefix(${oneLine(args)})")
          val result = t
          if (AsyncUtils.trace)
            AsyncUtils.trace(s"$indentString= ${oneLine(result)}")
          result
        } finally {
          indent -= 1
        }
      }
    }

    def typed(tree: Tree) = localTyper.typed(tree)

    def typedAt(exprPos: Position, tree: Tree) = localTyper.typed(atPos(exprPos)(tree))

    def typedAssign(lhs: Tree, varSym: Symbol) =
      typedAt(lhs.pos, Assign(Ident(varSym), lhs))

    object linearize {
      def transformToList(tree: Tree): List[Tree] = {
        mode = Linearizing;
        blockToList(transform(tree))
      }

      def transformToBlock(tree: Tree): Block = listToBlock(transformToList(tree))

      def _transformToList(tree: Tree): List[Tree] = trace(tree) {
        val stats :+ expr = _anf.transformToList(tree)

        def statsExprUnit = {
          stats :+ expr :+ typedAt(expr.pos, literalUnit)
        }

        def statsExprThrow =
          stats :+ expr :+ typedAt(expr.pos, Throw(Apply(Select(New(gen.mkAttributedRef(IllegalStateExceptionClass)), nme.CONSTRUCTOR), Nil)))

        expr match {
          case Apply(fun, args) if isAwait(fun) =>
            val awaitResType = transformType(expr.tpe)
            val valDef = defineVal(name.await(), expr, tree.pos)(awaitResType)
            val ref = gen.mkAttributedStableRef(valDef.symbol).setType(awaitResType)
            // https://github.com/scala/async/issues/74
            // Use a cast to hide from "pure expression does nothing" error
            // TODO avoid creating a ValDef for the result of this await to avoid this tree shape altogether.
            // This will require some deeper changes to the later parts of the macro which currently assume regular
            // tree structure around `await` calls.
            val refNoPureExpr =
            if (!isPastErasure && typeEqualsUnit(ref.tpe)) typedAt(tree.pos, gen.mkCast(ref, ref.tpe))
            else atPos(tree.pos)(ref)

            stats :+ valDef :+ refNoPureExpr

          case If(cond, thenp, elsep) =>
            // If we run the ANF transform post patmat, deal with trees like `(if (cond) jump1(){String} else jump2(){String}){String}`
            // as though it was typed with `Unit`.
            def isPatMatGeneratedJump(t: Tree): Boolean = t match {
              case Block(_, expr) => isPatMatGeneratedJump(expr)
              case If(_, thenp, elsep) => isPatMatGeneratedJump(thenp) && isPatMatGeneratedJump(elsep)
              case _: Apply if isLabel(t.symbol) => true
              case _ => false
            }

            if (isPatMatGeneratedJump(expr))
              assignUnitType(expr)

            // if type of if-else is Unit don't introduce assignment,
            // but add Unit value to bring it into form expected by async transform
            if (typeEqualsUnit(expr.tpe)) {
              statsExprUnit
            } else if (typeEqualsNothing(expr.tpe)) {
              statsExprThrow
            } else {
              val varDef = defineVar(name.ifRes(), expr.tpe, tree.pos)

              def branchWithAssign(t: Tree): Tree = {
                t match {
                  case MatchEnd(ld) =>
                    deriveLabelDef(ld, branchWithAssign)
                  case blk@Block(thenStats, thenExpr) =>
                    assignUnitType(treeCopy.Block(blk, thenStats, branchWithAssign(thenExpr)))
                  case _ =>
                    typedAssign(t, varDef.symbol)
                }
              }

              val ifWithAssign = assignUnitType(treeCopy.If(tree, cond, branchWithAssign(thenp), branchWithAssign(elsep)))
              stats :+ varDef :+ ifWithAssign :+ atPos(tree.pos)(gen.mkAttributedStableRef(varDef.symbol)).setType(tree.tpe)
            }
          case ld@LabelDef(name, params, rhs) =>
            if (isUnitType(ld.symbol.info.resultType)) statsExprUnit
            else stats :+ expr

          case Match(scrut, cases) =>
            // if type of match is Unit don't introduce assignment,
            // but add Unit value to bring it into form expected by async transform
            if (typeEqualsUnit(expr.tpe)) {
              statsExprUnit
            } else if (typeEqualsNothing(expr.tpe)) {
              statsExprThrow
            } else {
              val varDef = defineVar(name.matchRes(), expr.tpe, tree.pos)
              val casesWithAssign = cases map {
                case cd@CaseDef(pat, guard, body) =>
                  def bodyWithAssign(t: Tree): Tree = {
                    t match {
                      case MatchEnd(ld) => deriveLabelDef(ld, bodyWithAssign)
                      case b@Block(caseStats, caseExpr) => assignUnitType(treeCopy.Block(b, caseStats, bodyWithAssign(caseExpr)))
                      case _ => typedAssign(t, varDef.symbol)
                    }
                  }

                  assignUnitType(treeCopy.CaseDef(cd, pat, guard, bodyWithAssign(body)))
              }
              val matchWithAssign = assignUnitType(treeCopy.Match(tree, scrut, casesWithAssign))
              require(matchWithAssign.tpe != null, matchWithAssign)
              stats :+ varDef :+ matchWithAssign :+ atPos(tree.pos)(gen.mkAttributedStableRef(varDef.symbol)).setType(tree.tpe)
            }
          case _ =>
            stats :+ expr
        }
      }

      def defineVar(name: TermName, tp: Type, pos: Position): ValDef = {
        val sym = currentOwner.newTermSymbol(name, pos, Flags.MUTABLE | Flags.SYNTHETIC).setInfo(transformType(tp))
        ValDef(sym, mkZero(tp, pos)).setType(NoType).setPos(pos)
      }
    }

    def defineVal(name: TermName, lhs: Tree, pos: Position)(tp: Type = transformType(lhs.tpe)): ValDef = {
      val sym = currentOwner.newTermSymbol(name, pos, Flags.SYNTHETIC).setInfo(tp)

      val lhsOwned = lhs.changeOwner((currentOwner, sym))
      val rhs =
        if (isPastErasure && isUnitType(tp)) Block(lhsOwned :: Nil, literalUnit)
        else lhsOwned
      ValDef(sym, rhs).setType(NoType).setPos(pos)

    }

    object _anf {
      import treeInfo.Applied

      def transformToList(tree: Tree): List[Tree] = {
        mode = Anf;
        blockToList(transform(tree))
      }

      def _transformToList(tree: Tree): List[Tree] = trace(tree) {
        if (!containsAwait(tree)) {
          tree match {
            case Block(stats, expr) =>
              // avoids nested block in `while(await(false)) ...`.
              // TODO I think `containsAwait` really should return true if the code contains a label jump to an enclosing
              // while/doWhile and there is an await *anywhere* inside that construct.
              stats :+ expr
            case _ => List(tree)
          }
        } else tree match {
          case Select(qual, sel) =>
            val stats :+ expr = linearize.transformToList(qual)
            stats :+ treeCopy.Select(tree, expr, sel)

          case Throw(expr) =>
            val stats :+ expr1 = linearize.transformToList(expr)
            stats :+ treeCopy.Throw(tree, expr1)

          case Typed(expr, tpt) =>
            val stats :+ expr1 = linearize.transformToList(expr)
            stats :+ treeCopy.Typed(tree, expr1, tpt)

          case Applied(fun, targs, argss) if argss.nonEmpty =>
            // we can assume that no await call appears in a by-name argument position,
            // this has already been checked.
            val funStats :+ simpleFun = linearize.transformToList(fun)
            val (argStatss, argExprss): (List[List[List[Tree]]], List[List[Tree]]) =
              mapArgumentss[List[Tree]](fun, argss) {
                case Arg(expr, byName, _) if byName /*|| isPure(expr) TODO */ => (Nil, expr)
                case Arg(expr, _, argName) =>
                  linearize.transformToList(expr) match {
                    case stats :+ expr1 =>
                      val valDef = defineVal(name.freshen(argName), expr1, expr1.pos)()
                      require(valDef.tpe != null, valDef)
                      val stats1 = stats :+ valDef
                      (stats1, atPos(tree.pos.makeTransparent)(gen.stabilize(gen.mkAttributedIdent(valDef.symbol))))
                  }
              }

            def copyApplied(tree: Tree, depth: Int): Tree = {
              tree match {
                case TypeApply(_, targs) => treeCopy.TypeApply(tree, simpleFun, targs)
                case _ if depth == 0 => simpleFun
                case Apply(fun, args) =>
                  val newTypedArgs = map2(args.map(_.pos), argExprss(depth - 1))((pos, arg) => typedAt(pos, arg))
                  treeCopy.Apply(tree, copyApplied(fun, depth - 1), newTypedArgs)
              }
            }

            val typedNewApply = copyApplied(tree, argss.length)

            funStats ++ argStatss.flatten.flatten :+ typedNewApply

          case Block(stats, expr) =>
            val stats1 = stats.flatMap(linearize.transformToList).filterNot(isLiteralUnit)
            val exprs1 = linearize.transformToList(expr)
            val trees = stats1 ::: exprs1

            def groupsEndingWith[T](ts: List[T])(f: T => Boolean): List[List[T]] = if (ts.isEmpty) Nil else {
              ts.indexWhere(f) match {
                case -1 => List(ts)
                case i =>
                  val (ts1, ts2) = ts.splitAt(i + 1)
                  ts1 :: groupsEndingWith(ts2)(f)
              }
            }

            val matchGroups = groupsEndingWith(trees) { case MatchEnd(_) => true; case _ => false }
            val trees1 = matchGroups.flatMap(group => eliminateMatchEndLabelParameter(tree.pos, group))
            val result = trees1 flatMap {
              case Block(stats, expr) => stats :+ expr
              case t => t :: Nil
            }
            result

          case ValDef(mods, name, tpt, rhs) =>
            if (containsAwait(rhs)) {
              val stats :+ expr = atOwner(currentOwner.owner)(linearize.transformToList(rhs))
              stats.foreach(_.changeOwner((currentOwner, currentOwner.owner)))
              stats :+ treeCopy.ValDef(tree, mods, name, tpt, expr)
            } else List(tree)

          case Assign(lhs, rhs) =>
            val stats :+ expr = linearize.transformToList(rhs)
            stats :+ treeCopy.Assign(tree, lhs, expr)

          case If(cond, thenp, elsep) =>
            val condStats :+ condExpr = linearize.transformToList(cond)
            val thenBlock = linearize.transformToBlock(thenp)
            val elseBlock = linearize.transformToBlock(elsep)
            condStats :+ treeCopy.If(tree, condExpr, thenBlock, elseBlock)

          case Match(scrut, cases) =>
            val scrutStats :+ scrutExpr = linearize.transformToList(scrut)
            val caseDefs = cases map {
              case CaseDef(pat, guard, body) =>
                // extract local variables for all names bound in `pat`, and rewrite `body`
                // to refer to these.
                // TODO we can move this into ExprBuilder once we get rid of `AsyncDefinitionUseAnalyzer`.
                val block = linearize.transformToBlock(body)
                val (valDefs, mappings) = (pat collect {
                  case b@Bind(bindName, _) =>
                    val vd = defineVal(name.freshen(bindName.toTermName), gen.mkAttributedStableRef(b.symbol).setPos(b.pos), b.pos)()
                    vd.symbol.updateAttachment(SyntheticBindVal)
                    (vd, (b.symbol, vd.symbol))
                }).unzip
                val (from, to) = mappings.unzip
                val b@Block(stats1, expr1) = block.substituteSymbols(from, to).asInstanceOf[Block]
                val newBlock = treeCopy.Block(b, valDefs ++ stats1, expr1)
                treeCopy.CaseDef(tree, pat, guard, newBlock)
            }
            scrutStats :+ treeCopy.Match(tree, scrutExpr, caseDefs)

          case LabelDef(name, params, rhs) =>
            if (!isPastErasure && isUnitType(tree.symbol.info)) // erasure has already inserted unit
            List(treeCopy.LabelDef(tree, name, params, typed(Block(linearize.transformToList(rhs), literalUnit))).setSymbol(tree.symbol))
            else
            List(treeCopy.LabelDef(tree, name, params, typed(listToBlock(linearize.transformToList(rhs)))).setSymbol(tree.symbol))

          case TypeApply(fun, targs) =>
            val funStats :+ simpleFun = linearize.transformToList(fun)
            funStats :+ treeCopy.TypeApply(tree, simpleFun, targs)

          case _ =>
            List(tree)
        }
      }
    }

    // Replace the label parameters on `matchEnd` with use of a `matchRes` temporary variable
    //
    // CaseDefs are translated to labels without parameters. A terminal label, `matchEnd`, accepts
    // a parameter which is the result of the match (this is regular, so even Unit-typed matches have this).
    //
    // For our purposes, it is easier to:
    //   - extract a `matchRes` variable
    //   - rewrite the terminal label def to take no parameters, and instead read this temp variable
    //   - change jumps to the terminal label to an assignment and a no-arg label application
    def eliminateMatchEndLabelParameter(pos: Position, statsExpr: List[Tree]): List[Tree] = {
      val caseDefToMatchResult = collection.mutable.Map[Symbol, Symbol]()

      val matchResults = collection.mutable.Buffer[Tree]()

      def modifyLabelDef(ld: LabelDef): (Tree, Tree) = {
        val param = ld.params.head

        def unitLabelDef = {
          setUnitMethodInfo(ld.symbol)
          assignUnitType(treeCopy.LabelDef(ld, ld.name, Nil, typed(literalUnit)))
        }

        if (isUnitType(ld.params.head.tpe)) {
          // Unit typed match: eliminate the label def parameter, but don't create a matchres temp variable to
          // store the result for cleaner generated code.
          caseDefToMatchResult(ld.symbol) = NoSymbol
          (unitLabelDef, substituteTrees(ld.rhs, param.symbol :: Nil, typed(literalUnit) :: Nil))
        } else {
          // Otherwise, create the matchres var. We'll callers of the label def below.
          // Remember: we're iterating through the statement sequence in reverse, so we'll get
          // to the LabelDef and mutate `matchResults` before we'll get to its callers.
          val matchResult = linearize.defineVar(name.matchRes(), param.tpe, ld.pos)
          matchResults += matchResult
          caseDefToMatchResult(ld.symbol) = matchResult.symbol
          (unitLabelDef, ld.rhs.substituteSymbols(param.symbol :: Nil, matchResult.symbol :: Nil))
        }
      }

      val statsExpr0 = statsExpr.reverse.flatMap {
        case ld@LabelDef(_, param :: Nil, _) =>
          val (ld1, after) = modifyLabelDef(ld)
          List(after, ld1)
        case a@ValDef(mods, name, tpt, ld@LabelDef(_, param :: Nil, _)) =>
          val (ld1, after) = modifyLabelDef(ld)
          List(treeCopy.ValDef(a, mods, name, tpt, after), ld1)
        case t =>
          if (caseDefToMatchResult.isEmpty) t :: Nil
          else {
            val matchResultTransformer = new MatchResultTransformer(caseDefToMatchResult)
            val tree1 = matchResultTransformer.transformAtOwner(owner, t)
            tree1 :: Nil
          }
      }
      matchResults.toList match {
        case _ if caseDefToMatchResult.isEmpty =>
          statsExpr // return the original trees if nothing changed
        case Nil =>
          statsExpr0.reverse :+ literalUnit // must have been a unit-typed match, no matchRes variable to definne or refer to
        case r1 :: Nil =>
          // { var matchRes = _; ....; matchRes }
          (r1 +: statsExpr0.reverse) :+ atPos(pos)(gen.mkAttributedIdent(r1.symbol))
        case _ => error(pos, "Internal error: unexpected tree encountered during ANF transform " + statsExpr); statsExpr
      }
    }

    def anfLinearize(tree: Tree): Block = {
      val trees: List[Tree] = mode match {
        case Anf => _anf._transformToList(tree)
        case Linearizing => linearize._transformToList(tree)
      }
      listToBlock(trees)
    }

    override def transform(tree: Tree): Tree = {
      tree match {
        case _: ValDef | _: DefDef | _: Function | _: ClassDef | _: TypeDef =>
          atOwner(tree.symbol)(anfLinearize(tree))
        case _: ModuleDef =>
          atOwner(tree.symbol.asModule.moduleClass orElse tree.symbol)(anfLinearize(tree))
        case _ =>
          anfLinearize(tree)
      }
    }
  }

  final class MatchResultTransformer(caseDefToMatchResult: collection.Map[Symbol, Symbol]) extends TypingTransformer(currentTransformState.unit) {
    override def transform(tree: Tree): Tree = {
      def typedPos(pos: Position)(t: Tree): Tree = localTyper.typed(atPos(pos)(t))

      tree match {
        case Apply(fun, arg :: Nil) if isLabel(fun.symbol) && caseDefToMatchResult.contains(fun.symbol) =>
          val temp = caseDefToMatchResult(fun.symbol)
          if (temp == NoSymbol)
            typedPos(tree.pos)(Block(transform(arg) :: Nil, treeCopy.Apply(tree, fun, Nil)))
          else
          // setType needed for LateExpansion.shadowingRefinedType test case. There seems to be an inconsistency
          // in the trees after pattern matcher.
          // TODO miminize the problem in patmat and fix in scalac.
            typedPos(tree.pos)(Block(Assign(Ident(temp), transform(arg.setType(transformType(fun.tpe.paramLists.head.head.info)))) :: Nil, treeCopy.Apply(tree, fun, Nil)))
        case Block(stats, expr: Apply) if isLabel(expr.symbol) =>
          super.transform(tree) match {
            case Block(stats0, Block(stats1, expr1)) =>
              // flatten the block returned by `case Apply` above into the enclosing block for
              // cleaner generated code.
              treeCopy.Block(tree, stats0 ::: stats1, expr1)
            case t => t
          }
        case _ =>
          super.transform(tree)
      }
    }
  }
}

object SyntheticBindVal

// $Id$

package scala.tools.selectivecps

import scala.tools.nsc._
import scala.tools.nsc.transform._
import scala.tools.nsc.symtab._
import scala.tools.nsc.plugins._

import scala.tools.nsc.ast._

/**
 * In methods marked @cps, explicitly name results of calls to other @cps methods
 */
abstract class SelectiveANFTransform extends PluginComponent with Transform with
  TypingTransformers with CPSUtils {
  // inherits abstract value `global` and class `Phase` from Transform

  import global._                  // the global environment
  import definitions._             // standard classes and methods
  import typer.atOwner             // methods to type trees

  /** the following two members override abstract members in Transform */
  val phaseName: String = "selectiveanf"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new ANFTransformer(unit)


  class ANFTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    implicit val _unit = unit // allow code in CPSUtils.scala to report errors
    var cpsAllowed: Boolean = false // detect cps code in places we do not handle (yet)

    object RemoveTailReturnsTransformer extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Block(stms, r @ Return(expr)) =>
          treeCopy.Block(tree, stms, expr)

        case Block(stms, expr) =>
          treeCopy.Block(tree, stms, transform(expr))

        case If(cond, r1 @ Return(thenExpr), r2 @ Return(elseExpr)) =>
          treeCopy.If(tree, cond, transform(thenExpr), transform(elseExpr))

        case If(cond, r1 @ Return(thenExpr), elseExpr) =>
          treeCopy.If(tree, cond, transform(thenExpr), transform(elseExpr))

        case If(cond, thenExpr, r2 @ Return(elseExpr)) =>
          treeCopy.If(tree, cond, transform(thenExpr), transform(elseExpr))

        case If(cond, thenExpr, elseExpr) =>
          treeCopy.If(tree, cond, transform(thenExpr), transform(elseExpr))

        case Try(block, catches, finalizer) =>
          treeCopy.Try(tree,
                       transform(block),
                       (catches map (t => transform(t))).asInstanceOf[List[CaseDef]],
                       transform(finalizer))

        case CaseDef(pat, guard, r @ Return(expr)) =>
          treeCopy.CaseDef(tree, pat, guard, expr)

        case CaseDef(pat, guard, body) =>
          treeCopy.CaseDef(tree, pat, guard, transform(body))

        case Return(_) =>
          unit.error(tree.pos, "return expressions in CPS code must be in tail position")
          tree

        case _ =>
          super.transform(tree)
      }
    }

    def removeTailReturns(body: Tree): Tree = {
      // support body with single return expression
      body match {
        case Return(expr) => expr
        case _ => RemoveTailReturnsTransformer.transform(body)
      }
    }

    override def transform(tree: Tree): Tree = {
      if (!cpsEnabled) return tree

      tree match {

        // Maybe we should further generalize the transform and move it over
        // to the regular Transformer facility. But then, actual and required cps
        // state would need more complicated (stateful!) tracking.

        // Making the default case use transExpr(tree, None, None) instead of
        // calling super.transform() would be a start, but at the moment,
        // this would cause infinite recursion. But we could remove the
        // ValDef case here.

        case dd @ DefDef(mods, name, tparams, vparamss, tpt, rhs0) =>
          debuglog("transforming " + dd.symbol)

          atOwner(dd.symbol) {
            val rhs =
              if (cpsParamTypes(tpt.tpe).nonEmpty) removeTailReturns(rhs0)
              else rhs0
            val rhs1 = transExpr(rhs, None, getExternalAnswerTypeAnn(tpt.tpe))(getExternalAnswerTypeAnn(tpt.tpe).isDefined)

            debuglog("result "+rhs1)
            debuglog("result is of type "+rhs1.tpe)

            treeCopy.DefDef(dd, mods, name, transformTypeDefs(tparams), transformValDefss(vparamss),
                        transform(tpt), rhs1)
          }

        case ff @ Function(vparams, body) =>
          debuglog("transforming anon function " + ff.symbol)

          atOwner(ff.symbol) {

            //val body1 = transExpr(body, None, getExternalAnswerTypeAnn(body.tpe))

            // need to special case partial functions: if expected type is @cps
            // but all cases are pure, then we would transform
            // { x => x match { case A => ... }} to
            // { x => shiftUnit(x match { case A => ... })}
            // which Uncurry cannot handle (see function6.scala)
            // thus, we push down the shiftUnit to each of the case bodies

            val ext = getExternalAnswerTypeAnn(body.tpe)
            val pureBody = getAnswerTypeAnn(body.tpe).isEmpty
            implicit val isParentImpure = ext.isDefined

            def transformPureMatch(tree: Tree, selector: Tree, cases: List[CaseDef]) = {
              val caseVals = cases map { case cd @ CaseDef(pat, guard, body) =>
                // if (!hasPlusMarker(body.tpe)) body.tpe = body.tpe withAnnotation newPlusMarker() // TODO: to avoid warning
                val bodyVal = transExpr(body, None, ext) // ??? triggers "cps-transformed unexpectedly" warning in transTailValue
                treeCopy.CaseDef(cd, transform(pat), transform(guard), bodyVal)
              }
              treeCopy.Match(tree, transform(selector), caseVals)
            }

            def transformPureVirtMatch(body: Block, selDef: ValDef, cases: List[Tree], matchEnd: Tree) = {
              val stats = transform(selDef) :: (cases map (transExpr(_, None, ext)))
              treeCopy.Block(body, stats, transExpr(matchEnd, None, ext))
            }

            val body1 = body match {
              case Match(selector, cases) if ext.isDefined && pureBody =>
                transformPureMatch(body, selector, cases)

              // virtpatmat switch
              case Block(List(selDef: ValDef), mat@Match(selector, cases)) if ext.isDefined && pureBody =>
                treeCopy.Block(body, List(transform(selDef)), transformPureMatch(mat, selector, cases))

              // virtpatmat
              case b@Block(matchStats@((selDef: ValDef) :: cases), matchEnd) if ext.isDefined && pureBody && (matchStats forall treeInfo.hasSynthCaseSymbol) =>
                transformPureVirtMatch(b, selDef, cases, matchEnd)

              // virtpatmat that stores the scrut separately -- TODO: can we eliminate this case??
              case Block(List(selDef0: ValDef), mat@Block(matchStats@((selDef: ValDef) :: cases), matchEnd)) if ext.isDefined && pureBody  && (matchStats forall treeInfo.hasSynthCaseSymbol)=>
                treeCopy.Block(body, List(transform(selDef0)), transformPureVirtMatch(mat, selDef, cases, matchEnd))

              case _ =>
                transExpr(body, None, ext)
            }

            debuglog("anf result "+body1+"\nresult is of type "+body1.tpe)

            treeCopy.Function(ff, transformValDefs(vparams), body1)
          }

        case vd @ ValDef(mods, name, tpt, rhs) => // object-level valdefs
          debuglog("transforming valdef " + vd.symbol)

          if (getExternalAnswerTypeAnn(tpt.tpe).isEmpty) {
            
            atOwner(vd.symbol) {

              val rhs1 = transExpr(rhs, None, None)

              treeCopy.ValDef(vd, mods, name, transform(tpt), rhs1)
            }
          } else {
            unit.error(tree.pos, "cps annotations not allowed on by-value parameters or value definitions")
            super.transform(tree)
          }

        case TypeTree() =>
          // circumvent cpsAllowed here
          super.transform(tree)

        case Apply(_,_) =>
          // this allows reset { ... } in object constructors
          // it's kind of a hack to put it here (see note above)
          transExpr(tree, None, None)

        case _ =>
          if (hasAnswerTypeAnn(tree.tpe)) {
            if (!cpsAllowed) {
              if (tree.symbol.isLazy)
                unit.error(tree.pos, "implementation restriction: cps annotations not allowed on lazy value definitions")
              else
                unit.error(tree.pos, "cps code not allowed here / " + tree.getClass + " / " + tree)
            }
            log(tree)
          }

          cpsAllowed = false
          super.transform(tree)
      }
    }


    def transExpr(tree: Tree, cpsA: CPSInfo, cpsR: CPSInfo)(implicit isAnyParentImpure: Boolean = false): Tree = {
      transTailValue(tree, cpsA, cpsR)(cpsR.isDefined || isAnyParentImpure) match {
        case (Nil, b) => b
        case (a, b) =>
          treeCopy.Block(tree, a,b)
      }
    }


    def transArgList(fun: Tree, args: List[Tree], cpsA: CPSInfo)(implicit isAnyParentImpure: Boolean): (List[List[Tree]], List[Tree], CPSInfo) = {
      val formals = fun.tpe.paramTypes
      val overshoot = args.length - formals.length

      var spc: CPSInfo = cpsA

      val (stm,expr) = (for ((a,tp) <- args.zip(formals ::: List.fill(overshoot)(NoType))) yield {
        tp match {
          case TypeRef(_, ByNameParamClass, List(elemtp)) =>
            // note that we're not passing just isAnyParentImpure
            (Nil, transExpr(a, None, getAnswerTypeAnn(elemtp))(getAnswerTypeAnn(elemtp).isDefined || isAnyParentImpure))
          case _ =>
            val (valStm, valExpr, valSpc) = transInlineValue(a, spc)
            spc = valSpc
            (valStm, valExpr)
        }
      }).unzip

      (stm,expr,spc)
    }


    // precondition: cpsR.isDefined "implies" isAnyParentImpure
    def transValue(tree: Tree, cpsA: CPSInfo, cpsR: CPSInfo)(implicit isAnyParentImpure: Boolean): (List[Tree], Tree, CPSInfo) = {
      // return value: (stms, expr, spc), where spc is CPSInfo after stms but *before* expr
      implicit val pos = tree.pos
      tree match {
        case Block(stms, expr) =>
          val (cpsA2, cpsR2) = (cpsA, linearize(cpsA, getAnswerTypeAnn(tree.tpe))) // tbd
          //          val (cpsA2, cpsR2) = (None, getAnswerTypeAnn(tree.tpe))

          val (a, b) = transBlock(stms, expr, cpsA2, cpsR2)(cpsR2.isDefined || isAnyParentImpure)
          val tree1  = (treeCopy.Block(tree, a, b)) // no updateSynthFlag here!!!

          (Nil, tree1, cpsA)

        case If(cond, thenp, elsep) =>
          /* possible situations:
          cps before (cpsA)
          cps in condition (spc)  <-- synth flag set if *only* here!
          cps in (one or both) branches */
          val (condStats, condVal, spc) = transInlineValue(cond, cpsA)
          val (cpsA2, cpsR2) = if (hasSynthMarker(tree.tpe))
            (spc, linearize(spc, getAnswerTypeAnn(tree.tpe))) else
            (None, getAnswerTypeAnn(tree.tpe)) // if no cps in condition, branches must conform to tree.tpe directly
          val thenVal = transExpr(thenp, cpsA2, cpsR2)(cpsR2.isDefined || isAnyParentImpure)
          val elseVal = transExpr(elsep, cpsA2, cpsR2)(cpsR2.isDefined || isAnyParentImpure)

          // check that then and else parts agree (not necessary any more, but left as sanity check)
          if (cpsR.isDefined) {
            if (elsep == EmptyTree)
              unit.error(tree.pos, "always need else part in cps code")
          }
          if (hasAnswerTypeAnn(thenVal.tpe) != hasAnswerTypeAnn(elseVal.tpe)) {
            unit.error(tree.pos, "then and else parts must both be cps code or neither of them")
          }

          (condStats, updateSynthFlag(treeCopy.If(tree, condVal, thenVal, elseVal)), spc)

        case Match(selector, cases) =>
          val (selStats, selVal, spc) = transInlineValue(selector, cpsA)
          val (cpsA2, cpsR2) =
            if (hasSynthMarker(tree.tpe)) (spc, linearize(spc, getAnswerTypeAnn(tree.tpe)))
            else (None, getAnswerTypeAnn(tree.tpe))

          val caseVals = cases map { case cd @ CaseDef(pat, guard, body) =>
            val bodyVal = transExpr(body, cpsA2, cpsR2)(cpsR2.isDefined || isAnyParentImpure)
            treeCopy.CaseDef(cd, transform(pat), transform(guard), bodyVal)
          }

          (selStats, updateSynthFlag(treeCopy.Match(tree, selVal, caseVals)), spc)

        // this is utterly broken: LabelDefs need to be considered together when transforming them to DefDefs:
        // suppose a Block {L1; ... ; LN}
        // this should become {D1def ; ... ; DNdef ; D1()}
        // where D$idef = def L$i(..) = {L$i.body; L${i+1}(..)}

        case ldef @ LabelDef(name, params, rhs) =>
          // println("trans LABELDEF "+(name, params, tree.tpe, hasAnswerTypeAnn(tree.tpe)))
          // TODO why does the labeldef's type have a cpsMinus annotation, whereas the rhs does not? (BYVALmode missing/too much somewhere?)
          if (hasAnswerTypeAnn(tree.tpe)) {
            // currentOwner.newMethod(name, tree.pos, Flags.SYNTHETIC) setInfo ldef.symbol.info
            val sym    = ldef.symbol resetFlag Flags.LABEL
            val rhs1   = rhs //new TreeSymSubstituter(List(ldef.symbol), List(sym)).transform(rhs)
            val rhsVal = transExpr(rhs1, None, getAnswerTypeAnn(tree.tpe))(getAnswerTypeAnn(tree.tpe).isDefined || isAnyParentImpure) changeOwner (currentOwner -> sym)

            val stm1 = localTyper.typed(DefDef(sym, rhsVal))
            // since virtpatmat does not rely on fall-through, don't call the labels it emits
            // transBlock will take care of calling the first label
            // calling each labeldef is wrong, since some labels may be jumped over
            // we can get away with this for now since the only other labels we emit are for tailcalls/while loops,
            // which do not have consecutive labeldefs (and thus fall-through is irrelevant)
            if (treeInfo.hasSynthCaseSymbol(ldef)) (List(stm1), localTyper.typed{Literal(Constant(()))}, cpsA)
            else {
              assert(params.isEmpty, "problem in ANF transforming label with non-empty params "+ ldef)
              (List(stm1), localTyper.typed{Apply(Ident(sym), List())}, cpsA)
            }
          } else {
            val rhsVal = transExpr(rhs, None, None)
            (Nil, updateSynthFlag(treeCopy.LabelDef(tree, name, params, rhsVal)), cpsA)
          }


        case Try(block, catches, finalizer) =>
          val blockVal = transExpr(block, cpsA, cpsR)

          val catchVals = for {
            cd @ CaseDef(pat, guard, body) <- catches
            bodyVal = transExpr(body, cpsA, cpsR)
          } yield {
            treeCopy.CaseDef(cd, transform(pat), transform(guard), bodyVal)
          }

          val finallyVal = transExpr(finalizer, None, None) // for now, no cps in finally

          (Nil, updateSynthFlag(treeCopy.Try(tree, blockVal, catchVals, finallyVal)), cpsA)

        case Assign(lhs, rhs) =>
          // allow cps code in rhs only
          val (stms, expr, spc) = transInlineValue(rhs, cpsA)
          (stms, updateSynthFlag(treeCopy.Assign(tree, transform(lhs), expr)), spc)

        case Return(expr0) =>
          if (isAnyParentImpure)
            unit.error(tree.pos, "return expression not allowed, since method calls CPS method")
          val (stms, expr, spc) = transInlineValue(expr0, cpsA)
          (stms, updateSynthFlag(treeCopy.Return(tree, expr)), spc)

        case Throw(expr0) =>
          val (stms, expr, spc) = transInlineValue(expr0, cpsA)
          (stms, updateSynthFlag(treeCopy.Throw(tree, expr)), spc)

        case Typed(expr0, tpt) =>
          // TODO: should x: A @cps[B,C] have a special meaning?
          // type casts used in different ways (see match2.scala, #3199)
          val (stms, expr, spc) = transInlineValue(expr0, cpsA)
          val tpt1 = if (treeInfo.isWildcardStarArg(tree)) tpt else
            treeCopy.TypeTree(tpt).setType(removeAllCPSAnnotations(tpt.tpe))
//        (stms, updateSynthFlag(treeCopy.Typed(tree, expr, tpt1)), spc)
          (stms, treeCopy.Typed(tree, expr, tpt1).setType(removeAllCPSAnnotations(tree.tpe)), spc)

        case TypeApply(fun, args) =>
          val (stms, expr, spc) = transInlineValue(fun, cpsA)
          (stms, updateSynthFlag(treeCopy.TypeApply(tree, expr, args)), spc)

        case Select(qual, name) =>
          val (stms, expr, spc) = transInlineValue(qual, cpsA)
          (stms, updateSynthFlag(treeCopy.Select(tree, expr, name)), spc)

        case Apply(fun, args) =>
          val (funStm, funExpr, funSpc) = transInlineValue(fun, cpsA)
          val (argStm, argExpr, argSpc) = transArgList(fun, args, funSpc)

          (funStm ::: (argStm.flatten), updateSynthFlag(treeCopy.Apply(tree, funExpr, argExpr)),
            argSpc)

        case _ =>
          cpsAllowed = true
          (Nil, transform(tree), cpsA)
      }
    }

    // precondition: cpsR.isDefined "implies" isAnyParentImpure
    def transTailValue(tree: Tree, cpsA: CPSInfo, cpsR: CPSInfo)(implicit isAnyParentImpure: Boolean): (List[Tree], Tree) = {

      val (stms, expr, spc) = transValue(tree, cpsA, cpsR)

      val bot = linearize(spc, getAnswerTypeAnn(expr.tpe))(unit, tree.pos)

      val plainTpe = removeAllCPSAnnotations(expr.tpe)

      if (cpsR.isDefined && !bot.isDefined) {

        if (!expr.isEmpty && (expr.tpe.typeSymbol ne NothingClass)) {
          // must convert!
          debuglog("cps type conversion (has: " + cpsA + "/" + spc + "/" + expr.tpe  + ")")
          debuglog("cps type conversion (expected: " + cpsR.get + "): " + expr)

          if (!hasPlusMarker(expr.tpe))
            unit.warning(tree.pos, "expression " + tree + " is cps-transformed unexpectedly")

          try {
            val Some((a, b)) = cpsR
            /** Since shiftUnit is bounded [A,B,C>:B] this may not typecheck
             *  if C is overly specific.  So if !(B <:< C), call shiftUnit0
             *  instead, which takes only two type arguments.
             */
            val conforms = a <:< b
            val call = localTyper.typedPos(tree.pos)(
              Apply(
                TypeApply(
                  gen.mkAttributedRef( if (conforms) MethShiftUnit else MethShiftUnit0 ),
                  List(TypeTree(plainTpe), TypeTree(a)) ++ ( if (conforms) List(TypeTree(b)) else Nil )
                ),
                List(expr)
              )
            )
            // This is today's sick/meaningless heuristic for spotting breakdown so
            // we don't proceed until stack traces start draping themselves over everything.
            // If there are wildcard types in the tree and B == Nothing, something went wrong.
            // (I thought WildcardTypes would be enough, but nope.  'reset0 { 0 }' has them.)
            //
            // Code as simple as    reset((_: String).length)
            // will crash meaninglessly without this check.  See SI-3718.
            //
            // TODO - obviously this should be done earlier, differently, or with
            // a more skilled hand.  Most likely, all three.
            if ((b.typeSymbol eq NothingClass) && call.tpe.exists(_ eq WildcardType))
              unit.error(tree.pos, "cannot cps-transform malformed (possibly in shift/reset placement) expression")
            else
              return ((stms, call))
          }
          catch {
            case ex:TypeError =>
              unit.error(ex.pos, "cannot cps-transform expression " + tree + ": " + ex.msg)
          }
        }

      } else if (!cpsR.isDefined && bot.isDefined) {
        // error!
        debuglog("cps type error: " + expr)
        //println("cps type error: " + expr + "/" + expr.tpe + "/" + getAnswerTypeAnn(expr.tpe))

        //println(cpsR + "/" + spc + "/" + bot)

        unit.error(tree.pos, "found cps expression in non-cps position")
      } else {
        // all is well

        if (hasPlusMarker(expr.tpe)) {
          unit.warning(tree.pos, "expression " + expr + " of type " + expr.tpe + " is not expected to have a cps type")
          expr modifyType removeAllCPSAnnotations
        }

        // TODO: sanity check that types agree
      }

      (stms, expr)
    }

    def transInlineValue(tree: Tree, cpsA: CPSInfo)(implicit isAnyParentImpure: Boolean): (List[Tree], Tree, CPSInfo) = {

      val (stms, expr, spc) = transValue(tree, cpsA, None) // never required to be cps

      getAnswerTypeAnn(expr.tpe) match {
        case spcVal @ Some(_) =>

          val valueTpe = removeAllCPSAnnotations(expr.tpe)

          val sym: Symbol = (
            currentOwner.newValue(newTermName(unit.fresh.newName("tmp")), tree.pos, Flags.SYNTHETIC)
              setInfo valueTpe
              setAnnotations List(AnnotationInfo(MarkerCPSSym.tpe, Nil, Nil))
          )
          expr.changeOwner(currentOwner -> sym)

          (stms ::: List(ValDef(sym, expr) setType(NoType)),
             Ident(sym) setType(valueTpe) setPos(tree.pos), linearize(spc, spcVal)(unit, tree.pos))

        case _ =>
          (stms, expr, spc)
      }

    }



    def transInlineStm(stm: Tree, cpsA: CPSInfo)(implicit isAnyParentImpure: Boolean):  (List[Tree], CPSInfo) = {
      stm match {

        // TODO: what about DefDefs?
        // TODO: relation to top-level val def?
        // TODO: what about lazy vals?

        case tree @ ValDef(mods, name, tpt, rhs) =>
          val (stms, anfRhs, spc) = atOwner(tree.symbol) { transValue(rhs, cpsA, None) }

          val tv = new ChangeOwnerTraverser(tree.symbol, currentOwner)
          stms.foreach(tv.traverse(_))

          // TODO: symbol might already have annotation. Should check conformance
          // TODO: better yet: do without annotations on symbols

          val spcVal = getAnswerTypeAnn(anfRhs.tpe)
          if (spcVal.isDefined) {
              tree.symbol.setAnnotations(List(AnnotationInfo(MarkerCPSSym.tpe, Nil, Nil)))
          }

          (stms:::List(treeCopy.ValDef(tree, mods, name, tpt, anfRhs)), linearize(spc, spcVal)(unit, tree.pos))

        case _ =>
          val (headStms, headExpr, headSpc) = transInlineValue(stm, cpsA)
          val valSpc = getAnswerTypeAnn(headExpr.tpe)
          (headStms:::List(headExpr), linearize(headSpc, valSpc)(unit, stm.pos))
      }
    }

    // precondition: cpsR.isDefined "implies" isAnyParentImpure
    def transBlock(stms: List[Tree], expr: Tree, cpsA: CPSInfo, cpsR: CPSInfo)(implicit isAnyParentImpure: Boolean): (List[Tree], Tree) = {
      def rec(currStats: List[Tree], currAns: CPSInfo, accum: List[Tree]): (List[Tree], Tree) =
        currStats match {
          case Nil =>
            val (anfStats, anfExpr) = transTailValue(expr, currAns, cpsR)
            (accum ++ anfStats, anfExpr)

          case stat :: rest =>
            val (stats, nextAns) = transInlineStm(stat, currAns)
            rec(rest, nextAns, accum ++ stats)
         }

      val (anfStats, anfExpr) = rec(stms, cpsA, List())
      // println("\nanf-block:\n"+ ((stms :+ expr) mkString ("{", "\n", "}")) +"\nBECAME\n"+ ((anfStats :+ anfExpr) mkString ("{", "\n", "}")))
      // println("synth case? "+ (anfStats map (t => (t, t.isDef, treeInfo.hasSynthCaseSymbol(t)))))
      // SUPER UGLY HACK: handle virtpatmat-style matches, whose labels have already been turned into DefDefs
      if (anfStats.nonEmpty && (anfStats forall (t => !t.isDef || treeInfo.hasSynthCaseSymbol(t)))) {
        val (prologue, rest) = (anfStats :+ anfExpr) span (s => !s.isInstanceOf[DefDef]) // find first case
        // println("rest: "+ rest)
        // val (defs, calls) = rest partition (_.isInstanceOf[DefDef])
        if (rest.nonEmpty) {
          // the filter drops the ()'s emitted when transValue encountered a LabelDef
          val stats = prologue ++ (rest filter (_.isInstanceOf[DefDef])).reverse // ++ calls
          // println("REVERSED "+ (stats mkString ("{", "\n", "}")))
          (stats, localTyper.typed{Apply(Ident(rest.head.symbol), List())}) // call first label to kick-start the match
        } else (anfStats, anfExpr)
      } else (anfStats, anfExpr)
    }
  }
}

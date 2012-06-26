// $Id$

package scala.tools.selectivecps

import scala.tools.nsc.Global
import scala.tools.nsc.typechecker.Modes
import scala.tools.nsc.MissingRequirementError

abstract class CPSAnnotationChecker extends CPSUtils with Modes {
  val global: Global
  import global._
  import definitions._

  //override val verbose = true
  @inline override final def vprintln(x: =>Any): Unit = if (verbose) println(x)

  /**
   *  Checks whether @cps annotations conform
   */
  object checker extends AnnotationChecker {
    private def addPlusMarker(tp: Type)  = tp withAnnotation newPlusMarker()
    private def addMinusMarker(tp: Type) = tp withAnnotation newMinusMarker()

    private def cleanPlus(tp: Type) =
      removeAttribs(tp, MarkerCPSAdaptPlus, MarkerCPSTypes)
    private def cleanPlusWith(tp: Type)(newAnnots: AnnotationInfo*) =
      cleanPlus(tp) withAnnotations newAnnots.toList

    /** Check annotations to decide whether tpe1 <:< tpe2 */
    def annotationsConform(tpe1: Type, tpe2: Type): Boolean = {
      if (!cpsEnabled) return true

      vprintln("check annotations: " + tpe1 + " <:< " + tpe2)

      // Nothing is least element, but Any is not the greatest
      if (tpe1.typeSymbol eq NothingClass)
        return true

      val annots1 = cpsParamAnnotation(tpe1)
      val annots2 = cpsParamAnnotation(tpe2)

      // @plus and @minus should only occur at the left, and never together
      // TODO: insert check

      // @minus @cps is the same as no annotations
      if (hasMinusMarker(tpe1))
        return annots2.isEmpty

      // to handle answer type modification, we must make @plus <:< @cps
      if (hasPlusMarker(tpe1) && annots1.isEmpty)
        return true

      // @plus @cps will fall through and compare the @cps type args
      // @cps parameters must match exactly
      if ((annots1 corresponds annots2)(_.atp <:< _.atp))
        return true

      // Need to handle uninstantiated type vars specially:

      // g map (x => x)  with expected type List[Int] @cps
      // results in comparison ?That <:< List[Int] @cps

      // Instantiating ?That to an annotated type would fail during
      // transformation.

      // Instead we force-compare tpe1 <:< tpe2.withoutAnnotations
      // to trigger instantiation of the TypeVar to the base type

      // This is a bit unorthodox (we're only supposed to look at
      // annotations here) but seems to work.

      if (!annots2.isEmpty && !tpe1.isGround)
        return tpe1 <:< tpe2.withoutAnnotations

      false
    }

    /** Refine the computed least upper bound of a list of types.
     *  All this should do is add annotations. */
    override def annotationsLub(tpe: Type, ts: List[Type]): Type = {
      if (!cpsEnabled) return tpe

      val annots1 = cpsParamAnnotation(tpe)
      val annots2 = ts flatMap cpsParamAnnotation

      if (annots2.nonEmpty) {
        val cpsLub = newMarker(global.lub(annots1:::annots2 map (_.atp)))
        val tpe1 = if (annots1.nonEmpty) removeAttribs(tpe, MarkerCPSTypes) else tpe
        tpe1.withAnnotation(cpsLub)
      }
      else tpe
    }

    /** Refine the bounds on type parameters to the given type arguments. */
    override def adaptBoundsToAnnotations(bounds: List[TypeBounds], tparams: List[Symbol], targs: List[Type]): List[TypeBounds] = {
      if (!cpsEnabled) return bounds

      val anyAtCPS = newCpsParamsMarker(NothingClass.tpe, AnyClass.tpe)
      if (isFunctionType(tparams.head.owner.tpe) || isPartialFunctionType(tparams.head.owner.tpe)) {
        vprintln("function bound: " + tparams.head.owner.tpe + "/"+bounds+"/"+targs)
        if (hasCpsParamTypes(targs.last))
          bounds.reverse match {
            case res::b if !hasCpsParamTypes(res.hi) =>
              (TypeBounds(res.lo, res.hi.withAnnotation(anyAtCPS))::b).reverse
            case _ => bounds
          }
        else
          bounds
      }
      else if (tparams.head.owner == ByNameParamClass) {
        vprintln("byname bound: " + tparams.head.owner.tpe + "/"+bounds+"/"+targs)
        val TypeBounds(lo, hi) = bounds.head
        if (hasCpsParamTypes(targs.head) && !hasCpsParamTypes(hi))
          TypeBounds(lo, hi withAnnotation anyAtCPS) :: Nil
        else bounds
      } else
        bounds
    }

    override def canAdaptAnnotations(tree: Tree, mode: Int, pt: Type): Boolean = {
      if (!cpsEnabled) return false
      vprintln("can adapt annotations? " + tree + " / " + tree.tpe + " / " + Integer.toHexString(mode) + " / " + pt)

      val annots1 = cpsParamAnnotation(tree.tpe)
      val annots2 = cpsParamAnnotation(pt)

      if ((mode & global.analyzer.PATTERNmode) != 0) {
        //println("can adapt pattern annotations? " + tree + " / " + tree.tpe + " / " + Integer.toHexString(mode) + " / " + pt)
        if (!annots1.isEmpty) {
          return true
        }
      }

/*
      // not precise enough -- still relying on addAnnotations to remove things from ValDef symbols
      if ((mode & global.analyzer.TYPEmode) != 0 && (mode & global.analyzer.BYVALmode) != 0) {
        if (!annots1.isEmpty) {
          return true
        }
      }
*/

/*
      this interferes with overloading resolution
      if ((mode & global.analyzer.BYVALmode) != 0 && tree.tpe <:< pt) {
        vprintln("already compatible, can't adapt further")
        return false
      }
*/
      if ((mode & global.analyzer.EXPRmode) != 0) {
        if ((annots1 corresponds annots2)(_.atp <:< _.atp)) {
          vprintln("already same, can't adapt further")
          return false
        }

        if (annots1.isEmpty && !annots2.isEmpty && ((mode & global.analyzer.BYVALmode) == 0)) {
          //println("can adapt annotations? " + tree + " / " + tree.tpe + " / " + Integer.toHexString(mode) + " / " + pt)
          if (!hasPlusMarker(tree.tpe)) {
  //          val base = tree.tpe <:< removeAllCPSAnnotations(pt)
  //          val known = global.analyzer.isFullyDefined(pt)
  //          println(same + "/" + base + "/" + known)
            //val same = annots2 forall { case AnnotationInfo(atp: TypeRef, _, _) => atp.typeArgs(0) =:= atp.typeArgs(1) }
            // TBD: use same or not?
            //if (same) {
              vprintln("yes we can!! (unit)")
              return true
            //}
          }
        } else if (!annots1.isEmpty && ((mode & global.analyzer.BYVALmode) != 0)) {
          if (!hasMinusMarker(tree.tpe)) {
            vprintln("yes we can!! (byval)")
            return true
          }
        }
      }
      false
    }

    override def adaptAnnotations(tree: Tree, mode: Int, pt: Type): Tree = {
      if (!cpsEnabled) return tree

      vprintln("adapt annotations " + tree + " / " + tree.tpe + " / " + modeString(mode) + " / " + pt)

      val patMode   = (mode & global.analyzer.PATTERNmode) != 0
      val exprMode  = (mode & global.analyzer.EXPRmode) != 0
      val byValMode = (mode & global.analyzer.BYVALmode) != 0

      val annotsTree     = cpsParamAnnotation(tree.tpe)
      val annotsExpected = cpsParamAnnotation(pt)

      // not sure I rephrased this comment correctly:
      // replacing `patMode` in the condition below by `patMode || ((mode & global.analyzer.TYPEmode) != 0 && (mode & global.analyzer.BYVALmode))`
      // doesn't work correctly -- still relying on addAnnotations to remove things from ValDef symbols
      if (patMode && !annotsTree.isEmpty) tree modifyType removeAllCPSAnnotations
      else if (exprMode && !byValMode && !hasPlusMarker(tree.tpe) && annotsTree.isEmpty && annotsExpected.nonEmpty) { // shiftUnit
        // add a marker annotation that will make tree.tpe behave as pt, subtyping wise
        // tree will look like having any possible annotation
        //println("adapt annotations " + tree + " / " + tree.tpe + " / " + Integer.toHexString(mode) + " / " + pt)

        // CAVEAT:
        //  for monomorphic answer types we want to have @plus @cps (for better checking)
        //  for answer type modification we want to have only @plus (because actual answer type may differ from pt)

        val res = tree modifyType (_ withAnnotations newPlusMarker() :: annotsExpected) // needed for #1807
        vprintln("adapted annotations (not by val) of " + tree + " to " + res.tpe)
        res
      } else if (exprMode && byValMode && !hasMinusMarker(tree.tpe) && annotsTree.nonEmpty) { // dropping annotation
        // add a marker annotation that will make tree.tpe behave as pt, subtyping wise
        // tree will look like having no annotation
        val res = tree modifyType addMinusMarker
        vprintln("adapted annotations (by val) of " + tree + " to " + res.tpe)
        res
      } else tree
    }

    def updateAttributesFromChildren(tpe: Type, childAnnots: List[AnnotationInfo], byName: List[Tree]): Type = {
      tpe match {
        // Would need to push annots into each alternative of overloaded type
        // But we can't, since alternatives aren't types but symbols, which we
        // can't change (we'd be affecting symbols globally)
        /*
        case OverloadedType(pre, alts) =>
          OverloadedType(pre, alts.map((sym: Symbol) => updateAttributes(pre.memberType(sym), annots)))
        */
        case OverloadedType(pre, alts) => tpe   //reconstruct correct annotations later
        case MethodType(params, restpe) => tpe
        case PolyType(params, restpe) => tpe
        case _ =>
          assert(childAnnots forall (_ matches MarkerCPSTypes), childAnnots)
          /*
            [] + [] = []
            plus + [] = plus
            cps + [] = cps
            plus cps + [] = plus cps
            minus cps + [] = minus cps
            synth cps + [] = synth cps // <- synth on left - does it happen?

            [] + cps = cps
            plus + cps = synth cps
            cps + cps = cps! <- lin
            plus cps + cps = synth cps! <- unify
            minus cps + cps = minus cps! <- lin
            synth cps + cps = synth cps! <- unify
          */

          val plus = hasPlusMarker(tpe) || (
               hasCpsParamTypes(tpe)
            && byName.nonEmpty
            && (byName forall (t => hasPlusMarker(t.tpe)))
          )

          // move @plus annotations outward from by-name children
          if (childAnnots.isEmpty) return {
            if (plus) { // @plus or @plus @cps
              byName foreach (_ modifyType cleanPlus)
              addPlusMarker(tpe)
            }
            else tpe
          }

          val annots1 = cpsParamAnnotation(tpe)

          if (annots1.isEmpty) { // nothing or @plus
            cleanPlusWith(tpe)(newSynthMarker(), linearize(childAnnots))
          }
          else {
            val annot1 = single(annots1)
            if (plus) { // @plus @cps
              val annot2 = linearize(childAnnots)

              if (annot2.atp <:< annot1.atp) {
                try cleanPlusWith(tpe)(newSynthMarker(), annot2)
                finally byName foreach (_ modifyType cleanPlus)
              }
              else throw new TypeError(annot2 + " is not a subtype of " + annot1)
            }
            else if (hasSynthMarker(tpe)) { // @synth @cps
              val annot2 = linearize(childAnnots)
              if (annot2.atp <:< annot1.atp)
                cleanPlusWith(tpe)(annot2)
              else
                throw new TypeError(annot2 + " is not a subtype of " + annot1)
            }
            else // @cps
              cleanPlusWith(tpe)(linearize(childAnnots:::annots1))
          }
      }
    }

    def transArgList(fun: Tree, args: List[Tree]): List[List[Tree]] = {
      val formals = fun.tpe.paramTypes
      val overshoot = args.length - formals.length

      for ((a,tp) <- args.zip(formals ::: List.fill(overshoot)(NoType))) yield {
        tp match {
          case TypeRef(_, ByNameParamClass, List(elemtp)) =>
            Nil // TODO: check conformance??
          case _ =>
            List(a)
        }
      }
    }


    def transStms(stms: List[Tree]): List[Tree] = stms match {
      case ValDef(mods, name, tpt, rhs)::xs =>
        rhs::transStms(xs)
      case Assign(lhs, rhs)::xs =>
        rhs::transStms(xs)
      case x::xs =>
        x::transStms(xs)
      case Nil =>
        Nil
    }

    def single(xs: List[AnnotationInfo]) = xs match {
      case List(x) => x
      case _ =>
        global.globalError("not a single cps annotation: " + xs)
        xs(0)
    }
    
    def emptyOrSingleList(xs: List[AnnotationInfo]) = if (xs.isEmpty) Nil else List(single(xs))

    def transChildrenInOrder(tree: Tree, tpe: Type, childTrees: List[Tree], byName: List[Tree]) = {
      def inspect(t: Tree): List[AnnotationInfo] = {
        if (t.tpe eq null) Nil else {
          val extra: List[AnnotationInfo] = t.tpe match {
            case _: MethodType | _: PolyType | _: OverloadedType =>
              // method types, poly types and overloaded types do not obtain cps annotions by propagation
              // need to reconstruct transitively from their children.
              t match {
                case Select(qual, name) => inspect(qual)
                case Apply(fun, args) => (fun::(transArgList(fun,args).flatten)) flatMap inspect
                case TypeApply(fun, args) => (fun::(transArgList(fun,args).flatten)) flatMap inspect
                case _ => Nil
              }
            case _ => Nil
          }

          val types = cpsParamAnnotation(t.tpe)
          // TODO: check that it has been adapted and if so correctly
          extra ++ emptyOrSingleList(types)
        }
      }
      val children = childTrees flatMap inspect

      val newtpe = updateAttributesFromChildren(tpe, children, byName)

      if (!newtpe.annotations.isEmpty)
        vprintln("[checker] inferred " + tree + " / " + tpe + " ===> "+ newtpe)

      newtpe
    }

    /** Modify the type that has thus far been inferred
     *  for a tree.  All this should do is add annotations. */

    override def addAnnotations(tree: Tree, tpe: Type): Type = {
      import scala.util.control._
      if (!cpsEnabled) {
        if (Exception.failAsValue(classOf[MissingRequirementError])(false)(hasCpsParamTypes(tpe)))
          global.reporter.error(tree.pos, "this code must be compiled with the Scala continuations plugin enabled")
        return tpe
      }

//      if (tree.tpe.hasAnnotation(MarkerCPSAdaptPlus))
//        println("addAnnotation " + tree + "/" + tpe)

      tree match {

        case Apply(fun @ Select(qual, name), args) if fun.isTyped =>

          // HACK: With overloaded methods, fun will never get annotated. This is because
          // the 'overloaded' type gets annotated, but not the alternatives (among which
          // fun's type is chosen)

          vprintln("[checker] checking select apply " + tree + "/" + tpe)

          transChildrenInOrder(tree, tpe, qual::(transArgList(fun, args).flatten), Nil)

        case Apply(TypeApply(fun @ Select(qual, name), targs), args) if fun.isTyped => // not trigge

          vprintln("[checker] checking select apply type-apply " + tree + "/" + tpe)

          transChildrenInOrder(tree, tpe, qual::(transArgList(fun, args).flatten), Nil)

        case TypeApply(fun @ Select(qual, name), args) if fun.isTyped =>
          def stripNullaryMethodType(tp: Type) = tp match { case NullaryMethodType(restpe) => restpe case tp => tp }
          vprintln("[checker] checking select type-apply " + tree + "/" + tpe)

          transChildrenInOrder(tree, stripNullaryMethodType(tpe), List(qual, fun), Nil)

        case Apply(fun, args) if fun.isTyped =>

          vprintln("[checker] checking unknown apply " + tree + "/" + tpe)

          transChildrenInOrder(tree, tpe, fun::(transArgList(fun, args).flatten), Nil)

        case TypeApply(fun, args) =>

          vprintln("[checker] checking unknown type apply " + tree + "/" + tpe)

          transChildrenInOrder(tree, tpe, List(fun), Nil)

        case Select(qual, name) if qual.isTyped =>

          vprintln("[checker] checking select " + tree + "/" + tpe)

          // straightforward way is problematic (see select.scala and Test2.scala)
          // transChildrenInOrder(tree, tpe, List(qual), Nil)

          // the problem is that qual may be of type OverloadedType (or MethodType) and
          // we cannot safely annotate these. so we just ignore these cases and
          // clean up later in the Apply/TypeApply trees.

          if (hasCpsParamTypes(qual.tpe)) {
            // however there is one special case:
            // if it's a method without parameters, just apply it. normally done in adapt, but
            // we have to do it here so we don't lose the cps information (wouldn't trigger our
            // adapt and there is no Apply/TypeApply created)
            tpe match {
              case NullaryMethodType(restpe) =>
                //println("yep: " + restpe + "," + restpe.getClass)
                transChildrenInOrder(tree, restpe, List(qual), Nil)
              case _ : PolyType => tpe
              case _ : MethodType => tpe
              case _ : OverloadedType => tpe
              case _ =>
                transChildrenInOrder(tree, tpe, List(qual), Nil)
            }
          } else
            tpe

        case If(cond, thenp, elsep) =>
          transChildrenInOrder(tree, tpe, List(cond), List(thenp, elsep))

        case Match(select, cases) =>
          transChildrenInOrder(tree, tpe, List(select), cases:::(cases map { case CaseDef(_, _, body) => body }))

        case Try(block, catches, finalizer) =>
          val tpe1 = transChildrenInOrder(tree, tpe, Nil, block::catches:::(catches map { case CaseDef(_, _, body) => body }))

          val annots = cpsParamAnnotation(tpe1)
          if (annots.nonEmpty) {
            val ann = single(annots)
            val (atp0, atp1) = annTypes(ann)
            if (!(atp0 =:= atp1))
              throw new TypeError("only simple cps types allowed in try/catch blocks (found: " + tpe1 + ")")
            if (!finalizer.isEmpty) // no finalizers allowed. see explanation in SelectiveCPSTransform
              reporter.error(tree.pos, "try/catch blocks that use continuations cannot have finalizers")
          }
          tpe1

        case Block(stms, expr) =>
          // if any stm has annotation, so does block
          transChildrenInOrder(tree, tpe, transStms(stms), List(expr))

        case ValDef(mods, name, tpt, rhs) =>
          vprintln("[checker] checking valdef " + name + "/"+tpe+"/"+tpt+"/"+tree.symbol.tpe)
          // ValDef symbols must *not* have annotations!
          if (hasAnswerTypeAnn(tree.symbol.info)) { // is it okay to modify sym here?
            vprintln("removing annotation from sym " + tree.symbol + "/" + tree.symbol.tpe + "/" + tpt)
            tpt modifyType removeAllCPSAnnotations
            tree.symbol modifyInfo removeAllCPSAnnotations
          }
          tpe

        case _ =>
          tpe
      }


    }
  }
}

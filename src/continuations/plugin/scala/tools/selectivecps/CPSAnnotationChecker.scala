// $Id$

package scala.tools.selectivecps

import scala.tools.nsc.Global

import scala.collection.mutable.{Map, HashMap}

import java.io.{StringWriter, PrintWriter}

abstract class CPSAnnotationChecker extends CPSUtils {
  val global: Global
  import global._
  import definitions._

  //override val verbose = true

  /**
   *  Checks whether @cps annotations conform
   */
  object checker extends AnnotationChecker {

    /** Check annotations to decide whether tpe1 <:< tpe2 */
    def annotationsConform(tpe1: Type, tpe2: Type): Boolean = {
      if (!cpsEnabled) return true

      vprintln("check annotations: " + tpe1 + " <:< " + tpe2)

      // Nothing is least element, but Any is not the greatest
      if (tpe1.typeSymbol eq NothingClass)
        return true

      val annots1 = filterAttribs(tpe1,MarkerCPSTypes)
      val annots2 = filterAttribs(tpe2,MarkerCPSTypes)

      // @plus and @minus should only occur at the left, and never together
      // TODO: insert check
      val adaptPlusAnnots1 = filterAttribs(tpe1,MarkerCPSAdaptPlus)
      val adaptMinusAnnots1 = filterAttribs(tpe1,MarkerCPSAdaptMinus)

      // @minus @cps is the same as no annotations
      if (!adaptMinusAnnots1.isEmpty)
        return annots2.isEmpty

      // to handle answer type modification, we must make @plus <:< @cps
      if (!adaptPlusAnnots1.isEmpty && annots1.isEmpty)
        return true

      // @plus @cps will fall through and compare the @cps type args

      // @cps parameters must match exactly
      if ((annots1 corresponds annots2) { _.atp <:< _.atp })
        return true

/*
      hack no longer needed since introduction of adaptBoundsToAnnotations!

      // special treatment of type parameter bounds
      if ((tpe2.typeSymbol eq AnyClass)) {
        // This is an ugly hack to allow instantiating Functions with an @cps
        // return type. A better way would be to make sure everything goes through adapt,
        // but that's a bit of work. Alternatively, an explicit hook could be added in
        // Inferencer.checkBounds
        val w = new StringWriter()
        new Exception().printStackTrace(new PrintWriter(w, true))
        if (w.toString.contains("scala.tools.nsc.typechecker.Infer$Inferencer.checkBounds")) {
          vprintln("Testing whether " + tpe1 + " <:< " + tpe2 + ". We're inside Inferencer.checkBounds, so we just return true.")
          return true
        }
      }
*/
      false
    }


    /** Refine the computed least upper bound of a list of types.
     *  All this should do is add annotations. */
    override def annotationsLub(tpe: Type, ts: List[Type]): Type = {
      if (!cpsEnabled) return tpe

      val annots1 = filterAttribs(tpe, MarkerCPSTypes)
      val annots2 = ts flatMap (filterAttribs(_, MarkerCPSTypes))

      if (annots2.nonEmpty) {
        val cpsLub = AnnotationInfo(global.lub(annots1:::annots2 map (_.atp)), Nil, Nil)
        val tpe1 = if (annots1.nonEmpty) removeAttribs(tpe, MarkerCPSTypes) else tpe
        tpe1.withAnnotation(cpsLub)
      } else tpe
    }

    /** Refine the bounds on type parameters to the given type arguments. */
    override def adaptBoundsToAnnotations(bounds: List[TypeBounds], tparams: List[Symbol], targs: List[Type]): List[TypeBounds] = {
      if (!cpsEnabled) return bounds

      val anyAtCPS = AnnotationInfo(appliedType(MarkerCPSTypes.tpe, List(NothingClass.tpe, AnyClass.tpe)), Nil, Nil)
      if (isFunctionType(tparams.head.owner.tpe) || tparams.head.owner == PartialFunctionClass) {
        vprintln("function bound: " + tparams.head.owner.tpe + "/"+bounds+"/"+targs)
        if (targs.last.hasAnnotation(MarkerCPSTypes))
          bounds.reverse match {
            case res::b if !res.hi.hasAnnotation(MarkerCPSTypes) =>
              (TypeBounds(res.lo, res.hi.withAnnotation(anyAtCPS))::b).reverse
            case _ => bounds
          }
        else
          bounds
      } else if (tparams.head.owner == ByNameParamClass) {
        vprintln("byname bound: " + tparams.head.owner.tpe + "/"+bounds+"/"+targs)
        if (targs.head.hasAnnotation(MarkerCPSTypes) && !bounds.head.hi.hasAnnotation(MarkerCPSTypes))
          TypeBounds(bounds.head.lo, bounds.head.hi.withAnnotation(anyAtCPS))::Nil
        else bounds
      } else
        bounds
    }


    override def canAdaptAnnotations(tree: Tree, mode: Int, pt: Type): Boolean = {
      if (!cpsEnabled) return false
      vprintln("can adapt annotations? " + tree + " / " + tree.tpe + " / " + Integer.toHexString(mode) + " / " + pt)

      val annots1 = filterAttribs(tree.tpe,MarkerCPSTypes)
      val annots2 = filterAttribs(pt,MarkerCPSTypes)

      if ((mode & global.analyzer.PATTERNmode) != 0) {
        //println("can adapt pattern annotations? " + tree + " / " + tree.tpe + " / " + Integer.toHexString(mode) + " / " + pt)
        if (!annots1.isEmpty) {
          return true
        }
      }

/*
      //not precise enough
      if ((mode & global.analyzer.TYPEmode) != 0 && (mode & global.analyzer.BYVALmode) != 0) {
        //println("can adapt pattern annotations? " + tree + " / " + tree.tpe + " / " + Integer.toHexString(mode) + " / " + pt)
        if (!annots1.isEmpty) {
          return true
        }
      }
*/

      if ((mode & global.analyzer.EXPRmode) == 0) {
        vprintln("only handling EXPRmode")
        return false
      }
/*
      this interferes with overloading resolution
      if ((mode & global.analyzer.BYVALmode) != 0 && tree.tpe <:< pt) {
        vprintln("already compatible, can't adapt further")
        return false
      }
*/
      if ((mode & global.analyzer.EXPRmode) != 0) {
        if ((annots1 corresponds annots2) { case (a1,a2) => a1.atp <:< a2.atp }) {
          vprintln("already same, can't adapt further")
          return false
        }

        if (annots1.isEmpty && !annots2.isEmpty && ((mode & global.analyzer.BYVALmode) == 0)) {
          //println("can adapt annotations? " + tree + " / " + tree.tpe + " / " + Integer.toHexString(mode) + " / " + pt)
          val adapt = AnnotationInfo(MarkerCPSAdaptPlus.tpe, Nil, Nil)
          if (!tree.tpe.annotations.contains(adapt)) {
  //          val base = tree.tpe <:< removeAllCPSAnnotations(pt)
  //          val known = global.analyzer.isFullyDefined(pt)
  //          println(same + "/" + base + "/" + known)
            val same = true//annots2 forall { case AnnotationInfo(atp: TypeRef, _, _) => atp.typeArgs(0) =:= atp.typeArgs(1) }
            // TBD: use same or not?
            if (same) {
              vprintln("yes we can!! (unit)")
              return true
            }
          }
        } else if (!annots1.isEmpty && ((mode & global.analyzer.BYVALmode) != 0)) {
          val adapt = AnnotationInfo(MarkerCPSAdaptMinus.tpe, Nil, Nil)
          if (!tree.tpe.annotations.contains(adapt)) {
            vprintln("yes we can!! (byval)")
            return true
          }
        }
      }
      false
    }


    override def adaptAnnotations(tree: Tree, mode: Int, pt: Type): Tree = {
      if (!cpsEnabled) return tree

      // FIXME: there seems to be a problem when mode == 1 (expr, no poly) and
      // there are wildcards inside an annotation (which we don't resolve yet)
      // can we just instantiate things? <--- need to check this is still valid

      vprintln("adapt annotations " + tree + " / " + tree.tpe + " / " + Integer.toHexString(mode) + " / " + pt)

      val annots1 = filterAttribs(tree.tpe,MarkerCPSTypes)
      val annots2 = filterAttribs(pt,MarkerCPSTypes)

      if ((mode & global.analyzer.PATTERNmode) != 0) {
        if (!annots1.isEmpty) {
          return tree.setType(removeAllCPSAnnotations(tree.tpe))
        }
      }

/*
      // doesn't work correctly -- still relying on addAnnotations to remove things from ValDef symbols
      if ((mode & global.analyzer.TYPEmode) != 0 && (mode & global.analyzer.BYVALmode) != 0) {
        if (!annots1.isEmpty) {
          println("removing annotation from " + tree + "/" + tree.tpe)
          val s = tree.setType(removeAllCPSAnnotations(tree.tpe))
          println(s)
          s
        }
      }
*/

      if ((mode & global.analyzer.EXPRmode) != 0) {
        if (annots1.isEmpty && !annots2.isEmpty && ((mode & global.analyzer.BYVALmode) == 0)) { // shiftUnit
          // add a marker annotation that will make tree.tpe behave as pt, subtyping wise
          // tree will look like having any possible annotation
          //println("adapt annotations " + tree + " / " + tree.tpe + " / " + Integer.toHexString(mode) + " / " + pt)

          val adapt = AnnotationInfo(MarkerCPSAdaptPlus.tpe, Nil, Nil)
          val same = true//annots2 forall { case AnnotationInfo(atp: TypeRef, _, _) => atp.typeArgs(0) =:= atp.typeArgs(1) }
          // TBD: use same or not? see infer0.scala/infer1.scala

            // CAVEAT:
            //  for monomorphic answer types we want to have @plus @cps (for better checking)
            //  for answer type modification we want to have only @plus (because actual answer type may differ from pt)

  //        if (tree.tpe <:< removeAllCPSAnnotations(pt)) {

            //val known = global.analyzer.isFullyDefined(pt)

            if (same && !tree.tpe.annotations.contains(adapt)) {
              if (true /*known*/)
                return tree.setType(tree.tpe.withAnnotations(adapt::annots2)) // needed for #1807
              else
                return tree.setType(tree.tpe.withAnnotations(adapt::Nil))
            }
          tree
        } else if (!annots1.isEmpty && ((mode & global.analyzer.BYVALmode) != 0)) { // dropping annotation
          // add a marker annotation that will make tree.tpe behave as pt, subtyping wise
          // tree will look like having no annotation
          if (!tree.tpe.hasAnnotation(MarkerCPSAdaptMinus)) {
            val adapt = AnnotationInfo(MarkerCPSAdaptMinus.tpe, Nil, Nil)
            return tree.setType(tree.tpe.withAnnotations(adapt::Nil))
          }
        }
      }
      tree
    }


    def updateAttributesFromChildren(tpe: Type, childAnnots: List[AnnotationInfo], byName: List[Tree]): Type = {
      tpe match {
        // Need to push annots into each alternative of overloaded type

        // But we can't, since alternatives aren't types but symbols, which we
        // can't change (we'd be affecting symbols globally)
        /*
        case OverloadedType(pre, alts) =>
          OverloadedType(pre, alts.map((sym: Symbol) => updateAttributes(pre.memberType(sym), annots)))
        */
        case _ =>
          assert(childAnnots forall (_.atp.typeSymbol == MarkerCPSTypes), childAnnots)
          /*
            [] + [] = []
            plus + [] = plus
            cps + [] = cps
            plus cps + [] = plus cps
            minus cps + [] = minus cp
            synth cps + [] = synth cps // <- synth on left - does it happen?

            [] + cps = cps
            plus + cps = synth cps
            cps + cps = cps! <- lin
            plus cps + cps = synth cps! <- unify
            minus cps + cps = minus cps! <- lin
            synth cps + cps = synth cps! <- unify
          */

          val plus = tpe.hasAnnotation(MarkerCPSAdaptPlus) || (tpe.hasAnnotation(MarkerCPSTypes) &&
                        byName.nonEmpty && byName.forall(_.tpe.hasAnnotation(MarkerCPSAdaptPlus)))

          // move @plus annotations outward from by-name children
          if (childAnnots.isEmpty) {
            if (plus) { // @plus or @plus @cps
              for (t <- byName) {
                //println("removeAnnotation " + t + " / " + t.tpe)
                t.setType(removeAttribs(t.tpe, MarkerCPSAdaptPlus, MarkerCPSTypes))
              }
              return tpe.withAnnotation(AnnotationInfo(MarkerCPSAdaptPlus.tpe, Nil, Nil))
            } else
              return tpe
          }

          val annots1 = filterAttribs(tpe, MarkerCPSTypes)

          if (annots1.isEmpty) { // nothing or @plus
            val synth = MarkerCPSSynth.tpe
            val annots2 = List(linearize(childAnnots))
            removeAttribs(tpe,MarkerCPSAdaptPlus).withAnnotations(AnnotationInfo(synth, Nil, Nil)::annots2)
          } else {
            val annot1 = single(annots1)
            if (plus) { // @plus @cps
              val synth = AnnotationInfo(MarkerCPSSynth.tpe, Nil, Nil)
              val annot2 = linearize(childAnnots)
              if (!(annot2.atp <:< annot1.atp))
                throw new TypeError(annot2 + " is not a subtype of " + annot1)
              val res = removeAttribs(tpe, MarkerCPSAdaptPlus, MarkerCPSTypes).withAnnotations(List(synth, annot2))
              for (t <- byName) {
                //println("removeAnnotation " + t + " / " + t.tpe)
                t.setType(removeAttribs(t.tpe, MarkerCPSAdaptPlus, MarkerCPSTypes))
              }
              res
            } else if (tpe.hasAnnotation(MarkerCPSSynth)) { // @synth @cps
              val annot2 = linearize(childAnnots)
              if (!(annot2.atp <:< annot1.atp))
                throw new TypeError(annot2 + " is not a subtype of " + annot1)
              removeAttribs(tpe, MarkerCPSTypes).withAnnotation(annot2)
            } else { // @cps
              removeAttribs(tpe, MarkerCPSTypes).withAnnotation(linearize(childAnnots:::annots1))
            }
          }
      }
    }





    def transArgList(fun: Tree, args: List[Tree]): List[List[Tree]] = {
      val formals = fun.tpe.paramTypes
      val overshoot = args.length - formals.length

      for ((a,tp) <- args.zip(formals ::: List.fill(overshoot)(NoType))) yield {
        tp match {
          case TypeRef(_, sym, List(elemtp)) if sym == ByNameParamClass =>
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
        global.error("not a single cps annotation: " + xs)// FIXME: error message
        xs(0)
    }

    def transChildrenInOrder(tree: Tree, tpe: Type, childTrees: List[Tree], byName: List[Tree]) = {
      val children = childTrees.flatMap { t =>
        if (t.tpe eq null) Nil else {
          val types = filterAttribs(t.tpe, MarkerCPSTypes)
          // TODO: check that it has been adapted and if so correctly
          if (types.isEmpty) Nil else List(single(types))
        }
      }

      val newtpe = updateAttributesFromChildren(tpe, children, byName)

      if (!newtpe.annotations.isEmpty)
        vprintln("[checker] inferred " + tree + " / " + tpe + " ===> "+ newtpe)

      newtpe
    }

    /** Modify the type that has thus far been inferred
     *  for a tree.  All this should do is add annotations. */

    override def addAnnotations(tree: Tree, tpe: Type): Type = {
      if (!cpsEnabled) {
        if (tpe.annotations.nonEmpty && tpe.hasAnnotation(MarkerCPSTypes))
          global.reporter.error(tree.pos, "this code must be compiled with the Scala continuations plugin enabled")
        return tpe
      }

//      if (tree.tpe.hasAnnotation(MarkerCPSAdaptPlus))
//        println("addAnnotation " + tree + "/" + tpe)

      tree match {

        case Apply(fun @ Select(qual, name), args) if (fun.tpe ne null) && !fun.tpe.isErroneous =>

          // HACK: With overloaded methods, fun will never get annotated. This is because
          // the 'overloaded' type gets annotated, but not the alternatives (among which
          // fun's type is chosen)

          vprintln("[checker] checking select apply " + tree + "/" + tpe)

          transChildrenInOrder(tree, tpe, qual::(transArgList(fun, args).flatten), Nil)

        case TypeApply(fun @ Select(qual, name), args) if (fun.tpe ne null) && !fun.tpe.isErroneous =>
          vprintln("[checker] checking select apply " + tree + "/" + tpe)

          transChildrenInOrder(tree, tpe, List(qual, fun), Nil)

        case Apply(fun, args) if (fun.tpe ne null) && !fun.tpe.isErroneous =>

          vprintln("[checker] checking unknown apply " + tree + "/" + tpe)

          transChildrenInOrder(tree, tpe, fun::(transArgList(fun, args).flatten), Nil)

        case TypeApply(fun, args) =>

          vprintln("[checker] checking type apply " + tree + "/" + tpe)

          transChildrenInOrder(tree, tpe, List(fun), Nil)

        case Select(qual, name) =>

          vprintln("[checker] checking select " + tree + "/" + tpe)

          // FIXME: put it back in?? (problem with test cases select.scala and Test2.scala)
          // transChildrenInOrder(tree, tpe, List(qual), Nil)

          //TODO: cleanup

          // seem to be only a problem if qual is of type OverloadedType

          if (!tpe.isInstanceOf[OverloadedType] && !tpe.isInstanceOf[MethodType] && !tpe.isInstanceOf[PolyType]) {
            transChildrenInOrder(tree, tpe, List(qual), Nil)
          } else {
            if (qual.tpe.hasAnnotation(MarkerCPSTypes)) {
              // If it's a method without parameters, just apply it. normally done in adapt, but
              // we have to do it here so we don't lose the cps information
              tpe match {
                case PolyType(List(), restpe) =>
                  //println("yep: " + restpe + "," + restpe.getClass)
                  transChildrenInOrder(tree, restpe, List(qual), Nil)
                case _ =>
                  tpe
              }
            } else
              tpe
          }

        case If(cond, thenp, elsep) =>
          transChildrenInOrder(tree, tpe, List(cond), List(thenp, elsep))


        case Match(select, cases) =>
          // TODO: can there be cases that are not CaseDefs?? check collect vs map!
          transChildrenInOrder(tree, tpe, List(select), cases:::(cases collect { case CaseDef(_, _, body) => body }))

        case Try(block, catches, finalizer) =>
          val tpe1 = transChildrenInOrder(tree, tpe, Nil, block::catches:::(catches collect { case CaseDef(_, _, body) => body }))

          val annots = filterAttribs(tpe1, MarkerCPSTypes)
          if (annots.nonEmpty) {
            val ann = single(annots)
            val atp0::atp1::Nil = ann.atp.normalize.typeArgs
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
            tpt.setType(removeAllCPSAnnotations(tpt.tpe))
            tree.symbol.setInfo(removeAllCPSAnnotations(tree.symbol.info))
          }
          tpe

        case _ =>
          tpe
      }


    }
  }
}

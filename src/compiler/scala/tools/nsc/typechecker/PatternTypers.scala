/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala
package tools
package nsc
package typechecker

import scala.collection.mutable
import symtab.Flags
import Mode._

 /**
 *
 *  A pattern match such as
 *
 *    x match { case Foo(a, b) => ...}
 *
 *  Might match an instance of any of the following definitions of Foo.
 *  Note the analogous treatment between case classes and unapplies.
 *
 *    case class Foo(xs: Int*)
 *    case class Foo(a: Int, xs: Int*)
 *    case class Foo(a: Int, b: Int)
 *    case class Foo(a: Int, b: Int, xs: Int*)
 *
 *    object Foo { def unapplySeq(x: Any): Option[Seq[Int]] }
 *    object Foo { def unapplySeq(x: Any): Option[(Int, Seq[Int])] }
 *    object Foo { def unapply(x: Any): Option[(Int, Int)] }
 *    object Foo { def unapplySeq(x: Any): Option[(Int, Int, Seq[Int])] }
 */

trait PatternTypers {
  self: Analyzer =>

  import global._
  import definitions._

  // when true:
  //  - we may virtualize matches (if -Xexperimental and there's a suitable __match in scope)
  //  - we synthesize PartialFunction implementations for `x => x match {...}` and `match {...}` when the expected type is PartialFunction
  // this is disabled by: interactive compilation (we run it for scaladoc due to SI-5933)
  protected def newPatternMatching = true // presently overridden in the presentation compiler

  trait PatternTyper {
    self: Typer =>

    import TyperErrorGen._
    import infer._

    private def unit = context.unit

    /** Type trees in `args0` against corresponding expected type in `adapted0`.
     *
     * The mode in which each argument is typed is derived from `mode` and
     * whether the arg was originally by-name or var-arg (need `formals0` for that)
     * the default is by-val, of course.
     *
     * (docs reverse-engineered -- AM)
     */
    def typedArgs(args0: List[Tree], mode: Mode, formals0: List[Type], adapted0: List[Type]): List[Tree] = {
      def loop(args: List[Tree], formals: List[Type], adapted: List[Type]): List[Tree] = {
        if (args.isEmpty || adapted.isEmpty) Nil
        else {
          // No formals left or * indicates varargs.
          val isVarArgs = formals.isEmpty || formals.tail.isEmpty && isRepeatedParamType(formals.head)
          val isByName  = formals.nonEmpty && isByNameParamType(formals.head)
          def typedMode = if (isByName) mode.onlySticky else mode.onlySticky | BYVALmode
          def body = typedArg(args.head, mode, typedMode, adapted.head)
          def arg1 = if (isVarArgs) context.withinStarPatterns(body) else body

          // formals may be empty, so don't call tail
          arg1 :: loop(args.tail, formals drop 1, adapted.tail)
        }
      }
      loop(args0, formals0, adapted0)
    }

    protected def typedStarInPattern(tree: Tree, mode: Mode, pt: Type) = {
      val Typed(expr, tpt) = tree
      val exprTyped = typed(expr, mode.onlySticky)
      def subArrayType(pt: Type) =
        if (isPrimitiveValueClass(pt.typeSymbol) || !isFullyDefined(pt)) arrayType(pt)
        else {
          val tparam = context.owner freshExistential "" setInfo TypeBounds.upper(pt)
          newExistentialType(List(tparam), arrayType(tparam.tpe))
        }

      val (exprAdapted, baseClass) = exprTyped.tpe.typeSymbol match {
        case ArrayClass => (adapt(exprTyped, mode.onlySticky, subArrayType(pt)), ArrayClass)
        case _          => (adapt(exprTyped, mode.onlySticky, seqType(pt)), SeqClass)
      }
      exprAdapted.tpe.baseType(baseClass) match {
        case TypeRef(_, _, List(elemtp)) =>
          treeCopy.Typed(tree, exprAdapted, tpt setType elemtp) setType elemtp
        case _ =>
          setError(tree)
      }
    }

    protected def typedInPattern(tree: Typed, mode: Mode, pt: Type) = {
      val Typed(expr, tpt) = tree
      val tptTyped = typedType(tpt, mode)
      val exprTyped = typed(expr, mode.onlySticky, tptTyped.tpe.deconst)
      val treeTyped = treeCopy.Typed(tree, exprTyped, tptTyped)

      if (mode.inPatternMode) {
        val uncheckedTypeExtractor = extractorForUncheckedType(tpt.pos, tptTyped.tpe)
        // make fully defined to avoid bounded wildcard types that may be in pt from calling dropExistential (SI-2038)
        val ptDefined = ensureFullyDefined(pt) // FIXME this is probably redundant now that we don't dropExistenial in pattern mode.
        val ownType = inferTypedPattern(tptTyped, tptTyped.tpe, ptDefined, canRemedy = uncheckedTypeExtractor.nonEmpty)
        treeTyped setType ownType

        uncheckedTypeExtractor match {
          case None => treeTyped
          case Some(extractor) => wrapClassTagUnapply(treeTyped, extractor, tptTyped.tpe)
        }
      } else
        treeTyped setType tptTyped.tpe
    }

    /*
     * To deal with the type slack between actual (run-time) types and statically known types, for each abstract type T,
     * reflect its variance as a skolem that is upper-bounded by T (covariant position), or lower-bounded by T (contravariant).
     *
     * Consider the following example:
     *
     *  class AbsWrapperCov[+A]
     *  case class Wrapper[B](x: Wrapped[B]) extends AbsWrapperCov[B]
     *
     *  def unwrap[T](x: AbsWrapperCov[T]): Wrapped[T] = x match {
     *    case Wrapper(wrapped) => // Wrapper's type parameter must not be assumed to be equal to T, it's *upper-bounded* by it
     *      wrapped // : Wrapped[_ <: T]
     *  }
     *
     * this method should type check if and only if Wrapped is covariant in its type parameter
     *
     * when inferring Wrapper's type parameter B from x's type AbsWrapperCov[T],
     * we must take into account that x's actual type is AbsWrapperCov[Tactual] forSome {type Tactual <: T}
     * as AbsWrapperCov is covariant in A -- in other words, we must not assume we know T exactly, all we know is its upper bound
     *
     * since method application is the only way to generate this slack between run-time and compile-time types (TODO: right!?),
     * we can simply replace skolems that represent method type parameters as seen from the method's body
     * by other skolems that are (upper/lower)-bounded by that type-parameter skolem
     * (depending on the variance position of the skolem in the statically assumed type of the scrutinee, pt)
     *
     * see test/files/../t5189*.scala
     */
    def adaptConstrPattern(tree: Tree, pt: Type): Tree = { // (5)
      def hasUnapplyMember(tp: Type) = reallyExists(unapplyMember(tp))
      val overloadedExtractorOfObject = tree.symbol filter (sym => hasUnapplyMember(sym.tpe))
      // if the tree's symbol's type does not define an extractor, maybe the tree's type does.
      // this is the case when we encounter an arbitrary tree as the target of an unapply call
      // (rather than something that looks like a constructor call.) (for now, this only happens
      // due to wrapClassTagUnapply, but when we support parameterized extractors, it will become
      // more common place)
      val extractor = overloadedExtractorOfObject orElse unapplyMember(tree.tpe)
      def convertToCaseConstructor(clazz: Symbol): TypeTree = {
        // convert synthetic unapply of case class to case class constructor
        val prefix = tree.tpe.prefix
        val tree1 = TypeTree(clazz.primaryConstructor.tpe.asSeenFrom(prefix, clazz.owner))
          .setOriginal(tree)

        val skolems = new mutable.ListBuffer[TypeSymbol]
        object variantToSkolem extends TypeMap(trackVariance = true) {
          def apply(tp: Type) = mapOver(tp) match {
            // !!! FIXME - skipping this when variance.isInvariant allows unsoundness, see SI-5189
            case TypeRef(NoPrefix, tpSym, Nil) if !variance.isInvariant && tpSym.isTypeParameterOrSkolem && tpSym.owner.isTerm =>
              // must initialize or tpSym.tpe might see random type params!!
              // without this, we'll get very weird types inferred in test/scaladoc/run/SI-5933.scala
              // TODO: why is that??
              tpSym.initialize
              val bounds = if (variance.isPositive) TypeBounds.upper(tpSym.tpe) else TypeBounds.lower(tpSym.tpe)
              // origin must be the type param so we can deskolemize
              val skolem = context.owner.newGADTSkolem(unit.freshTypeName("?"+tpSym.name), tpSym, bounds)
              // println("mapping "+ tpSym +" to "+ skolem + " : "+ bounds +" -- pt= "+ pt +" in "+ context.owner +" at "+ context.tree )
              skolems += skolem
              skolem.tpe
            case tp1 => tp1
          }
        }

        // have to open up the existential and put the skolems in scope
        // can't simply package up pt in an ExistentialType, because that takes us back to square one (List[_ <: T] == List[T] due to covariance)
        val ptSafe   = variantToSkolem(pt) // TODO: pt.skolemizeExistential(context.owner, tree) ?
        val freeVars = skolems.toList

        // use "tree" for the context, not context.tree: don't make another CaseDef context,
        // as instantiateTypeVar's bounds would end up there
        val ctorContext = context.makeNewScope(tree, context.owner)
        freeVars foreach ctorContext.scope.enter
        newTyper(ctorContext).infer.inferConstructorInstance(tree1, clazz.typeParams, ptSafe)

        // simplify types without losing safety,
        // so that we get rid of unnecessary type slack, and so that error messages don't unnecessarily refer to skolems
        val extrapolate = new ExistentialExtrapolation(freeVars) extrapolate (_: Type)
        val extrapolated = tree1.tpe match {
          case MethodType(ctorArgs, res) => // ctorArgs are actually in a covariant position, since this is the type of the subpatterns of the pattern represented by this Apply node
            ctorArgs foreach (p => p.info = extrapolate(p.info)) // no need to clone, this is OUR method type
            copyMethodType(tree1.tpe, ctorArgs, extrapolate(res))
          case tp => tp
        }

        // once the containing CaseDef has been type checked (see typedCase),
        // tree1's remaining type-slack skolems will be deskolemized (to the method type parameter skolems)
        tree1 setType extrapolated
      }

      if (extractor != NoSymbol) {
        // if we did some ad-hoc overloading resolution, update the tree's symbol
        // do not update the symbol if the tree's symbol's type does not define an unapply member
        // (e.g. since it's some method that returns an object with an unapply member)
        if (overloadedExtractorOfObject != NoSymbol)
          tree setSymbol overloadedExtractorOfObject

        tree.tpe match {
          case OverloadedType(pre, alts) => tree setType overloadedType(pre, alts filter (alt => hasUnapplyMember(alt.tpe)))
          case _ =>
        }
        val unapply = unapplyMember(extractor.tpe)
        val clazz = unapplyParameterType(unapply)

        if (unapply.isCase && clazz.isCase) {
          convertToCaseConstructor(clazz)
        } else {
          tree
        }
      } else {
        val clazz = tree.tpe.typeSymbol.linkedClassOfClass
        if (clazz.isCase)
          convertToCaseConstructor(clazz)
        else
          CaseClassConstructorError(tree)
      }
    }

    def doTypedUnapply(tree: Tree, fun0: Tree, fun: Tree, args: List[Tree], mode: Mode, pt: Type): Tree = {
      def duplErrTree = setError(treeCopy.Apply(tree, fun0, args))
      def duplErrorTree(err: AbsTypeError) = { issue(err); duplErrTree }

      val otpe = fun.tpe

      if (args.length > MaxTupleArity)
        return duplErrorTree(TooManyArgsPatternError(fun))

      //
      def freshArgType(tp: Type): (List[Symbol], Type) = tp match {
        case MethodType(param :: _, _) =>
          (Nil, param.tpe)
        case PolyType(tparams, restpe) =>
          createFromClonedSymbols(tparams, freshArgType(restpe)._2)((ps, t) => ((ps, t)))
        // No longer used, see test case neg/t960.scala (#960 has nothing to do with it)
        case OverloadedType(_, _) =>
          OverloadedUnapplyError(fun)
          (Nil, ErrorType)
        case _ =>
          UnapplyWithSingleArgError(fun)
          (Nil, ErrorType)
      }

      val unapp     = unapplyMember(otpe)
      val unappType = otpe.memberType(unapp)
      val argDummy  = context.owner.newValue(nme.SELECTOR_DUMMY, fun.pos, Flags.SYNTHETIC) setInfo pt
      val arg       = Ident(argDummy) setType pt

      val uncheckedTypeExtractor =
        if (unappType.paramTypes.nonEmpty)
          extractorForUncheckedType(tree.pos, unappType.paramTypes.head)
        else None

      if (!isApplicableSafe(Nil, unappType, List(pt), WildcardType)) {
        //Console.println(s"UNAPP: need to typetest, arg: ${arg.tpe} unappType: $unappType")
        val (freeVars, unappFormal) = freshArgType(unappType.skolemizeExistential(context.owner, tree))
        val unapplyContext = context.makeNewScope(context.tree, context.owner)
        freeVars foreach unapplyContext.scope.enter

        val typer1 = newTyper(unapplyContext)
        val pattp = typer1.infer.inferTypedPattern(tree, unappFormal, arg.tpe, canRemedy = uncheckedTypeExtractor.nonEmpty)

        // turn any unresolved type variables in freevars into existential skolems
        val skolems = freeVars map (fv => unapplyContext.owner.newExistentialSkolem(fv, fv))
        arg setType pattp.substSym(freeVars, skolems)
        argDummy setInfo arg.tpe
      }

      // clearing the type is necessary so that ref will be stabilized; see bug 881
      val fun1 = typedPos(fun.pos)(Apply(Select(fun.clearType(), unapp), List(arg)))

      if (fun1.tpe.isErroneous) duplErrTree
      else {
        val resTp     = fun1.tpe.finalResultType.dealiasWiden
        val nbSubPats = args.length
        val (formals, formalsExpanded) =
          extractorFormalTypes(fun0.pos, resTp, nbSubPats, fun1.symbol, treeInfo.effectivePatternArity(args))
        if (formals == null) duplErrorTree(WrongNumberOfArgsError(tree, fun))
        else {
          val args1 = typedArgs(args, mode, formals, formalsExpanded)
          val pt1   = ensureFullyDefined(pt) // SI-1048
          val itype = glb(List(pt1, arg.tpe))
          arg setType pt1    // restore type (arg is a dummy tree, just needs to pass typechecking)
          val unapply = UnApply(fun1, args1) setPos tree.pos setType itype

          // if the type that the unapply method expects for its argument is uncheckable, wrap in classtag extractor
          // skip if the unapply's type is not a method type with (at least, but really it should be exactly) one argument
          // also skip if we already wrapped a classtag extractor (so we don't keep doing that forever)
          if (uncheckedTypeExtractor.isEmpty || fun1.symbol.owner.isNonBottomSubClass(ClassTagClass)) unapply
          else wrapClassTagUnapply(unapply, uncheckedTypeExtractor.get, unappType.paramTypes.head)
        }
      }
    }

    def wrapClassTagUnapply(uncheckedPattern: Tree, classTagExtractor: Tree, pt: Type): Tree = {
      // TODO: disable when in unchecked match
      // we don't create a new Context for a Match, so find the CaseDef,
      // then go out one level and navigate back to the match that has this case
      val args = List(uncheckedPattern)
      val app  = atPos(uncheckedPattern.pos)(Apply(classTagExtractor, args))
      // must call doTypedUnapply directly, as otherwise we get undesirable rewrites
      // and re-typechecks of the target of the unapply call in PATTERNmode,
      // this breaks down when the classTagExtractor (which defineds the unapply member) is not a simple reference to an object,
      // but an arbitrary tree as is the case here
      val res = doTypedUnapply(app, classTagExtractor, classTagExtractor, args, PATTERNmode, pt)

      log(sm"""
        |wrapClassTagUnapply {
        |  pattern: $uncheckedPattern
        |  extract: $classTagExtractor
        |       pt: $pt
        |      res: $res
        |}""".trim)

      res
    }

    // if there's a ClassTag that allows us to turn the unchecked type test for `pt` into a checked type test
    // return the corresponding extractor (an instance of ClassTag[`pt`])
    def extractorForUncheckedType(pos: Position, pt: Type): Option[Tree] = if (isPastTyper) None else {
      // only look at top-level type, can't (reliably) do anything about unchecked type args (in general)
      // but at least make a proper type before passing it elsewhere
      val pt1 = pt.dealiasWiden match {
        case tr @ TypeRef(pre, sym, args) if args.nonEmpty => copyTypeRef(tr, pre, sym, sym.typeParams map (_.tpeHK)) // replace actual type args with dummies
        case pt1                                           => pt1
      }
      pt1 match {
        // if at least one of the types in an intersection is checkable, use the checkable ones
        // this avoids problems as in run/matchonseq.scala, where the expected type is `Coll with scala.collection.SeqLike`
        // Coll is an abstract type, but SeqLike of course is not
        case RefinedType(ps, _) if ps.length > 1 && (ps exists infer.isCheckable) =>
          None

        case ptCheckable if infer isUncheckable ptCheckable =>
          val classTagExtractor = resolveClassTag(pos, ptCheckable)

          if (classTagExtractor != EmptyTree && unapplyMember(classTagExtractor.tpe) != NoSymbol)
            Some(classTagExtractor)
          else None

        case _ => None
      }
    }
  }
}
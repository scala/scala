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

  private object FixedAndRepeatedTypes {
    def unapply(types: List[Type]) = types match {
      case init :+ last if isRepeatedParamType(last) => Some((init, dropRepeated(last)))
      case _                                         => Some((types, NoType))
    }
  }

  trait PatternTyper {
    self: Typer =>

    import TyperErrorGen._
    import infer._

    private def unit = context.unit

    // If the tree's symbol's type does not define an extractor, maybe the tree's type does.
    // this is the case when we encounter an arbitrary tree as the target of an unapply call
    // (rather than something that looks like a constructor call.) (for now, this only happens
    // due to wrapClassTagUnapply, but when we support parameterized extractors, it will become
    // more common place)
    private def hasUnapplyMember(tpe: Type): Boolean   = reallyExists(unapplyMember(tpe))
    private def hasUnapplyMember(sym: Symbol): Boolean = hasUnapplyMember(sym.tpe_*)
    private def hasUnapplyMember(fun: Tree): Boolean   = hasUnapplyMember(fun.symbol) || hasUnapplyMember(fun.tpe)

    // ad-hoc overloading resolution to deal with unapplies and case class constructors
    // If some but not all alternatives survive filtering the tree's symbol with `p`,
    // then update the tree's symbol and type to exclude the filtered out alternatives.
    private def inPlaceAdHocOverloadingResolution(fun: Tree)(p: Symbol => Boolean): Tree = fun.symbol filter p match {
      case sym if sym.exists && (sym ne fun.symbol) => fun setSymbol sym modifyType (tp => filterOverloadedAlts(tp)(p))
      case _                                        => fun
    }
    private def filterOverloadedAlts(tpe: Type)(p: Symbol => Boolean): Type = tpe match {
      case OverloadedType(pre, alts) => overloadedType(pre, alts filter p)
      case tp                        => tp
    }

    def typedConstructorPattern(fun0: Tree, pt: Type) = {
      // Do some ad-hoc overloading resolution and update the tree's symbol and type
      // do not update the symbol if the tree's symbol's type does not define an unapply member
      // (e.g. since it's some method that returns an object with an unapply member)
      val fun       = inPlaceAdHocOverloadingResolution(fun0)(hasUnapplyMember)
      def caseClass = fun.tpe.typeSymbol.linkedClassOfClass

      // Dueling test cases: pos/overloaded-unapply.scala, run/case-class-23.scala, pos/t5022.scala
      // A case class with 23+ params has no unapply method.
      // A case class constructor be overloaded with unapply methods in the companion.
      if (caseClass.isCase && !unapplyMember(fun.tpe).isOverloaded)
        convertToCaseConstructor(fun, caseClass, pt)
      else if (hasUnapplyMember(fun))
        fun
      else
        CaseClassConstructorError(fun)
    }

    def expectedPatternTypes(fun: Tree, args: List[Tree]): List[Type] =
      newExtractorShape(fun, args).expectedPatternTypes

    def typedPatternArgs(fun: Tree, args: List[Tree], mode: Mode): List[Tree] =
      typedArgsForFormals(args, newExtractorShape(fun, args).formals, mode)

    def typedArgsForFormals(args: List[Tree], formals: List[Type], mode: Mode): List[Tree] = {
      def typedArgWithFormal(arg: Tree, pt: Type) = {
        val newMode = if (isByNameParamType(pt)) mode.onlySticky else mode.onlySticky | BYVALmode
        typedArg(arg, mode, newMode, dropByName(pt))
      }
      val FixedAndRepeatedTypes(fixed, elem) = formals
      val front = (args, fixed).zipped map typedArgWithFormal
      def rest  = context withinStarPatterns (args drop front.length map (typedArgWithFormal(_, elem)))

      elem match {
        case NoType => front
        case _      => front ::: rest
      }
    }

    private def boundedArrayType(bound: Type): Type = {
      val tparam = context.owner freshExistential "" setInfo (TypeBounds upper bound)
      newExistentialType(tparam :: Nil, arrayType(tparam.tpe_*))
    }

    protected def typedStarInPattern(tree: Tree, mode: Mode, pt: Type) = {
      val Typed(expr, tpt) = tree
      val exprTyped = typed(expr, mode)
      val baseClass = exprTyped.tpe.typeSymbol match {
        case ArrayClass => ArrayClass
        case _          => SeqClass
      }
      val starType = baseClass match {
        case ArrayClass if isPrimitiveValueType(pt) || !isFullyDefined(pt) => arrayType(pt)
        case ArrayClass                                                    => boundedArrayType(pt)
        case _                                                             => seqType(pt)
      }
      val exprAdapted = adapt(exprTyped, mode, starType)
      exprAdapted.tpe baseType baseClass match {
        case TypeRef(_, _, elemtp :: Nil) => treeCopy.Typed(tree, exprAdapted, tpt setType elemtp) setType elemtp
        case _                            => setError(tree)
      }
    }

    protected def typedInPattern(tree: Typed, mode: Mode, pt: Type) = {
      val Typed(expr, tpt) = tree
      val tptTyped  = typedType(tpt, mode)
      val tpe       = tptTyped.tpe
      val exprTyped = typed(expr, mode, tpe.deconst)
      val extractor = extractorForUncheckedType(tpt.pos, tpe)

      val canRemedy = tpe match {
        case RefinedType(_, decls) if !decls.isEmpty                 => false
        case RefinedType(parents, _) if parents exists isUncheckable => false
        case _                                                       => extractor.nonEmpty
      }

      val ownType   = inferTypedPattern(tptTyped, tpe, pt, canRemedy)
      val treeTyped = treeCopy.Typed(tree, exprTyped, tptTyped) setType ownType

      extractor match {
        case EmptyTree => treeTyped
        case _         => wrapClassTagUnapply(treeTyped, extractor, tpe)
      }
    }

    def newExtractorShape(tree: Tree): ExtractorShape = tree match {
      case Apply(fun, args)   => ExtractorShape(fun, args)
      case UnApply(fun, args) => ExtractorShape(fun, args)
    }
    def newExtractorShape(fun: Tree, args: List[Tree]): ExtractorShape = ExtractorShape(fun, args)

    case class CaseClassInfo(clazz: Symbol, classType: Type) {
      def constructor     = clazz.primaryConstructor
      def constructorType = classType.prefix memberType clazz memberType constructor
      def paramTypes      = constructorType.paramTypes
      def accessors       = clazz.caseFieldAccessors
      def accessorTypes   = accessors map (m => (classType memberType m).finalResultType)
      // def inverted        = MethodType(clazz :: Nil, tupleType(accessorTypes))
    }
    object NoCaseClassInfo extends CaseClassInfo(NoSymbol, NoType) {
      override def toString = "NoCaseClassInfo"
    }

    case class UnapplyMethodInfo(unapply: Symbol, tpe: Type) {
      def name         = unapply.name
      def isUnapplySeq = name == nme.unapplySeq
      def unapplyType  = tpe memberType method
      def resultType   = tpe.finalResultType
      def method       = unapplyMember(tpe)
      def paramType    = firstParamType(unapplyType)
      def rawGet       = if (isBool) UnitTpe else typeOfMemberNamedGetOrSelf(resultType)
      def rawTypes     = if (isBool) Nil else typesOfSelectorsOrSelf(rawGet)
      def rawArity     = rawTypes.size
      def isBool       = resultType =:= BooleanTpe   // aka "Tuple0" or "Option[Unit]"
      def isNothing    = rawGet =:= NothingTpe
      def isCase       = method.isCase
    }

    object NoUnapplyMethodInfo extends UnapplyMethodInfo(NoSymbol, NoType) {
      override def toString = "NoUnapplyMethodInfo"
    }

    case class ExtractorShape(fun: Tree, args: List[Tree]) {
      def pos            = fun.pos
      private def symbol = fun.symbol
      private def tpe    = fun.tpe

      val ccInfo = tpe.typeSymbol.linkedClassOfClass match {
        case clazz if clazz.isCase => CaseClassInfo(clazz, tpe)
        case _                     => NoCaseClassInfo
      }
      val exInfo = UnapplyMethodInfo(symbol, tpe)
      import exInfo.{ rawGet, rawTypes, isUnapplySeq }

      override def toString = s"ExtractorShape($fun, $args)"

      def unapplyMethod    = exInfo.method
      def unapplyType      = exInfo.unapplyType
      def unapplyParamType = exInfo.paramType
      def caseClass        = ccInfo.clazz
      def enclClass        = symbol.enclClass

      // TODO - merge these. The difference between these two methods is that expectedPatternTypes
      // expands the list of types so it is the same length as the number of patterns, whereas formals
      // leaves the varargs type unexpanded.
      def formals   = (
        if (isUnapplySeq) productTypes :+ varargsType
        else if (elementArity == 0) productTypes
        else if (isSingle) squishIntoOne()
        else wrongArity(patternFixedArity)
      )
      def expectedPatternTypes = elementArity match {
        case 0                                               => productTypes
        case _ if elementArity > 0 && isUnapplySeq           => productTypes ::: elementTypes
        case _ if productArity > 1 && patternFixedArity == 1 => squishIntoOne()
        case _                                               => wrongArity(patternFixedArity)
      }

      def elementType = elementTypeOfLastSelectorOrSelf(rawGet)

      private def hasBogusExtractor = directUnapplyMember(tpe).exists && !unapplyMethod.exists
      private def expectedArity = "" + productArity + ( if (isUnapplySeq) "+" else "")
      private def wrongArityMsg(n: Int) = (
        if (hasBogusExtractor) s"$enclClass does not define a valid extractor method"
        else s"wrong number of patterns for $enclClass offering $rawTypes_s: expected $expectedArity, found $n"
      )
      private def rawTypes_s = rawTypes match {
        case Nil       => "()"
        case tp :: Nil => "" + tp
        case tps       => tps.mkString("(", ", ", ")")
      }

      private def err(msg: String)    = { unit.error(pos, msg) ; throw new TypeError(msg) }
      private def wrongArity(n: Int)  = err(wrongArityMsg(n))

      def squishIntoOne()     = {
        if (settings.lint)
          unit.warning(pos, s"$enclClass expects $expectedArity patterns to hold $rawGet but crushing into $productArity-tuple to fit single pattern (SI-6675)")

        rawGet :: Nil
      }
      // elementArity is the number of non-sequence patterns minus the
      // the number of non-sequence product elements returned by the extractor.
      // If it is zero, there is a perfect match between those parts, and
      // if there is a wildcard star it will match any sequence.
      // If it is positive, there are more patterns than products,
      // so a sequence will have to fill in the elements. If it is negative,
      // there are more products than patterns, which is a compile time error.
      def elementArity      = patternFixedArity - productArity
      def patternFixedArity = treeInfo effectivePatternArity args
      def productArity      = productTypes.size
      def isSingle          = !isUnapplySeq && (patternFixedArity == 1)

      def productTypes = if (isUnapplySeq) rawTypes dropRight 1 else rawTypes
      def elementTypes = List.fill(elementArity)(elementType)
      def varargsType  = scalaRepeatedType(elementType)
    }

    private class VariantToSkolemMap extends TypeMap(trackVariance = true) {
      private val skolemBuffer = mutable.ListBuffer[TypeSymbol]()

      def skolems = try skolemBuffer.toList finally skolemBuffer.clear()
      def apply(tp: Type): Type = mapOver(tp) match {
        // !!! FIXME - skipping this when variance.isInvariant allows unsoundness, see SI-5189
        case tp @ TypeRef(NoPrefix, tpSym, Nil) if tpSym.isTypeParameterOrSkolem && tpSym.owner.isTerm =>
          if (variance.isInvariant) {
            // if (variance.isInvariant) tpSym.tpeHK.bounds
            devWarning(s"variantToSkolem skipping rewrite of $tpSym due to invariance")
            return tp
          }
          val bounds = (
            if (variance.isPositive) TypeBounds.upper(tpSym.tpeHK)
            else TypeBounds.lower(tpSym.tpeHK)
          )
          // origin must be the type param so we can deskolemize
          val skolem = context.owner.newGADTSkolem(unit.freshTypeName("?"+tpSym.name), tpSym, bounds)
          skolemBuffer += skolem
          skolem.tpe_*
        case tp1 => tp1
      }
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
    private def convertToCaseConstructor(tree: Tree, caseClass: Symbol, pt: Type): Tree = {
      val variantToSkolem     = new VariantToSkolemMap
      val caseConstructorType = tree.tpe.prefix memberType caseClass memberType caseClass.primaryConstructor
      val tree1               = TypeTree(caseConstructorType) setOriginal tree

      // have to open up the existential and put the skolems in scope
      // can't simply package up pt in an ExistentialType, because that takes us back to square one (List[_ <: T] == List[T] due to covariance)
      val ptSafe   = variantToSkolem(pt) // TODO: pt.skolemizeExistential(context.owner, tree) ?
      val freeVars = variantToSkolem.skolems

      // use "tree" for the context, not context.tree: don't make another CaseDef context,
      // as instantiateTypeVar's bounds would end up there
      log(s"convert ${tree.summaryString}: ${tree.tpe} to case constructor, pt=$ptSafe")

      val ctorContext = context.makeNewScope(tree, context.owner)
      freeVars foreach ctorContext.scope.enter
      newTyper(ctorContext).infer.inferConstructorInstance(tree1, caseClass.typeParams, ptSafe)

      // simplify types without losing safety,
      // so that we get rid of unnecessary type slack, and so that error messages don't unnecessarily refer to skolems
      val extrapolator = new ExistentialExtrapolation(freeVars)
      def extrapolate(tp: Type) = extrapolator extrapolate tp

      // once the containing CaseDef has been type checked (see typedCase),
      // tree1's remaining type-slack skolems will be deskolemized (to the method type parameter skolems)
      tree1 modifyType {
        case MethodType(ctorArgs, restpe) => // ctorArgs are actually in a covariant position, since this is the type of the subpatterns of the pattern represented by this Apply node
          copyMethodType(tree1.tpe, ctorArgs map (_ modifyInfo extrapolate), extrapolate(restpe)) // no need to clone ctorArgs, this is OUR method type
        case tp => tp
      }
    }

    def doTypedUnapply(tree: Tree, fun0: Tree, fun: Tree, args: List[Tree], mode: Mode, pt: Type): Tree = {
      def duplErrTree = setError(treeCopy.Apply(tree, fun0, args))
      def duplErrorTree(err: AbsTypeError) = { issue(err); duplErrTree }

      if (args.length > MaxTupleArity)
        return duplErrorTree(TooManyArgsPatternError(fun))

      def freshArgType(tp: Type): Type = tp match {
        case MethodType(param :: _, _) => param.tpe
        case PolyType(tparams, restpe) => createFromClonedSymbols(tparams, freshArgType(restpe))(polyType)
        case OverloadedType(_, _)      => OverloadedUnapplyError(fun) ; ErrorType
        case _                         => UnapplyWithSingleArgError(fun) ; ErrorType
      }
      val shape = newExtractorShape(fun, args)
      import shape.{ unapplyParamType, unapplyType, unapplyMethod }

      def extractor     = extractorForUncheckedType(shape.pos, unapplyParamType)
      def canRemedy     = unapplyParamType match {
        case RefinedType(_, decls) if !decls.isEmpty                 => false
        case RefinedType(parents, _) if parents exists isUncheckable => false
        case _                                                       => extractor.nonEmpty
      }

      def freshUnapplyArgType(): Type = {
        val GenPolyType(freeVars, unappFormal) = freshArgType(unapplyType.skolemizeExistential(context.owner, tree))
        val unapplyContext = context.makeNewScope(context.tree, context.owner)
        freeVars foreach unapplyContext.scope.enter
        val pattp = newTyper(unapplyContext).infer.inferTypedPattern(tree, unappFormal, pt, canRemedy)
        // turn any unresolved type variables in freevars into existential skolems
        val skolems = freeVars map (fv => unapplyContext.owner.newExistentialSkolem(fv, fv))
        pattp.substSym(freeVars, skolems)
      }

      val unapplyArg = (
        context.owner.newValue(nme.SELECTOR_DUMMY, fun.pos, Flags.SYNTHETIC) setInfo (
          if (isApplicableSafe(Nil, unapplyType, pt :: Nil, WildcardType)) pt
          else freshUnapplyArgType()
        )
      )
      // clearing the type is necessary so that ref will be stabilized; see bug 881
      val fun1 = typedPos(fun.pos)(Apply(Select(fun.clearType(), unapplyMethod), Ident(unapplyArg) :: Nil))

      def makeTypedUnApply() = {
        // the union of the expected type and the inferred type of the argument to unapply
        val glbType        = glb(ensureFullyDefined(pt) :: unapplyArg.tpe_* :: Nil)
        val wrapInTypeTest = canRemedy && !(fun1.symbol.owner isNonBottomSubClass ClassTagClass)
        val args1          = typedPatternArgs(fun1, args, mode)
        val result         = UnApply(fun1, args1) setPos tree.pos setType glbType

        if (wrapInTypeTest)
          wrapClassTagUnapply(result, extractor, glbType)
        else
          result
      }

      if (fun1.tpe.isErroneous)
        duplErrTree
      else if (unapplyMethod.isMacro && !fun1.isInstanceOf[Apply])
        duplErrorTree(WrongShapeExtractorExpansion(tree))
      else
        makeTypedUnApply()
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
    def extractorForUncheckedType(pos: Position, pt: Type): Tree = {
      if (isPastTyper || (pt eq NoType)) EmptyTree else {
        pt match {
          case RefinedType(parents, decls) if !decls.isEmpty || (parents exists isUncheckable) => return EmptyTree
          case _                                                                               =>
        }
        // only look at top-level type, can't (reliably) do anything about unchecked type args (in general)
        // but at least make a proper type before passing it elsewhere
        val pt1 = pt.dealiasWiden match {
          case tr @ TypeRef(pre, sym, args) if args.nonEmpty => copyTypeRef(tr, pre, sym, sym.typeParams map (_.tpeHK)) // replace actual type args with dummies
          case pt1                                           => pt1
        }
        if (isCheckable(pt1)) EmptyTree
        else resolveClassTag(pos, pt1) match {
          case tree if unapplyMember(tree.tpe).exists => tree
          case _                                      => devWarning(s"Cannot create runtime type test for $pt1") ; EmptyTree
        }
      }
    }
  }
}
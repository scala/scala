/* NSC -- new Scala compiler
 *
 * Copyright 2011-2013 LAMP/EPFL
 * @author Adriaan Moors
 */

package scala.tools.nsc.transform.patmat

import scala.tools.nsc.{ast, symtab, typechecker, transform, Global}
import transform._
import typechecker._
import symtab._
import Flags.{MUTABLE, METHOD, LABEL, SYNTHETIC, ARTIFACT}
import scala.language.postfixOps
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.transform.Transform
import scala.collection.mutable
import scala.reflect.internal.util.Statistics
import scala.reflect.internal.Types
import scala.reflect.internal.util.HashSet

trait TypeAnalysis extends Debugging {
  val global: Global
  import global.{Tree, Type, Symbol, definitions, analyzer,
    ConstantType, Literal, Constant,  appliedType, WildcardType, TypeRef, ModuleClassSymbol,
    nestedMemberType, TypeMap, Ident}

  import definitions._
  import analyzer.Typer

  import debugging.patmatDebug

  // we use subtyping as a model for implication between instanceof tests
  // i.e., when S <:< T we assume x.isInstanceOf[S] implies x.isInstanceOf[T]
  // unfortunately this is not true in general:
  // SI-6022 expects instanceOfTpImplies(ProductClass.tpe, AnyRefClass.tpe)
  def instanceOfTpImplies(tp: Type, tpImplied: Type) = {
    val tpValue    = tp.typeSymbol.isPrimitiveValueClass

    // pretend we're comparing to Any when we're actually comparing to AnyVal or AnyRef
    // (and the subtype is respectively a value type or not a value type)
    // this allows us to reuse subtyping as a model for implication between instanceOf tests
    // the latter don't see a difference between AnyRef, Object or Any when comparing non-value types -- SI-6022
    val tpImpliedNormalizedToAny =
      if (tpImplied =:= (if (tpValue) AnyValClass.tpe else AnyRefClass.tpe)) AnyClass.tpe
      else tpImplied

    tp <:< tpImpliedNormalizedToAny
  }

  // TODO: improve, e.g., for constants
  def sameValue(a: Tree, b: Tree): Boolean = (a eq b) || ((a, b) match {
    case (_ : Ident, _ : Ident) => a.symbol eq b.symbol
    case _                      => false
  })

  trait CheckableTypeAnalysis {
    val typer: Typer

    // TODO: domain of other feasibly enumerable built-in types (char?)
    def enumerateSubtypes(tp: Type): Option[List[Type]] =
      tp.typeSymbol match {
        // TODO case _ if tp.isTupleType => // recurse into component types?
        case UnitClass =>
          Some(List(UnitClass.tpe))
        case BooleanClass =>
          Some((List(ConstantType(Constant(true)), ConstantType(Constant(false)))))
        // TODO case _ if tp.isTupleType => // recurse into component types
        case modSym: ModuleClassSymbol =>
          Some(List(tp))
        // make sure it's not a primitive, else (5: Byte) match { case 5 => ... } sees no Byte
        case sym if !sym.isSealed || isPrimitiveValueClass(sym) =>
          patmatDebug("enum unsealed "+ (tp, sym, sym.isSealed, isPrimitiveValueClass(sym)))
          None
        case sym =>
          val subclasses = (
            sym.sealedDescendants.toList sortBy (_.sealedSortName)
            // symbols which are both sealed and abstract need not be covered themselves, because
            // all of their children must be and they cannot otherwise be created.
            filterNot (x => x.isSealed && x.isAbstractClass && !isPrimitiveValueClass(x)))
          patmatDebug("enum sealed -- subclasses: "+ (sym, subclasses))

          val tpApprox = typer.infer.approximateAbstracts(tp)
          val pre = tpApprox.prefix
          // valid subtypes are turned into checkable types, as we are entering the realm of the dynamic
          val validSubTypes = (subclasses flatMap {sym =>
              // have to filter out children which cannot match: see ticket #3683 for an example
              // compare to the fully known type `tp` (modulo abstract types),
              // so that we can rule out stuff like: sealed trait X[T]; class XInt extends X[Int] --> XInt not valid when enumerating X[String]
              // however, must approximate abstract types in

              val memberType  = nestedMemberType(sym, pre, tpApprox.typeSymbol.owner)
              val subTp       = appliedType(memberType, sym.typeParams.map(_ => WildcardType))
              val subTpApprox = typer.infer.approximateAbstracts(subTp) // TODO: needed?
              // patmatDebug("subtp"+(subTpApprox <:< tpApprox, subTpApprox, tpApprox))
              if (subTpApprox <:< tpApprox) Some(checkableType(subTp))
              else None
            })
          patmatDebug("enum sealed "+ (tp, tpApprox) + " as "+ validSubTypes)
          Some(validSubTypes)
      }

    // approximate a type to the static type that is fully checkable at run time,
    // hiding statically known but dynamically uncheckable information using existential quantification
    // TODO: this is subject to the availability of TypeTags (since an abstract type with a type tag is checkable at run time)
    def checkableType(tp: Type): Type = {
      // TODO: this is extremely rough...
      // replace type args by wildcards, since they can't be checked (don't use existentials: overkill)
      // TODO: when type tags are available, we will check -- when this is implemented, can we take that into account here?
      // similar to typer.infer.approximateAbstracts
      object typeArgsToWildcardsExceptArray extends TypeMap {
        def apply(tp: Type): Type = tp match {
          case TypeRef(pre, sym, args) if args.nonEmpty && (sym ne ArrayClass) =>
            TypeRef(pre, sym, args map (_ => WildcardType))
          case _ =>
            mapOver(tp)
        }
      }

      val res = typeArgsToWildcardsExceptArray(tp)
      patmatDebug("checkable "+(tp, res))
      res
    }

    // a type is "uncheckable" (for exhaustivity) if we don't statically know its subtypes (i.e., it's unsealed)
    // we consider tuple types with at least one component of a checkable type as a checkable type
    def uncheckableType(tp: Type): Boolean = {
      def tupleComponents(tp: Type) = tp.normalize.typeArgs
      val checkable = (
           (isTupleType(tp) && tupleComponents(tp).exists(tp => !uncheckableType(tp)))
        || enumerateSubtypes(tp).nonEmpty)
      // if (!checkable) patmatDebug("deemed uncheckable: "+ tp)
      !checkable
    }
  }
}

trait Analysis extends TypeAnalysis { self: PatternMatching =>
  import PatternMatchingStats._
  val global: Global
  import global.{Tree, Type, Symbol, CaseDef, Position, atPos, NoPosition,
    Select, Block, ThisType, SingleType, NoPrefix, NoType, definitions, needsOuterTest,
    ConstantType, Literal, Constant, gen, This, analyzer, EmptyTree, map2, NoSymbol, Traverser,
    Function, Typed, treeInfo, DefTree, ValDef, nme, appliedType, Name, WildcardType, Ident, TypeRef,
    UniqueType, RefinedType, currentUnit, SingletonType, singleType, ModuleClassSymbol,
    nestedMemberType, TypeMap, EmptyScope, Apply, If, Bind, lub, Alternative, deriveCaseDef, Match, MethodType, LabelDef, TypeTree, Throw, newTermName}

  import definitions._
  import analyzer.{Typer, ErrorUtils, formalTypes}

  import debugging.patmatDebug

  /**
   * Represent a match as a formula in propositional logic that encodes whether the match matches (abstractly: we only consider types)
   *
   * TODO: remove duplication between Cond and Prop
   */
  trait TreeMakerApproximation extends TreeMakers with FirstOrderLogic with CheckableTypeAnalysis { self: CodegenCore =>
    object Test {
      var currId = 0
    }
    case class Test(cond: Cond, treeMaker: TreeMaker) {
      // private val reusedBy = new scala.collection.mutable.HashSet[Test]
      var reuses: Option[Test] = None
      def registerReuseBy(later: Test): Unit = {
        assert(later.reuses.isEmpty, later.reuses)
        // reusedBy += later
        later.reuses = Some(this)
      }

      val id = { Test.currId += 1; Test.currId}
      override def toString =
        "T"+ id + "C("+ cond +")"  //+ (reuses map ("== T"+_.id) getOrElse (if(reusedBy.isEmpty) treeMaker else reusedBy mkString (treeMaker+ " -->(", ", ",")")))
    }

    // TODO: remove Cond, replace by Prop from Logic
    object Cond {
      var currId = 0
    }

    abstract class Cond {
      val id = { Cond.currId += 1; Cond.currId}
    }

    case object TrueCond extends Cond  {override def toString = "T"}
    case object FalseCond extends Cond {override def toString = "F"}

    case class AndCond(a: Cond, b: Cond) extends Cond {override def toString = a +"/\\"+ b}
    case class OrCond(a: Cond, b: Cond)  extends Cond {override def toString = "("+a+") \\/ ("+ b +")"}

    case class EqualityCond(val testedPath: Tree, val rhs: Tree) extends Cond {
      override def toString = testedPath +" == "+ rhs +"#"+ id
    }

    case class NonNullCond(val testedPath: Tree) extends Cond {
      override def toString = testedPath +" ne null " +"#"+ id
    }

    case class TypeCond(val testedPath: Tree, val pt: Type) extends Cond {
      override def toString = testedPath +" : "+ pt +"#"+ id
    }

//    class OuterEqCond(val testedPath: Tree, val expectedType: Type) extends Cond {
//      val expectedOuter = expectedTp.prefix match {
//        case ThisType(clazz)  => THIS(clazz)
//        case pre              => REF(pre.prefix, pre.termSymbol)
//      }
//
//      // ExplicitOuter replaces `Select(q, outerSym) OBJ_EQ expectedPrefix` by `Select(q, outerAccessor(outerSym.owner)) OBJ_EQ expectedPrefix`
//      // if there's an outer accessor, otherwise the condition becomes `true` -- TODO: can we improve needsOuterTest so there's always an outerAccessor?
//      val outer = expectedTp.typeSymbol.newMethod(vpmName.outer) setInfo expectedTp.prefix setFlag SYNTHETIC
//
//      (Select(codegen._asInstanceOf(testedBinder, expectedTp), outer)) OBJ_EQ expectedOuter
//    }

    def /\(conds: Iterable[Cond]) = if (conds.isEmpty) TrueCond else conds.reduceLeft(AndCond(_, _))
    def \/(conds: Iterable[Cond]) = if (conds.isEmpty) FalseCond else conds.reduceLeft(OrCond(_, _))

    object IrrefutableExtractorTreeMaker {
      // will an extractor with unapply method of methodtype `tp` always succeed?
      // note: this assumes the other side-conditions implied by the extractor are met
      // (argument of the right type, length check succeeds for unapplySeq,...)
      def irrefutableExtractorType(tp: Type): Boolean = tp.resultType.dealias match {
        case TypeRef(_, SomeClass, _) => true
        // probably not useful since this type won't be inferred nor can it be written down (yet)
        case ConstantType(Constant(true)) => true
        case _ => false
      }

      def unapply(xtm: ExtractorTreeMaker): Option[(Tree, Symbol)] = xtm match {
        case ExtractorTreeMaker(extractor, None, nextBinder) if irrefutableExtractorType(extractor.tpe) =>
          Some((extractor, nextBinder))
        case _ =>
          None
      }
    }

    private[this] val uniqueEqualityConds = new scala.collection.mutable.HashMap[(Tree, Tree), EqualityCond]
    private[this] val uniqueNonNullConds = new scala.collection.mutable.HashMap[Tree, NonNullCond]
    private[this] val uniqueTypeConds = new scala.collection.mutable.HashMap[(Tree, Type), TypeCond]

    def uniqueEqualityCond(testedPath: Tree, rhs: Tree): EqualityCond = uniqueEqualityConds getOrElseUpdate((testedPath, rhs), EqualityCond(testedPath, rhs))
    def uniqueNonNullCond (testedPath: Tree): NonNullCond = uniqueNonNullConds getOrElseUpdate(testedPath, NonNullCond(testedPath))
    def uniqueTypeCond(testedPath: Tree, pt: Type): TypeCond = uniqueTypeConds getOrElseUpdate((testedPath, pt), TypeCond(testedPath, pt))

    class TreeMakersToCondsIgnoreNullChecks(root: Symbol) extends TreeMakersToConds(root) {
      override def nonNullCond(p: Tree): Cond = TrueCond
    }

    // returns (tree, tests), where `tree` will be used to refer to `root` in `tests`
    class TreeMakersToConds(val root: Symbol) {
      // overridden in TreeMakersToPropsIgnoreNullChecks
      def nonNullCond(p: Tree): Cond = uniqueNonNullCond(p)

      // a variable in this set should never be replaced by a tree that "does not consist of a selection on a variable in this set" (intuitively)
      private val pointsToBound = scala.collection.mutable.HashSet(root)
      private val trees         = scala.collection.mutable.HashSet.empty[Tree]

      // the substitution that renames variables to variables in pointsToBound
      private var normalize: Substitution  = EmptySubstitution
      private var substitutionComputed = false

      // replaces a variable (in pointsToBound) by a selection on another variable in pointsToBound
      // in the end, instead of having x1, x1.hd, x2, x2.hd, ... flying around,
      // we want something like x1, x1.hd, x1.hd.tl, x1.hd.tl.hd, so that we can easily recognize when
      // we're testing the same variable
      // TODO check:
      //   pointsToBound -- accumSubst.from == Set(root) && (accumSubst.from.toSet -- pointsToBound) isEmpty
      private var accumSubst: Substitution = EmptySubstitution

      // hashconsing trees (modulo value-equality)
      def unique(t: Tree, tpOverride: Type = NoType): Tree =
        trees find (a => a.correspondsStructure(t)(sameValue)) match {
          case Some(orig) =>
            // patmatDebug("unique: "+ (t eq orig, orig))
            orig
          case _ =>
            trees += t
            if (tpOverride != NoType) t setType tpOverride
            else t
        }

      def uniqueTp(tp: Type): Type = tp match {
        // typerefs etc are already hashconsed
        case _ : UniqueType                      => tp
        case tp@RefinedType(parents, EmptyScope) => tp.memo(tp: Type)(identity) // TODO: does this help?
        case _                                   => tp
      }

      // produce the unique tree used to refer to this binder
      // the type of the binder passed to the first invocation
      // determines the type of the tree that'll be returned for that binder as of then
      final def binderToUniqueTree(b: Symbol) =
        unique(accumSubst(normalize(CODE.REF(b))), b.tpe)

      // note that the sequencing of operations is important: must visit in same order as match execution
      // binderToUniqueTree uses the type of the first symbol that was encountered as the type for all future binders
      abstract class TreeMakerToCond extends (TreeMaker => Cond) {
        // requires(if (!substitutionComputed))
        def updateSubstitution(subst: Substitution): Unit = {
          // find part of substitution that replaces bound symbols by new symbols, and reverse that part
          // so that we don't introduce new aliases for existing symbols, thus keeping the set of bound symbols minimal
          val (boundSubst, unboundSubst) = (subst.from zip subst.to) partition {
            case (f, t) =>
              t.isInstanceOf[Ident] && (t.symbol ne NoSymbol) && pointsToBound(f)
          }
          val (boundFrom, boundTo) = boundSubst.unzip
          val (unboundFrom, unboundTo) = unboundSubst.unzip

          // reverse substitution that would otherwise replace a variable we already encountered by a new variable
          // NOTE: this forgets the more precise type we have for these later variables, but that's probably okay
          normalize >>= Substitution(boundTo map (_.symbol), boundFrom map (CODE.REF(_)))
          // patmatDebug ("normalize subst: "+ normalize)

          val okSubst = Substitution(unboundFrom, unboundTo map (normalize(_))) // it's important substitution does not duplicate trees here -- it helps to keep hash consing simple, anyway
          pointsToBound ++= ((okSubst.from, okSubst.to).zipped filter { (f, t) => pointsToBound exists (sym => t.exists(_.symbol == sym)) })._1
          // patmatDebug("pointsToBound: "+ pointsToBound)

          accumSubst >>= okSubst
          // patmatDebug("accumSubst: "+ accumSubst)
        }

        def handleUnknown(tm: TreeMaker): Cond

        /** apply itself must render a faithful representation of the TreeMaker
         *
         * Concretely, TrueCond must only be used to represent a TreeMaker that is sure to match and that does not do any computation at all
         * e.g., doCSE relies on apply itself being sound in this sense (since it drops TreeMakers that are approximated to TrueCond -- SI-6077)
         *
         * handleUnknown may be customized by the caller to approximate further
         *
         * TODO: don't ignore outer-checks
         */
        def apply(tm: TreeMaker): Cond = {
          if (!substitutionComputed) updateSubstitution(tm.subPatternsAsSubstitution)

          tm match {
            case ttm@TypeTestTreeMaker(prevBinder, testedBinder, pt, _)   =>
              object condStrategy extends TypeTestTreeMaker.TypeTestCondStrategy {
                type Result                                           = Cond
                def and(a: Result, b: Result)                         = AndCond(a, b)
                def outerTest(testedBinder: Symbol, expectedTp: Type) = TrueCond // TODO OuterEqCond(testedBinder, expectedType)
                def typeTest(b: Symbol, pt: Type) = { // a type test implies the tested path is non-null (null.isInstanceOf[T] is false for all T)
                  val p = binderToUniqueTree(b);                        AndCond(nonNullCond(p), uniqueTypeCond(p, uniqueTp(pt)))
                }
                def nonNullTest(testedBinder: Symbol)                 = nonNullCond(binderToUniqueTree(testedBinder))
                def equalsTest(pat: Tree, testedBinder: Symbol)       = uniqueEqualityCond(binderToUniqueTree(testedBinder), unique(pat))
                def eqTest(pat: Tree, testedBinder: Symbol)           = uniqueEqualityCond(binderToUniqueTree(testedBinder), unique(pat)) // TODO: eq, not ==
                def tru                                               = TrueCond
              }
              ttm.renderCondition(condStrategy)
            case EqualityTestTreeMaker(prevBinder, patTree, _)        => uniqueEqualityCond(binderToUniqueTree(prevBinder), unique(patTree))
            case AlternativesTreeMaker(_, altss, _)                   => \/(altss map (alts => /\(alts map this)))
            case ProductExtractorTreeMaker(testedBinder, None)        => nonNullCond(binderToUniqueTree(testedBinder))
            case SubstOnlyTreeMaker(_, _)                             => TrueCond
            case GuardTreeMaker(guard) =>
              guard.tpe match {
                case ConstantType(Constant(true))  => TrueCond
                case ConstantType(Constant(false)) => FalseCond
                case _                             => handleUnknown(tm)
              }
            case ExtractorTreeMaker(_, _, _) |
                 ProductExtractorTreeMaker(_, _) |
                 BodyTreeMaker(_, _)               => handleUnknown(tm)
          }
        }
      }


      private val irrefutableExtractor: PartialFunction[TreeMaker, Cond] = {
        // the extra condition is None, the extractor's result indicates it always succeeds,
        // (the potential type-test for the argument is represented by a separate TypeTestTreeMaker)
        case IrrefutableExtractorTreeMaker(_, _) => TrueCond
      }

      // special-case: interpret pattern `List()` as `Nil`
      // TODO: make it more general List(1, 2) => 1 :: 2 :: Nil  -- not sure this is a good idea...
      private val rewriteListPattern: PartialFunction[TreeMaker, Cond] = {
        case p @ ExtractorTreeMaker(_, _, testedBinder)
          if testedBinder.tpe.typeSymbol == ListClass && p.checkedLength == Some(0) =>
            uniqueEqualityCond(binderToUniqueTree(p.prevBinder), unique(Ident(NilModule) setType NilModule.tpe))
      }
      val fullRewrite      = (irrefutableExtractor orElse rewriteListPattern)
      val refutableRewrite = irrefutableExtractor

      @inline def onUnknown(handler: TreeMaker => Cond) = new TreeMakerToCond {
        def handleUnknown(tm: TreeMaker) = handler(tm)
      }

      // used for CSE -- rewrite all unknowns to False (the most conserative option)
      object conservative extends TreeMakerToCond {
        def handleUnknown(tm: TreeMaker) = FalseCond
      }

      final def approximateMatch(cases: List[List[TreeMaker]], treeMakerToCond: TreeMakerToCond = conservative) ={
        val testss = cases.map { _ map (tm => Test(treeMakerToCond(tm), tm)) }
        substitutionComputed = true // a second call to approximateMatch should not re-compute the substitution (would be wrong)
        testss
      }
    }

    def approximateMatchConservative(root: Symbol, cases: List[List[TreeMaker]]): List[List[Test]] =
      (new TreeMakersToConds(root)).approximateMatch(cases)

    def prepareNewAnalysis() = { Var.resetUniques(); Const.resetUniques() }

    def condToProp(t: Cond): Prop = t match {
      case AndCond(a, b) => And(condToProp(a), condToProp(b))
      case OrCond(a, b) => Or(condToProp(a), condToProp(b))
      case TrueCond => True
      case FalseCond => False
      case TypeCond(p, pt) => Eq(Var(p), TypeConst(checkableType(pt)))
      case EqualityCond(p, q) => Eq(Var(p), ValueConst(q))
      case NonNullCond(p) => Not(Eq(Var(p), NullConst))
    }

    object Var {
      private var _nextId = 0
      def nextId = {_nextId += 1; _nextId}

      def resetUniques() = {_nextId = 0; uniques.clear()}
      private val uniques = new mutable.HashMap[Tree, Var]
      def apply(x: Tree): Var = uniques getOrElseUpdate(x, new Var(x, x.tpe))
    }
    class Var(val path: Tree, staticTp: Type) extends AbsVar {
      private[this] val id: Int = Var.nextId

      // private[this] var canModify: Option[Array[StackTraceElement]] = None
      private[this] def ensureCanModify() = {} //if (canModify.nonEmpty) patmatDebug("BUG!"+ this +" modified after having been observed: "+ canModify.get.mkString("\n"))

      private[this] def observed() = {} //canModify = Some(Thread.currentThread.getStackTrace)

      // don't access until all potential equalities have been registered using registerEquality
      private[this] val symForEqualsTo = new mutable.HashMap[Const, Sym]

      // when looking at the domain, we only care about types we can check at run time
      val staticTpCheckable: Type = checkableType(staticTp)

      private[this] var _mayBeNull = false
      def registerNull(): Unit = { ensureCanModify; if (NullTp <:< staticTpCheckable) _mayBeNull = true }
      def mayBeNull: Boolean = _mayBeNull

      // case None => domain is unknown,
      // case Some(List(tps: _*)) => domain is exactly tps
      // we enumerate the subtypes of the full type, as that allows us to filter out more types statically,
      // once we go to run-time checks (on Const's), convert them to checkable types
      // TODO: there seems to be bug for singleton domains (variable does not show up in model)
      lazy val domain: Option[Set[Const]] = {
        val subConsts = enumerateSubtypes(staticTp).map{ tps =>
          tps.toSet[Type].map{ tp =>
            val domainC = TypeConst(tp)
            registerEquality(domainC)
            domainC
          }
        }

        val allConsts =
          if (mayBeNull) {
            registerEquality(NullConst)
            subConsts map (_ + NullConst)
          } else
            subConsts

        observed; allConsts
      }

      // populate equalitySyms
      // don't care about the result, but want only one fresh symbol per distinct constant c
      def registerEquality(c: Const): Unit = {ensureCanModify; symForEqualsTo getOrElseUpdate(c, Sym(this, c))}

      // return the symbol that represents this variable being equal to the constant `c`, if it exists, otherwise False (for robustness)
      // (registerEquality(c) must have been called prior, either when constructing the domain or from outside)
      def propForEqualsTo(c: Const): Prop = {observed; symForEqualsTo.getOrElse(c, False)}

      // [implementation NOTE: don't access until all potential equalities have been registered using registerEquality]p
      /** the information needed to construct the boolean proposition that encods the equality proposition (V = C)
       *
       * that models a type test pattern `_: C` or constant pattern `C`, where the type test gives rise to a TypeConst C,
       * and the constant pattern yields a ValueConst C
       *
       * for exhaustivity, we really only need implication (e.g., V = 1 implies that V = 1 /\ V = Int, if both tests occur in the match,
       * and thus in this variable's equality symbols), but reachability also requires us to model things like V = 1 precluding V = "1"
       */
      lazy val implications = {
        /** when we know V = C, which other equalities must hold
         *
         * in general, equality to some type implies equality to its supertypes
         * (this multi-valued kind of equality is necessary for unreachability)
         * note that we use subtyping as a model for implication between instanceof tests
         * i.e., when S <:< T we assume x.isInstanceOf[S] implies x.isInstanceOf[T]
         * unfortunately this is not true in general (see e.g. SI-6022)
         */
        def implies(lower: Const, upper: Const): Boolean =
          // values and null
            lower == upper ||
          // type implication
            (lower != NullConst && !upper.isValue &&
             instanceOfTpImplies(if (lower.isValue) lower.wideTp else lower.tp, upper.tp))

          // if(r) patmatDebug("implies    : "+(lower, lower.tp, upper, upper.tp))
          // else  patmatDebug("NOT implies: "+(lower, upper))


        /** does V = C preclude V having value `other`?
         (1) V = null is an exclusive assignment,
         (2) V = A and V = B, for A and B value constants, are mutually exclusive unless A == B
             we err on the safe side, for example:
               - assume `val X = 1; val Y = 1`, then
                 (2: Int) match { case X => case Y =>  <falsely considered reachable>  }
               - V = 1 does not preclude V = Int, or V = Any, it could be said to preclude V = String, but we don't model that

         (3) for types we could try to do something fancy, but be conservative and just say no
         */
        def excludes(a: Const, b: Const): Boolean =
          a != b && ((a == NullConst || b == NullConst) || (a.isValue && b.isValue))

          // if(r) patmatDebug("excludes    : "+(a, a.tp, b, b.tp))
          // else  patmatDebug("NOT excludes: "+(a, b))

/*
[ HALF BAKED FANCINESS: //!equalitySyms.exists(common => implies(common.const, a) && implies(common.const, b)))
 when type tests are involved, we reason (conservatively) under a closed world assumption,
 since we are really only trying to counter the effects of the symbols that we introduce to model type tests
 we don't aim to model the whole subtyping hierarchy, simply to encode enough about subtyping to do unreachability properly

 consider the following hierarchy:

    trait A
    trait B
    trait C
    trait AB extends B with A

  // two types are mutually exclusive if there is no equality symbol whose constant implies both
  object Test extends App {
    def foo(x: Any) = x match {
      case _ : C  => println("C")
      case _ : AB => println("AB")
      case _ : (A with B) => println("AB'")
      case _ : B  => println("B")
      case _ : A  => println("A")
    }

 of course this kind of reasoning is not true in general,
 but we can safely pretend types are mutually exclusive as long as there are no counter-examples in the match we're analyzing}
*/

        val excludedPair = new mutable.HashSet[ExcludedPair]

        case class ExcludedPair(a: Const, b: Const) {
          override def equals(o: Any) = o match {
            case ExcludedPair(aa, bb) => (a == aa && b == bb) || (a == bb && b == aa)
            case _ => false
          }
          // make ExcludedPair(a, b).hashCode == ExcludedPair(b, a).hashCode
          override def hashCode = a.hashCode ^ b.hashCode
        }

        equalitySyms map { sym =>
          // if we've already excluded the pair at some point (-A \/ -B), then don't exclude the symmetric one (-B \/ -A)
          // (nor the positive implications -B \/ A, or -A \/ B, which would entail the equality axioms falsifying the whole formula)
          val todo = equalitySyms filterNot (b => (b.const == sym.const) || excludedPair(ExcludedPair(b.const, sym.const)))
          val (excluded, notExcluded) = todo partition (b => excludes(sym.const, b.const))
          val implied = notExcluded filter (b => implies(sym.const, b.const))

          patmatDebug("eq axioms for: "+ sym.const)
          patmatDebug("excluded: "+ excluded)
          patmatDebug("implied: "+ implied)

          excluded foreach { excludedSym => excludedPair += ExcludedPair(sym.const, excludedSym.const)}

          (sym, implied, excluded)
        }
      }

      // accessing after calling registerNull will result in inconsistencies
      lazy val domainSyms: Option[Set[Sym]] = domain map { _ map symForEqualsTo }

      lazy val symForStaticTp: Option[Sym]  = symForEqualsTo.get(TypeConst(staticTpCheckable))

      // don't access until all potential equalities have been registered using registerEquality
      private lazy val equalitySyms = {observed; symForEqualsTo.values.toList}

      // don't call until all equalities have been registered and registerNull has been called (if needed)
      def describe = {
        def domain_s = domain match {
          case Some(d) => d mkString (" ::= ", " | ", "// "+ symForEqualsTo.keys)
          case _       => symForEqualsTo.keys mkString (" ::= ", " | ", " | ...")
        }
        s"$this: ${staticTp}${domain_s} // = $path"
      }
      override def toString = "V"+ id
    }


    // all our variables range over types
    // a literal constant becomes ConstantType(Constant(v)) when the type allows it (roughly, anyval + string + null)
    // equality between variables: SingleType(x) (note that pattern variables cannot relate to each other -- it's always patternVar == nonPatternVar)
    object Const {
      def resetUniques() = {_nextTypeId = 0; _nextValueId = 0; uniques.clear() ; trees.clear()}

      private var _nextTypeId = 0
      def nextTypeId = {_nextTypeId += 1; _nextTypeId}

      private var _nextValueId = 0
      def nextValueId = {_nextValueId += 1; _nextValueId}

      private val uniques = new mutable.HashMap[Type, Const]
      private[TreeMakerApproximation] def unique(tp: Type, mkFresh: => Const): Const =
        uniques.get(tp).getOrElse(
          uniques.find {case (oldTp, oldC) => oldTp =:= tp} match {
            case Some((_, c)) =>
              patmatDebug("unique const: "+ (tp, c))
              c
            case _ =>
              val fresh = mkFresh
              patmatDebug("uniqued const: "+ (tp, fresh))
              uniques(tp) = fresh
              fresh
          })

      private val trees = mutable.HashSet.empty[Tree]

      // hashconsing trees (modulo value-equality)
      private[TreeMakerApproximation] def uniqueTpForTree(t: Tree): Type =
        // a new type for every unstable symbol -- only stable value are uniqued
        // technically, an unreachable value may change between cases
        // thus, the failure of a case that matches on a mutable value does not exclude the next case succeeding
        // (and thuuuuus, the latter case must be considered reachable)
        if (!t.symbol.isStable) t.tpe.narrow
        else trees find (a => a.correspondsStructure(t)(sameValue)) match {
          case Some(orig) =>
            patmatDebug("unique tp for tree: "+ (orig, orig.tpe))
            orig.tpe
          case _ =>
            // duplicate, don't mutate old tree (TODO: use a map tree -> type instead?)
            val treeWithNarrowedType = t.duplicate setType t.tpe.narrow
            patmatDebug("uniqued: "+ (t, t.tpe, treeWithNarrowedType.tpe))
            trees += treeWithNarrowedType
            treeWithNarrowedType.tpe
        }
    }

    sealed abstract class Const {
      def tp: Type
      def wideTp: Type

      def isAny = wideTp.typeSymbol == AnyClass
      def isValue: Boolean //= tp.isStable

      // note: use reference equality on Const since they're hash-consed (doing type equality all the time is too expensive)
      // the equals inherited from AnyRef does just this
    }

    // find most precise super-type of tp that is a class
    // we skip non-class types (singleton types, abstract types) so that we can
    // correctly compute how types relate in terms of the values they rule out
    // e.g., when we know some value must be of type T, can it still be of type S? (this is the positive formulation of what `excludes` on Const computes)
    // since we're talking values, there must have been a class involved in creating it, so rephrase our types in terms of classes
    // (At least conceptually: `true` is an instance of class `Boolean`)
    private def widenToClass(tp: Type): Type =
      if (tp.typeSymbol.isClass) tp
      else tp.baseType(tp.baseClasses.head)

    object TypeConst extends TypeConstExtractor {
      def apply(tp: Type) = {
        if (tp =:= NullTp) NullConst
        else if (tp.isInstanceOf[SingletonType]) ValueConst.fromType(tp)
        else Const.unique(tp, new TypeConst(tp))
      }
      def unapply(c: TypeConst): Some[Type] = Some(c.tp)
    }

    // corresponds to a type test that does not imply any value-equality (well, except for outer checks, which we don't model yet)
    sealed class TypeConst(val tp: Type) extends Const {
      assert(!(tp =:= NullTp))
      /*private[this] val id: Int = */ Const.nextTypeId

      val wideTp = widenToClass(tp)
      def isValue = false
      override def toString = tp.toString //+"#"+ id
    }

    // p is a unique type or a constant value
    object ValueConst {
      def fromType(tp: Type) = {
        assert(tp.isInstanceOf[SingletonType])
        val toString = tp match {
          case ConstantType(c) => c.escapedStringValue
          case _ => tp.toString
        }
        Const.unique(tp, new ValueConst(tp, tp.widen, toString))
      }
      def apply(p: Tree) = {
        val tp = p.tpe.normalize
        if (tp =:= NullTp) NullConst
        else {
          val wideTp = widenToClass(tp)

          val narrowTp =
            if (tp.isInstanceOf[SingletonType]) tp
            else p match {
              case Literal(c) =>
                if (c.tpe.typeSymbol == UnitClass) c.tpe
                else ConstantType(c)
              case Ident(_) if p.symbol.isStable =>
                // for Idents, can encode uniqueness of symbol as uniqueness of the corresponding singleton type
                // for Selects, which are handled by the next case, the prefix of the select varies independently of the symbol (see pos/virtpatmat_unreach_select.scala)
                singleType(tp.prefix, p.symbol)
              case _ =>
                Const.uniqueTpForTree(p)
            }

          val toString =
            if (hasStableSymbol(p)) p.symbol.name.toString // tp.toString
            else p.toString //+"#"+ id

          Const.unique(narrowTp, new ValueConst(narrowTp, checkableType(wideTp), toString)) // must make wide type checkable so that it is comparable to types from TypeConst
        }
      }
    }
    sealed class ValueConst(val tp: Type, val wideTp: Type, override val toString: String) extends Const {
      // patmatDebug("VC"+(tp, wideTp, toString))
      assert(!(tp =:= NullTp)) // TODO: assert(!tp.isStable)
      /*private[this] val id: Int = */Const.nextValueId
      def isValue = true
    }

    lazy val NullTp = ConstantType(Constant(null))
    case object NullConst extends Const {
      def tp     = NullTp
      def wideTp = NullTp

      def isValue = true
      override def toString = "null"
    }


    // turns a case (represented as a list of abstract tests)
    // into a proposition that is satisfiable if the case may match
    def symbolicCase(tests: List[Test]): Prop = {
      val testsBeforeBody = tests.takeWhile(t => !t.treeMaker.isInstanceOf[BodyTreeMaker])
      /\(testsBeforeBody.map(t => condToProp(t.cond)))
    }

    def showTreeMakers(cases: List[List[TreeMaker]]) = {
      patmatDebug("treeMakers:")
      patmatDebug(alignAcrossRows(cases, ">>"))
    }

    def showTests(testss: List[List[Test]]) = {
      patmatDebug("tests: ")
      patmatDebug(alignAcrossRows(testss, "&"))
    }
  }

  trait SymbolicMatchAnalysis extends TreeMakerApproximation  { self: CodegenCore =>
     // TODO: model dependencies between variables: if V1 corresponds to (x: List[_]) and V2 is (x.hd), V2 cannot be assigned when V1 = null or V1 = Nil
    // right now hackily implement this by pruning counter-examples
    // unreachability would also benefit from a more faithful representation


    // reachability (dead code)

    // computes the first 0-based case index that is unreachable (if any)
    // a case is unreachable if it implies its preceding cases
    // call C the formula that is satisfiable if the considered case matches
    // call P the formula that is satisfiable if the cases preceding it match
    // the case is reachable if there is a model for -P /\ C,
    // thus, the case is unreachable if there is no model for -(-P /\ C),
    // or, equivalently, P \/ -C, or C => P
    def unreachableCase(prevBinder: Symbol, cases: List[List[TreeMaker]], pt: Type): Option[Int] = {
      val start = if (Statistics.canEnable) Statistics.startTimer(patmatAnaReach) else null

      // use the same approximator so we share variables,
      // but need different conditions depending on whether we're conservatively looking for failure or success
      // don't rewrite List-like patterns, as List() and Nil need to distinguished for unreachability
      val approx = new TreeMakersToConds(prevBinder)
      def approximate(default: Cond) = approx.approximateMatch(cases, approx.onUnknown { tm =>
        approx.refutableRewrite.applyOrElse(tm, (_: TreeMaker) => default )
      })

      val testCasesOk   = approximate(TrueCond)
      val testCasesFail = approximate(FalseCond)

      prepareNewAnalysis()

      val propsCasesOk   = testCasesOk   map symbolicCase
      val propsCasesFail = testCasesFail map (t => Not(symbolicCase(t)))

      try {
        val (eqAxiomsFail, symbolicCasesFail) = removeVarEq(propsCasesFail, modelNull = true)
        val (eqAxiomsOk, symbolicCasesOk)     = removeVarEq(propsCasesOk,   modelNull = true)
        val eqAxioms = simplifyFormula(andFormula(eqAxiomsOk, eqAxiomsFail)) // I'm pretty sure eqAxiomsOk == eqAxiomsFail, but not 100% sure.

        val prefix   = formulaBuilder
        addFormula(prefix, eqAxioms)

        var prefixRest = symbolicCasesFail
        var current    = symbolicCasesOk
        var reachable  = true
        var caseIndex  = 0

        patmatDebug("reachability, vars:\n"+ ((propsCasesFail flatMap gatherVariables).distinct map (_.describe) mkString ("\n")))
        patmatDebug("equality axioms:\n"+ cnfString(eqAxiomsOk))

        // invariant (prefixRest.length == current.length) && (prefix.reverse ++ prefixRest == symbolicCasesFail)
        // termination: prefixRest.length decreases by 1
        while (prefixRest.nonEmpty && reachable) {
          val prefHead = prefixRest.head
          caseIndex += 1
          prefixRest = prefixRest.tail
          if (prefixRest.isEmpty) reachable = true
          else {
            addFormula(prefix, prefHead)
            current = current.tail
            val model = findModelFor(andFormula(current.head, toFormula(prefix)))

            // patmatDebug("trying to reach:\n"+ cnfString(current.head) +"\nunder prefix:\n"+ cnfString(prefix))
            // if (NoModel ne model) patmatDebug("reached: "+ modelString(model))

            reachable = NoModel ne model
          }
        }

        if (Statistics.canEnable) Statistics.stopTimer(patmatAnaReach, start)

        if (reachable) None else Some(caseIndex)
      } catch {
        case ex: AnalysisBudget.Exception =>
          ex.warn(prevBinder.pos, "unreachability")
          None // CNF budget exceeded
      }
    }

    // exhaustivity

    def exhaustive(prevBinder: Symbol, cases: List[List[TreeMaker]], pt: Type): List[String] = if (uncheckableType(prevBinder.info)) Nil else {
      // customize TreeMakersToConds (which turns a tree of tree makers into a more abstract DAG of tests)
      // - approximate the pattern `List()` (unapplySeq on List with empty length) as `Nil`,
      //   otherwise the common (xs: List[Any]) match { case List() => case x :: xs => } is deemed unexhaustive
      // - back off (to avoid crying exhaustive too often) when:
      //    - there are guards -->
      //    - there are extractor calls (that we can't secretly/soundly) rewrite
      val start = if (Statistics.canEnable) Statistics.startTimer(patmatAnaExhaust) else null
      var backoff = false

      val approx = new TreeMakersToCondsIgnoreNullChecks(prevBinder)
      val tests = approx.approximateMatch(cases, approx.onUnknown { tm =>
        approx.fullRewrite.applyOrElse[TreeMaker, Cond](tm, {
          case BodyTreeMaker(_, _) => TrueCond // irrelevant -- will be discarded by symbolCase later
          case _ => // patmatDebug("backing off due to "+ tm)
            backoff = true
            FalseCond
        })
      })

      if (backoff) Nil else {
        val prevBinderTree = approx.binderToUniqueTree(prevBinder)

        prepareNewAnalysis()

        val symbolicCases = tests map symbolicCase


        // TODO: null tests generate too much noise, so disabled them -- is there any way to bring them back?
        // assuming we're matching on a non-null scrutinee (prevBinder), when does the match fail?
        // val nonNullScrutineeCond =
        //   assume non-null for all the components of the tuple we're matching on (if we're matching on a tuple)
        //   if (isTupleType(prevBinder.tpe))
        //     prevBinder.tpe.typeArgs.mapWithIndex{case (_, i) => NonNullCond(codegen.tupleSel(prevBinderTree)(i))}.reduceLeft(AndCond)
        //   else
        //     NonNullCond(prevBinderTree)
        // val matchFails = And(symbolic(nonNullScrutineeCond), Not(symbolicCases reduceLeft (Or(_, _))))

        // when does the match fail?
        val matchFails = Not(\/(symbolicCases))

  // debug output:
        patmatDebug("analysing:")
        showTreeMakers(cases)
        showTests(tests)

        // patmatDebug("\nvars:\n"+ (vars map (_.describe) mkString ("\n")))
        // patmatDebug("\nmatchFails as CNF:\n"+ cnfString(propToSolvable(matchFails)))

        try {
          // find the models (under which the match fails)
          val matchFailModels = findAllModelsFor(propToSolvable(matchFails))

          val scrutVar = Var(prevBinderTree)
          val counterExamples = matchFailModels.map(modelToCounterExample(scrutVar))

          val pruned = CounterExample.prune(counterExamples).map(_.toString).sorted

          if (Statistics.canEnable) Statistics.stopTimer(patmatAnaExhaust, start)
          pruned
        } catch {
          case ex : AnalysisBudget.Exception =>
            ex.warn(prevBinder.pos, "exhaustivity")
            Nil // CNF budget exceeded
        }
      }
    }

    object CounterExample {
      def prune(examples: List[CounterExample]): List[CounterExample] = {
        val distinct = examples.filterNot(_ == NoExample).toSet
        distinct.filterNot(ce => distinct.exists(other => (ce ne other) && ce.coveredBy(other))).toList
      }
    }

    // a way to construct a value that will make the match fail: a constructor invocation, a constant, an object of some type)
    class CounterExample {
      protected[SymbolicMatchAnalysis] def flattenConsArgs: List[CounterExample] = Nil
      def coveredBy(other: CounterExample): Boolean = this == other || other == WildcardExample
    }
    case class ValueExample(c: ValueConst) extends CounterExample { override def toString = c.toString }
    case class TypeExample(c: Const)  extends CounterExample { override def toString = "(_ : "+ c +")" }
    case class NegativeExample(eqTo: Const, nonTrivialNonEqualTo: List[Const]) extends CounterExample {
      // require(nonTrivialNonEqualTo.nonEmpty, nonTrivialNonEqualTo)
      override def toString = {
        val negation =
          if (nonTrivialNonEqualTo.tail.isEmpty) nonTrivialNonEqualTo.head.toString
          else nonTrivialNonEqualTo.map(_.toString).sorted.mkString("(", ", ", ")")
        "(x: "+ eqTo +" forSome x not in "+ negation +")"
      }
    }
    case class ListExample(ctorArgs: List[CounterExample]) extends CounterExample {
      protected[SymbolicMatchAnalysis] override def flattenConsArgs: List[CounterExample] = ctorArgs match {
        case hd :: tl :: Nil => hd :: tl.flattenConsArgs
        case _ => Nil
      }
      protected[SymbolicMatchAnalysis] lazy val elems = flattenConsArgs

      override def coveredBy(other: CounterExample): Boolean =
        other match {
          case other@ListExample(_) =>
            this == other || ((elems.length == other.elems.length) && (elems zip other.elems).forall{case (a, b) => a coveredBy b})
          case _ => super.coveredBy(other)
        }

      override def toString = elems.mkString("List(", ", ", ")")
    }
    case class TupleExample(ctorArgs: List[CounterExample]) extends CounterExample {
      override def toString = ctorArgs.mkString("(", ", ", ")")

      override def coveredBy(other: CounterExample): Boolean =
        other match {
          case TupleExample(otherArgs) =>
            this == other || ((ctorArgs.length == otherArgs.length) && (ctorArgs zip otherArgs).forall{case (a, b) => a coveredBy b})
          case _ => super.coveredBy(other)
        }
    }
    case class ConstructorExample(cls: Symbol, ctorArgs: List[CounterExample]) extends CounterExample {
      override def toString = cls.decodedName + (if (cls.isModuleClass) "" else ctorArgs.mkString("(", ", ", ")"))
    }

    case object WildcardExample extends CounterExample { override def toString = "_" }
    case object NoExample extends CounterExample { override def toString = "??" }

    def modelToVarAssignment(model: Model): Map[Var, (Seq[Const], Seq[Const])] =
      model.toSeq.groupBy{f => f match {case (sym, value) => sym.variable} }.mapValues{ xs =>
        val (trues, falses) = xs.partition(_._2)
        (trues map (_._1.const), falses map (_._1.const))
        // should never be more than one value in trues...
      }

    def varAssignmentString(varAssignment: Map[Var, (Seq[Const], Seq[Const])]) =
      varAssignment.toSeq.sortBy(_._1.toString).map { case (v, (trues, falses)) =>
         val assignment = "== "+ (trues mkString("(", ", ", ")")) +"  != ("+ (falses mkString(", ")) +")"
         v +"(="+ v.path +": "+ v.staticTpCheckable +") "+ assignment
       }.mkString("\n")

    // return constructor call when the model is a true counter example
    // (the variables don't take into account type information derived from other variables,
    //  so, naively, you might try to construct a counter example like _ :: Nil(_ :: _, _ :: _),
    //  since we didn't realize the tail of the outer cons was a Nil)
    def modelToCounterExample(scrutVar: Var)(model: Model): CounterExample = {
      // x1 = ...
      // x1.hd = ...
      // x1.tl = ...
      // x1.hd.hd = ...
      // ...
      val varAssignment = modelToVarAssignment(model)

      patmatDebug("var assignment for model "+ model +":\n"+ varAssignmentString(varAssignment))

      // chop a path into a list of symbols
      def chop(path: Tree): List[Symbol] = path match {
        case Ident(_) => List(path.symbol)
        case Select(pre, name) => chop(pre) :+ path.symbol
        case _ =>
          // patmatDebug("don't know how to chop "+ path)
          Nil
      }

      // turn the variable assignments into a tree
      // the root is the scrutinee (x1), edges are labelled by the fields that are assigned
      // a node is a variable example (which is later turned into a counter example)
      object VariableAssignment {
        private def findVar(path: List[Symbol]) = path match {
          case List(root) if root == scrutVar.path.symbol => Some(scrutVar)
          case _ => varAssignment.find{case (v, a) => chop(v.path) == path}.map(_._1)
        }

        private val uniques = new mutable.HashMap[Var, VariableAssignment]
        private def unique(variable: Var): VariableAssignment =
          uniques.getOrElseUpdate(variable, {
            val (eqTo, neqTo) = varAssignment.getOrElse(variable, (Nil, Nil)) // TODO
            VariableAssignment(variable, eqTo.toList, neqTo.toList, mutable.HashMap.empty)
          })

        def apply(variable: Var): VariableAssignment = {
          val path  = chop(variable.path)
          val pre   = path.init
          val field = path.last

          val newCtor = unique(variable)

          if (pre.isEmpty) newCtor
          else {
            findVar(pre) foreach { preVar =>
              val outerCtor = this(preVar)
              outerCtor.fields(field) = newCtor
            }
            newCtor
          }
        }
      }

      // node in the tree that describes how to construct a counter-example
      case class VariableAssignment(variable: Var, equalTo: List[Const], notEqualTo: List[Const], fields: scala.collection.mutable.Map[Symbol, VariableAssignment]) {
        // need to prune since the model now incorporates all super types of a constant (needed for reachability)
        private lazy val uniqueEqualTo = equalTo filterNot (subsumed => equalTo.exists(better => (better ne subsumed) && instanceOfTpImplies(better.tp, subsumed.tp)))
        private lazy val prunedEqualTo = uniqueEqualTo filterNot (subsumed => variable.staticTpCheckable <:< subsumed.tp)
        private lazy val ctor       = (prunedEqualTo match { case List(TypeConst(tp)) => tp case _ => variable.staticTpCheckable }).typeSymbol.primaryConstructor
        private lazy val ctorParams = if (ctor == NoSymbol || ctor.paramss.isEmpty) Nil else ctor.paramss.head
        private lazy val cls        = if (ctor == NoSymbol) NoSymbol else ctor.owner
        private lazy val caseFieldAccs = if (cls == NoSymbol) Nil else cls.caseFieldAccessors


        def allFieldAssignmentsLegal: Boolean =
          (fields.keySet subsetOf caseFieldAccs.toSet) && fields.values.forall(_.allFieldAssignmentsLegal)

        private lazy val nonTrivialNonEqualTo = notEqualTo.filterNot{c => c.isAny }

        // NoExample if the constructor call is ill-typed
        // (thus statically impossible -- can we incorporate this into the formula?)
        // beBrief is used to suppress negative information nested in tuples -- it tends to get too noisy
        def toCounterExample(beBrief: Boolean = false): CounterExample =
          if (!allFieldAssignmentsLegal) NoExample
          else {
            patmatDebug("describing "+ (variable, equalTo, notEqualTo, fields, cls, allFieldAssignmentsLegal))
            val res = prunedEqualTo match {
              // a definite assignment to a value
              case List(eq: ValueConst) if fields.isEmpty => ValueExample(eq)

              // constructor call
              // or we did not gather any information about equality but we have information about the fields
              //  --> typical example is when the scrutinee is a tuple and all the cases first unwrap that tuple and only then test something interesting
              case _ if cls != NoSymbol && !isPrimitiveValueClass(cls) &&
                        (  uniqueEqualTo.nonEmpty
                        || (fields.nonEmpty && prunedEqualTo.isEmpty && notEqualTo.isEmpty)) =>

                def args(brevity: Boolean = beBrief) = {
                  // figure out the constructor arguments from the field assignment
                  val argLen = (caseFieldAccs.length min ctorParams.length)

                  (0 until argLen).map(i => fields.get(caseFieldAccs(i)).map(_.toCounterExample(brevity)) getOrElse WildcardExample).toList
                }

                cls match {
                  case ConsClass               => ListExample(args())
                  case _ if isTupleSymbol(cls) => TupleExample(args(true))
                  case _ => ConstructorExample(cls, args())
                }

              // a definite assignment to a type
              case List(eq) if fields.isEmpty => TypeExample(eq)

              // negative information
              case Nil if nonTrivialNonEqualTo.nonEmpty =>
                // negation tends to get pretty verbose
                if (beBrief) WildcardExample
                else {
                  val eqTo = equalTo.headOption getOrElse TypeConst(variable.staticTpCheckable)
                  NegativeExample(eqTo, nonTrivialNonEqualTo)
                }

              // not a valid counter-example, possibly since we have a definite type but there was a field mismatch
              // TODO: improve reasoning -- in the mean time, a false negative is better than an annoying false positive
              case _ => NoExample
            }
            patmatDebug("described as: "+ res)
            res
          }

        override def toString = toCounterExample().toString
      }

      // slurp in information from other variables
      varAssignment.keys.foreach{ v => if (v != scrutVar) VariableAssignment(v) }

      // this is the variable we want a counter example for
      VariableAssignment(scrutVar).toCounterExample()
    }
  }
}

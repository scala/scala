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

package scala
package reflect
package internal

import java.util.Objects

import scala.collection.mutable
import scala.ref.WeakReference
import mutable.{ListBuffer, LinkedHashSet}
import Flags._
import scala.util.control.ControlThrowable
import scala.annotation.{tailrec, unused}
import util.{ReusableInstance, Statistics}
import util.ThreeValues._
import Variance._
import Depth._
import TypeConstants._

/* A standard type pattern match:
  case ErrorType =>
    // internal: error
  case WildcardType =>
    // internal: unknown
  case BoundedWildcardType(bounds) =>
    // internal: unknown
  case NoType =>
  case NoPrefix =>
  case ThisType(sym) =>
    // sym.this.type
  case SuperType(thistpe, supertpe) =>
    // super references
  case SingleType(pre, sym) =>
    // pre.sym.type
  case ConstantType(value) =>
    // Int(2)
  case TypeRef(pre, sym, args) =>
    // pre.sym[targs]
    // Outer.this.C would be represented as TypeRef(ThisType(Outer), C, List())
  case RefinedType(parents, defs) =>
    // parent1 with ... with parentn { defs }
  case ExistentialType(tparams, result) =>
    // result forSome { tparams }
  case AnnotatedType(annots, tp) =>
    // tp @annots

  // the following are non-value types; you cannot write them down in Scala source.

  case TypeBounds(lo, hi) =>
    // >: lo <: hi
  case ClassInfoType(parents, defs, clazz) =>
    // same as RefinedType except as body of class
  case MethodType(paramtypes, result) =>
    // (paramtypes)result
    // For instance def m(): T is represented as MethodType(List(), T)
  case NullaryMethodType(result) => // eliminated by uncurry
    // an eval-by-name type
    // For instance def m: T is represented as NullaryMethodType(T)
  case PolyType(tparams, result) =>
    // [tparams]result where result is a (Nullary)MethodType or ClassInfoType

  // The remaining types are not used after phase `typer`.
  case OverloadedType(pre, alternatives) =>
    // all alternatives of an overloaded ident
  case AntiPolyType(pre, targs) =>
    // rarely used, disappears when combined with a PolyType
  case TypeVar(inst, constr) =>
    // a type variable
    // Replace occurrences of type parameters with type vars, where
    // inst is the instantiation and constr is a list of bounds.
  case ErasedValueType(clazz, underlying)
    // only used during erasure of derived value classes.
*/

trait Types
  extends api.Types
  with tpe.TypeComparers
  with tpe.TypeToStrings
  with tpe.CommonOwners
  with tpe.GlbLubs
  with tpe.TypeMaps
  with tpe.TypeConstraints
  with tpe.FindMembers
  with util.Collections { self: SymbolTable =>

  import definitions._
  import statistics._

  private[this] var explainSwitch = false
  @unused private final val emptySymbolSet = Set.empty[Symbol]

  @unused private final val breakCycles = settings.breakCycles.value
  /** In case anyone wants to turn on type parameter bounds being used
   *  to seed type constraints.
   */
  private final val sharperSkolems = System.getProperty("scalac.experimental.sharper-skolems") != null

  /** Caching the most recent map has a 75-90% hit rate. */
  private object substTypeMapCache {
    private[this] var cached: SubstTypeMap = new SubstTypeMap(Nil, Nil)

    def apply(from: List[Symbol], to: List[Type]): SubstTypeMap = if (isCompilerUniverse) {
      if ((cached.from ne from) || (cached.to ne to))
        cached = new SubstTypeMap(from, to)
      cached
    } else new SubstTypeMap(from, to)
  }

  /** The current skolemization level, needed for the algorithms
   *  in isSameType, isSubType that do constraint solving under a prefix.
   */
  private[this] var _skolemizationLevel = 0
  def skolemizationLevel = _skolemizationLevel
  def skolemizationLevel_=(value: Int) = _skolemizationLevel = value

  /** A map from lists to compound types that have the given list as parents.
   *  This is used to avoid duplication in the computation of base type sequences and baseClasses.
   *  It makes use of the fact that these two operations depend only on the parents,
   *  not on the refinement.
   */
  private[this] val _intersectionWitness = perRunCaches.newWeakMap[List[Type], WeakReference[Type]]()
  def intersectionWitness = _intersectionWitness

  /** A proxy for a type (identified by field `underlying`) that forwards most
   *  operations to it (for exceptions, see WrappingProxy, which forwards even more operations).
   *  every operation that is overridden for some kind of types should be forwarded.
   */
  trait SimpleTypeProxy extends Type {
    def underlying: Type

    // the following operations + those in RewrappingTypeProxy are all operations
    // in class Type that are overridden in some subclass
    // Important to keep this up-to-date when new operations are added!
    override def isTrivial = underlying.isTrivial
    override def isHigherKinded: Boolean = underlying.isHigherKinded
    override def typeConstructor: Type = underlying.typeConstructor
    override def isError = underlying.isError
    override def isErroneous = underlying.isErroneous
    override def paramSectionCount = underlying.paramSectionCount
    override def paramss = underlying.paramss
    override def params = underlying.params
    override def paramTypes = underlying.paramTypes
    override def termSymbol = underlying.termSymbol
    override def termSymbolDirect = underlying.termSymbolDirect
    override def typeParams = underlying.typeParams
    override def typeSymbol = underlying.typeSymbol
    override def typeSymbolDirect = underlying.typeSymbolDirect
    override def widen = underlying.widen
    override def typeOfThis = underlying.typeOfThis
    override def bounds = underlying.bounds
    override def lowerBound = underlying.lowerBound
    override def upperBound = underlying.upperBound
    override def parents = underlying.parents
    override def prefix = underlying.prefix
    override def prefixDirect = underlying.prefixDirect
    override def decls = underlying.decls
    override def baseType(clazz: Symbol) = underlying.baseType(clazz)
    override def baseTypeSeq = underlying.baseTypeSeq
    override def baseTypeSeqDepth = underlying.baseTypeSeqDepth
    override def baseClasses = underlying.baseClasses
  }

  /** A proxy for a type (identified by field `underlying`) that forwards most
   *  operations to it. Every operation that is overridden for some kind of types is
   *  forwarded here. Some operations are rewrapped again.
   */
  trait RewrappingTypeProxy extends SimpleTypeProxy {
    protected def maybeRewrap(newtp: Type) = if (newtp eq underlying) this else rewrap(newtp)
    protected def rewrap(newtp: Type): Type

    // the following are all operations in class Type that are overridden in some subclass
    // Important to keep this up-to-date when new operations are added!
    override def widen = maybeRewrap(underlying.widen)
    override def narrow = underlying.narrow
    override def deconst = maybeRewrap(underlying.deconst)
    override def resultType = maybeRewrap(underlying.resultType)
    override def resultType(actuals: List[Type]) = maybeRewrap(underlying.resultType(actuals))
    override def paramSectionCount = 0
    override def paramss: List[List[Symbol]] = List()
    override def params: List[Symbol] = List()
    override def paramTypes: List[Type] = List()
    override def typeArgs = underlying.typeArgs
    override def instantiateTypeParams(formals: List[Symbol], actuals: List[Type]) = underlying.instantiateTypeParams(formals, actuals)
    override def skolemizeExistential(owner: Symbol, origin: AnyRef) = underlying.skolemizeExistential(owner, origin)
    override def normalize = maybeRewrap(underlying.normalize)
    override def etaExpand = maybeRewrap(underlying.etaExpand)
    override def dealias = maybeRewrap(underlying.dealias)
    override def cloneInfo(owner: Symbol) = maybeRewrap(underlying.cloneInfo(owner))
    override def atOwner(owner: Symbol) = maybeRewrap(underlying.atOwner(owner))
    override def prefixString = underlying.prefixString
    override def isComplete = underlying.isComplete
    override def complete(sym: Symbol) = underlying.complete(sym)
    override def load(sym: Symbol): Unit = { underlying.load(sym) }
    override def withAnnotations(annots: List[AnnotationInfo]) = maybeRewrap(underlying.withAnnotations(annots))
    override def withoutAnnotations = maybeRewrap(underlying.withoutAnnotations)
  }

  case object UnmappableTree extends TermTree {
    override def toString = "<unmappable>"
    super.setType(NoType)
    override def tpe_=(t: Type) = if (t != NoType) {
      throw new UnsupportedOperationException("tpe_=("+t+") inapplicable for <empty>")
    }
  }

  abstract class TypeApiImpl extends TypeApi { this: Type =>
    def declaration(name: Name): Symbol = decl(name)
    def declarations = decls
    def typeArguments = typeArgs
    def erasure = this match {
      case ConstantType(value) => widen.erasure
      case _ =>
        var result: Type = transformedType(this)
        result = result.normalize match { // necessary to deal with erasures of HK types, typeConstructor won't work
          case PolyType(undets, underlying) => existentialAbstraction(undets, underlying) // we don't want undets in the result
          case _ => result
        }
        // erasure screws up all ThisTypes for modules into PackageTypeRefs
        // we need to unscrew them, or certain typechecks will fail mysteriously
        // https://groups.google.com/group/scala-internals/browse_thread/thread/6d3277ae21b6d581
        result = result.map(tpe => tpe match {
          case tpe: PackageTypeRef => ThisType(tpe.sym)
          case _ => tpe
        })
        result
    }
    def substituteSymbols(from: List[Symbol], to: List[Symbol]): Type = substSym(from, to)
    def substituteTypes(from: List[Symbol], to: List[Type]): Type = subst(from, to)

    // the only thingies that we want to splice are: 1) type parameters, 2) abstract type members
    // the thingies that we don't want to splice are: 1) concrete types (obviously), 2) existential skolems
    def isSpliceable = {
      this.isInstanceOf[TypeRef] && typeSymbol.isAbstractType && !typeSymbol.isExistential
    }

    def companion = {
      val sym = typeSymbolDirect
      if (sym.isModule && !sym.hasPackageFlag) sym.companionSymbol.tpe
      else if (sym.isModuleClass && !sym.isPackageClass) sym.sourceModule.companionSymbol.tpe
      else if (sym.isClass && !sym.isModuleClass && !sym.isPackageClass) sym.companionSymbol.info
      else NoType
    }

    def paramLists: List[List[Symbol]] = paramss
  }

  /** The base class for all types */
  abstract class Type extends TypeApiImpl with Annotatable[Type] {
    /** Types for which asSeenFrom always is the identity, no matter what
     *  prefix or owner.
     */
    def isTrivial: Boolean = false

    /** Is this type higher-kinded, i.e., is it a type constructor \@M */
    def isHigherKinded: Boolean = false
    def takesTypeArgs: Boolean = this.isHigherKinded

    /** Does this type denote a stable reference (i.e. singleton type)? */
    final def isStable: Boolean = definitions isStable this

    /** Is this type dangerous (i.e. it might contain conflicting
     *  type information when empty, so that it can be constructed
     *  so that type unsoundness results.) A dangerous type has an underlying
     *  type of the form T_1 with T_n { decls }, where one of the
     *  T_i (i > 1) is an abstract type.
     */
    final def isVolatile: Boolean = definitions isVolatile this

    /** Is this type a structural refinement type (it ''refines'' members that have not been inherited) */
    def isStructuralRefinement: Boolean = false

    /** Does this type depend immediately on an enclosing method parameter?
      * I.e., is it a singleton type whose termSymbol refers to an argument of the symbol's owner (which is a method)?
      */
    def isImmediatelyDependent: Boolean = false

    /** Is this type a dependent method type? */
    def isDependentMethodType: Boolean = false

    /** True for WildcardType or BoundedWildcardType. */
    def isWildcard = false

    /** Is this type produced as a repair for an error? */
    def isError: Boolean = typeSymbol.isError || termSymbol.isError

    /** Is this type produced as a repair for an error? */
    def isErroneous: Boolean = ErroneousCollector.collect(this)

    /** Can this type only be subtyped by bottom types?
     *  This is assessed to be the case if the class is final,
     *  and all type parameters (if any) are invariant.
     */
    def isFinalType: Boolean = typeSymbol.hasOnlyBottomSubclasses && prefix.isStable

    /** Is this type completed (i.e. not a lazy type)? */
    def isComplete: Boolean = true

    /** Should this be printed as an infix type (@showAsInfix class &&[T, U])? */
    def isShowAsInfixType: Boolean = false

    /** If this is a lazy type, assign a new type to `sym`. */
    def complete(sym: Symbol): Unit = ()

    /** If this is a lazy type corresponding to a subclass add it to its
     *  parents children
     */
    def forceDirectSuperclasses(): Unit = ()

    /** The term symbol associated with the type
      * Note that the symbol of the normalized type is returned (@see normalize)
      */
    def termSymbol: Symbol = NoSymbol

    /** The type symbol associated with the type
      * Note that the symbol of the normalized type is returned (@see normalize)
      * A type's typeSymbol should if possible not be inspected directly, due to
      * the likelihood that what is true for tp.typeSymbol is not true for
      * tp.sym, due to normalization.
      */
    def typeSymbol: Symbol = NoSymbol

    /** The term symbol ''directly'' associated with the type.
     */
    def termSymbolDirect: Symbol = termSymbol

    /** The type symbol ''directly'' associated with the type.
     *  In other words, no normalization is performed: if this is an alias type,
     *  the symbol returned is that of the alias, not the underlying type.
     */
    def typeSymbolDirect: Symbol = typeSymbol

    /** The base type underlying a type proxy, identity on all other types */
    def underlying: Type = this

    /** Widen from singleton type to its underlying non-singleton
     *  base type by applying one or more `underlying` dereferences,
     *  identity for all other types.
     *
     *  class Outer { class C ; val x: C }
     *  val o: Outer
     *  <o.x.type>.widen = o.C
     */
    def widen: Type = this

    /** Map a constant type or not-null-type to its underlying base type,
     *  identity for all other types.
     */
    def deconst: Type = this

    /** The type of `this` of a class type or reference type. */
    def typeOfThis: Type = typeSymbol.typeOfThis

    /** Map to a singleton type which is a subtype of this type.
     *  The fallback implemented here gives
     *    T.narrow  = T' forSome { type T' <: T with Singleton }
     *  Overridden where we know more about where types come from.
     */
    /*
    Note: this implementation of narrow is theoretically superior to the one
    in use below, but imposed a significant performance penalty.  It was in trunk
    from svn r24960 through r25080.
    */
    /*
    def narrow: Type =
      if (phase.erasedTypes) this
      else commonOwner(this) freshExistential ".type" setInfo singletonBounds(this) tpe
    */

    /** Map to a singleton type which is a subtype of this type.
     *  The fallback implemented here gives:
     *  {{{
     *    T.narrow  =  (T {}).this.type
     *  }}}
     *  Overridden where we know more about where types come from.
     */
    def narrow: Type =
      if (phase.erasedTypes) this
      else {
        val cowner = commonOwner(this)
        refinedType(this :: Nil, cowner, EmptyScope, cowner.pos).narrow
      }

    /** For a TypeBounds type, itself;
     *  for a reference denoting an abstract type, its bounds,
     *  for all other types, a TypeBounds type all of whose bounds are this type.
     */
    def bounds: TypeBounds = TypeBounds(lowerBound, upperBound)
    def lowerBound: Type = this
    def upperBound: Type = this

    /** For a class or intersection type, its parents.
     *  For a TypeBounds type, the parents of its hi bound.
     *  inherited by typerefs, singleton types, and refinement types,
     *  The empty list for all other types */
    def parents: List[Type] = List()

    /** For a class with !isEmpty parents, the first parent.
     *  Otherwise some specific fixed top type.
     */
    def firstParent: Type = if (!parents.isEmpty) parents.head else ObjectTpe

    /** For a typeref or single-type, the prefix of the normalized type (@see normalize).
     *  NoType for all other types. */
    def prefix: Type = NoType

    /** The prefix ''directly'' associated with the type.
     *  In other words, no normalization is performed: if this is an alias type,
     *  the prefix returned is that of the alias, not the underlying type.
     */
    def prefixDirect: Type = prefix

    /** A chain of all typeref or singletype prefixes of this type, longest first.
     *  (Only used from safeToString.)
     */
    def prefixChain: List[Type] = this match {
      case TypeRef(pre, _, _) => pre :: pre.prefixChain
      case SingleType(pre, _) => pre :: pre.prefixChain
      case _ => List()
    }

    /** This type, without its type arguments \@M */
    def typeConstructor: Type = this

    /** For a typeref, its arguments. The empty list for all other types */
    def typeArgs: List[Type] = List()

    /** A list of placeholder types derived from the type parameters.
     *  Used by RefinedType and TypeRef.
     */
    protected def dummyArgs: List[Type] = typeParams map (_.typeConstructor)

    /** For a (nullary) method or poly type, its direct result type,
     *  the type itself for all other types. */
    def resultType: Type = this

    def resultType(actuals: List[Type]) = this

    /** Only used for dependent method types. */
    def resultApprox: Type = ApproximateDependentMap(resultType)

    /** For a curried/nullary method or poly type its non-method result type,
     *  the type itself for all other types */
    final def finalResultType: Type = definitions finalResultType this

    /** For a method type, the number of its value parameter sections,
     *  0 for all other types */
    def paramSectionCount: Int = 0

    /** For a method or poly type, a list of its value parameter sections,
     *  the empty list for all other types */
    def paramss: List[List[Symbol]] = List()

    /** For a method or poly type, its first value parameter section,
     *  the empty list for all other types */
    def params: List[Symbol] = List()

    /** For a method or poly type, the types of its first value parameter section,
     *  the empty list for all other types */
    def paramTypes: List[Type] = List()

    /** For a (potentially wrapped) poly type, its type parameters,
     *  the empty list for all other types */
    def typeParams: List[Symbol] = List()

    /** Replace formal type parameter symbols with actual type arguments. ErrorType on arity mismatch.
     *
     * Amounts to substitution except for higher-kinded types. (See overridden method in TypeRef) -- \@M
     */
    def instantiateTypeParams(formals: List[Symbol], actuals: List[Type]): Type =
      if (sameLength(formals, actuals)) this.subst(formals, actuals) else ErrorType

    /** If this type is an existential, turn all existentially bound variables to type skolems.
     *  @param  owner    The owner of the created type skolems
     *  @param  origin   The tree whose type was an existential for which the skolem was created.
     */
    def skolemizeExistential(owner: Symbol, origin: AnyRef): Type = this

    /** A simple version of skolemizeExistential for situations where
     *  owner or unpack location do not matter (typically used in subtype tests)
     */
    def skolemizeExistential: Type = skolemizeExistential(NoSymbol, null)

    /** Reduce to beta eta-long normal form.
     *  Expands type aliases and converts higher-kinded TypeRefs to PolyTypes.
     *  Functions on types are also implemented as PolyTypes.
     *
     *  Example: (in the below, `<List>` is the type constructor of List)
     *    TypeRef(pre, `<List>`, List()) is replaced by
     *    PolyType(X, TypeRef(pre, `<List>`, List(X)))
     *
     *  Discussion: normalize is NOT usually what you want to be calling.
     *  The (very real) danger with normalize is that it will force types
     *  which would not otherwise have been forced, leading to mysterious
     *  behavioral differences, cycles, and other elements of mysteries.
     *  Under most conditions the method you should be calling is `dealiasWiden`
     *  (see that method for more info.)
     *
     *  Here are a few of the side-effect-trail-leaving methods called
     *  by various implementations of normalize:
     *
     *   - sym.info
     *   - tpe.etaExpand
     *   - tpe.betaReduce
     *   - tpe.memberType
     *   - sym.nextOverriddenSymbol
     *   - constraint.inst
     *
     *  If you've been around the compiler a while that list must fill
     *  your heart with fear.
     */
    def normalize = this // @MAT

    def etaExpand = this

    /** Expands type aliases. */
    def dealias = this

    /** Repeatedly apply widen and dealias until they have no effect.
     *  This compensates for the fact that type aliases can hide beneath
     *  singleton types and singleton types can hide inside type aliases.
     *  !!! - and yet it is still inadequate, because aliases and singletons
     *  might lurk in the upper bounds of an abstract type. See scala/bug#7051.
     */
    @tailrec
    final def dealiasWiden: Type = {
      val widened = widen
      if (this ne widened) widened.dealiasWiden
      else {
        val dealiased = dealias
        if (this ne dealiased) dealiased.dealiasWiden
        else this
      }
    }

    /** All the types encountered in the course of dealiasing/widening,
     *  including each intermediate beta reduction step (whereas calling
     *  dealias applies as many as possible.)
     */
    def dealiasWidenChain: List[Type] = this :: {
      val widened = widen
      if (this ne widened) widened.dealiasWidenChain
      else {
        val betaReduced = betaReduce
        if (this ne betaReduced) betaReduced.dealiasWidenChain
        else Nil
      }
    }

    /** Performs a single step of beta-reduction on types.
     *  Given:
     *
     *    type C[T] = B[T]
     *    type B[T] = A
     *    class A
     *
     *  The following will happen after `betaReduce` is invoked:
     *    TypeRef(pre, <C>, List(Int)) is replaced by
     *    TypeRef(pre, <B>, List(Int))
     *
     *  Unlike `dealias`, which recursively applies beta reduction, until it's stuck,
     *  `betaReduce` performs exactly one step and then returns.
     */
    def betaReduce: Type = this

    /** For a classtype or refined type, its defined or declared members;
     *  inherited by subtypes and typerefs.
     *  The empty scope for all other types.
     */
    def decls: Scope = EmptyScope

    /** The defined or declared members with name `name` in this type;
     *  an OverloadedSymbol if several exist, NoSymbol if none exist.
     *  Alternatives of overloaded symbol appear in the order they are declared.
     */
    def decl(name: Name): Symbol = findDecl(name, 0)

    /** A list of all non-private members defined or declared in this type. */
    def nonPrivateDecls: List[Symbol] = decls.filterNot(_.isPrivate).toList

    /** The non-private defined or declared members with name `name` in this type;
     *  an OverloadedSymbol if several exist, NoSymbol if none exist.
     *  Alternatives of overloaded symbol appear in the order they are declared.
     */
    def nonPrivateDecl(name: Name): Symbol = findDecl(name, PRIVATE)

    /** A list of all members of this type (defined or inherited)
     *  Members appear in linearization order of their owners.
     *  Members with the same owner appear in reverse order of their declarations.
     */
    def members: Scope = membersBasedOnFlags(0, 0)

    /** A list of all non-private members of this type (defined or inherited) */
    def nonPrivateMembers: Scope = membersBasedOnFlags(BridgeAndPrivateFlags, 0)

    /** A list of all non-private members of this type  (defined or inherited),
     *  admitting members with given flags `admit`
     */
    def nonPrivateMembersAdmitting(admit: Long): Scope = membersBasedOnFlags(BridgeAndPrivateFlags & ~admit, 0)

    /** A list of all implicit symbols of this type  (defined or inherited) */
    def implicitMembers: Scope = {
      typeSymbolDirect match {
        case sym: ModuleClassSymbol => sym.implicitMembers
        case _ => membersBasedOnFlags(BridgeFlags, IMPLICIT)
      }
    }

    /** A list of all deferred symbols of this type  (defined or inherited) */
    def deferredMembers: Scope = membersBasedOnFlags(BridgeFlags, DEFERRED)

    /** The member with given name,
     *  an OverloadedSymbol if several exist, NoSymbol if none exist */
    def member(name: Name): Symbol =
      memberBasedOnName(name, BridgeFlags)

    /** The non-private member with given name,
     *  an OverloadedSymbol if several exist, NoSymbol if none exist.
     *  Bridges are excluded from the result
     */
    def nonPrivateMember(name: Name): Symbol =
      memberBasedOnName(name, BridgeAndPrivateFlags)
    def hasNonPrivateMember(name: Name): Boolean = {
      new HasMember(this, name, BridgeAndPrivateFlags, 0L).apply()
    }

    def packageObject: Symbol = member(nme.PACKAGE)

    /** The non-private member with given name, admitting members with given flags `admit`.
     *  "Admitting" refers to the fact that members with a PRIVATE, BRIDGE, or VBRIDGE
     *  flag are usually excluded from findMember results, but supplying any of those flags
     *  to this method disables that exclusion.
     *
     *  An OverloadedSymbol if several exist, NoSymbol if none exists.
     */
    def nonPrivateMemberAdmitting(name: Name, admit: Long): Symbol =
      memberBasedOnName(name, BridgeAndPrivateFlags & ~admit)

    /** The non-local member with given name,
     *  an OverloadedSymbol if several exist, NoSymbol if none exist */
    def nonLocalMember(name: Name): Symbol =
      memberBasedOnName(name, BridgeFlags | LOCAL)

    /** Members excluding and requiring the given flags.
     *  Note: unfortunately it doesn't work to exclude DEFERRED this way.
     */
    def membersBasedOnFlags(excludedFlags: Long, requiredFlags: Long): Scope =
      findMembers(excludedFlags, requiredFlags)

    def memberBasedOnName(name: Name, excludedFlags: Long): Symbol =
      findMember(name, excludedFlags, 0, stableOnly = false)

    /** The least type instance of given class which is a supertype
     *  of this type.  Example:
     *    class D[T]
     *    class C extends p.D[Int]
     *    ThisType(C).baseType(D) = p.D[Int]
     */
    def baseType(clazz: Symbol): Type = NoType

    /** This type as seen from prefix `pre` and class `clazz`. This means:
     *  Replace all thistypes of `clazz` or one of its subclasses
     *  by `pre` and instantiate all parameters by arguments of `pre`.
     *  Proceed analogously for thistypes referring to outer classes.
     *
     *  Example:
     *    class D[T] { def m: T }
     *    class C extends p.D[Int]
     *    T.asSeenFrom(ThisType(C), D)  (where D is owner of m)
     *      = Int
     */
    def asSeenFrom(pre: Type, clazz: Symbol): Type = {
      val start = if (settings.areStatisticsEnabled) statistics.pushTimer(typeOpsStack, asSeenFromNanos)  else null
      try {
        val trivial = (
             this.isTrivial
          || phase.erasedTypes && pre.typeSymbol != ArrayClass
          || skipPrefixOf(pre, clazz)
        )
        if (trivial) this
        else {
          val m     = new AsSeenFromMap(pre.normalize, clazz)
          val tp    = m(this)
          val tp1   = existentialAbstraction(m.capturedParams, tp)

          if (m.capturedSkolems.isEmpty) tp1
          else deriveType(m.capturedSkolems, _.cloneSymbol setFlag CAPTURED)(tp1)
        }
      } finally if (settings.areStatisticsEnabled) statistics.popTimer(typeOpsStack, start)
    }

    /** The info of `sym`, seen as a member of this type.
     *
     *  Example:
     *  {{{
     *    class D[T] { def m: T }
     *    class C extends p.D[Int]
     *    ThisType(C).memberType(m) = Int
     *  }}}
     */
    def memberInfo(sym: Symbol): Type = {
//      assert(sym ne NoSymbol, this)
      sym.info.asSeenFrom(this, sym.owner)
    }

    /** The type of `sym`, seen as a member of this type. */
    def memberType(sym: Symbol): Type = sym match {
      case meth: MethodSymbol =>
        meth.typeAsMemberOf(this)
      case _ =>
        computeMemberType(sym)
    }

    def computeMemberType(sym: Symbol): Type = sym.tpeHK match {
      case OverloadedType(_, alts) =>
        OverloadedType(this, alts)
      case tp =>
        if (sym eq NoSymbol) NoType else tp.asSeenFrom(this, sym.owner)
    }

    /** Substitute types `to` for occurrences of references to
     *  symbols `from` in this type.
     */
    def subst(from: List[Symbol], to: List[Type]): Type =
      if (from.isEmpty) this else substTypeMapCache(from, to)(this)

    /** Substitute symbols `to` for occurrences of symbols `from` in this type.
     *
     * !!! NOTE !!!: If you need to do a substThis and a substSym, the substThis has to come
     * first, as otherwise symbols will immediately get rebound in typeRef to the old
     * symbol.
     */
    def substSym(from: List[Symbol], to: List[Symbol]): Type =
      if ((from eq to) || from.isEmpty) this
      else SubstSymMap(from, to).apply(this)

    /** Substitute all occurrences of `ThisType(from)` in this type by `to`.
     *
     * !!! NOTE !!!: If you need to do a substThis and a substSym, the substThis has to come
     * first, as otherwise symbols will immediately get rebound in typeRef to the old
     * symbol.
     */
    def substThis(from: Symbol, to: Type): Type =
      new SubstThisMap(from, to) apply this
    def substThis(from: Symbol, to: Symbol): Type =
      substThis(from, to.thisType)

    /** Performs both substThis and substSym, in that order.
     *
     * [JZ] Reverted `SubstThisAndSymMap` from 334872, which was not the same as
     * `substThis(from, to).substSym(symsFrom, symsTo)`.
     *
     * `SubstThisAndSymMap` performs a breadth-first map over this type, which meant that
     * symbol substitution occurred before `ThisType` substitution. Consequently, in substitution
     * of a `SingleType(ThisType(from), sym)`, symbols were rebound to `from` rather than `to`.
     */
    def substThisAndSym(from: Symbol, to: Type, symsFrom: List[Symbol], symsTo: List[Symbol]): Type =
      if (symsFrom eq symsTo) substThis(from, to)
      else substThis(from, to).substSym(symsFrom, symsTo)

    /** Returns all parts of this type which satisfy predicate `p` */
    def withFilter(p: Type => Boolean) = new FilterMapForeach(p)

    class FilterMapForeach(p: Type => Boolean) extends FilterTypeCollector(p){
      def foreach[U](f: Type => U): Unit = this.collect(Type.this).foreach(f)
      def map[T](f: Type => T): List[T]  = this.collect(Type.this).map(f)
    }

    @inline final def orElse(alt: => Type): Type = if (this ne NoType) this else alt

    /** Returns optionally first type (in a preorder traversal) which satisfies predicate `p`,
     *  or None if none exists.
     */
    def find(p: Type => Boolean): Option[Type] = new FindTypeCollector(p).collect(this)

    /** Apply `f` to each part of this type */
    def foreach(f: Type => Unit): Unit = { new ForEachTypeTraverser(f).traverse(this) }

    /** Apply `pf` to each part of this type on which the function is defined */
    def collect[T](pf: PartialFunction[Type, T]): List[T] = new CollectTypeCollector(pf).collect(this)

    /** Apply `f` to each part of this type; children get mapped before their parents */
    def map(f: Type => Type): Type = new TypeMap {
      def apply(x: Type) = f(x.mapOver(this))
    } apply this

    /** Is there part of this type which satisfies predicate `p`? */
    def exists(p: Type => Boolean): Boolean = !find(p).isEmpty

    /** Does this type contain a reference to this symbol? */
    def contains(sym: Symbol): Boolean = new ContainsCollector(sym).collect(this)

    /** Is this type a subtype of that type? */
    def <:<(that: Type): Boolean = {
      if (settings.areStatisticsEnabled) stat_<:<(that)
      else {
        (this eq that) ||
        (if (explainSwitch) explain("<:", isSubType(_: Type, _: Type), this, that)
         else isSubType(this, that))
      }
    }

    /** Is this type a subtype of that type in a pattern context?
     *  Dummy type arguments on the right hand side are replaced with
     *  fresh existentials, except for Arrays.
     *
     *  See bug1434.scala for an example of code which would fail
     *  if only a <:< test were applied.
     */
    def matchesPattern(that: Type): Boolean = (this <:< that) || (that match {
      case ArrayTypeRef(elem2) if elem2.typeConstructor.isHigherKinded =>
        this match {
          case ArrayTypeRef(elem1) => elem1 matchesPattern elem2
          case _                   => false
        }
      case TypeRef(_, sym, args) =>
        val that1 = existentialAbstraction(args.map(_.typeSymbol), that)
        (that ne that1) && (this <:< that1) && {
          debuglog(s"$this.matchesPattern($that) depended on discarding args and testing <:< $that1")
          true
        }
      case _ =>
        false
    })

    def stat_<:<(that: Type): Boolean = {
      if (settings.areStatisticsEnabled) statistics.incCounter(subtypeCount)
      val start = if (settings.areStatisticsEnabled) statistics.pushTimer(typeOpsStack, subtypeNanos) else null
      val result =
        (this eq that) ||
        (if (explainSwitch) explain("<:", isSubType(_: Type, _: Type), this, that)
         else isSubType(this, that))
      if (settings.areStatisticsEnabled) statistics.popTimer(typeOpsStack, start)
      result
    }

    /** Is this type a weak subtype of that type? True also for numeric types, i.e. Int weak_<:< Long.
     */
    def weak_<:<(that: Type): Boolean = {
      if (settings.areStatisticsEnabled) statistics.incCounter(subtypeCount)
      val start = if (settings.areStatisticsEnabled) statistics.pushTimer(typeOpsStack, subtypeNanos) else null
      val result =
        ((this eq that) ||
         (if (explainSwitch) explain("weak_<:", isWeakSubType, this, that)
          else isWeakSubType(this, that)))
      if (settings.areStatisticsEnabled) statistics.popTimer(typeOpsStack, start)
      result
    }

    /** Is this type equivalent to that type? */
    def =:=(that: Type): Boolean = (
      (this eq that) ||
      (if (explainSwitch) explain("=", isSameType, this, that)
       else isSameType(this, that))
    )

    /** Is this type close enough to that type so that members
     *  with the two type would override each other?
     *  This means:
     *    - Either both types are polytypes with the same number of
     *      type parameters and their result types match after renaming
     *      corresponding type parameters
     *    - Or both types are (nullary) method types with equivalent type parameter types
     *      and matching result types
     *    - Or both types are equivalent
     *    - Or phase.erasedTypes is false and both types are neither method nor
     *      poly types.
     */
    def matches(that: Type): Boolean = matchesType(this, that, !phase.erasedTypes)

    /** Same as matches, except that non-method types are always assumed to match. */
    def looselyMatches(that: Type): Boolean = matchesType(this, that, alwaysMatchSimple = true)

    /** The base type sequence of T is the smallest set of (potentially existentially quantified)
      * class types Ti, so that for each supertype T' (T <:< T'),
      * there is a Ti so that T <:< Ti <:< T'.
      *
      * This is also known as the upward closed set of the partially ordered set of
      * class types under Symbol#isLess (a refinement of Symbol#isSubClass).
      *
      * See "Base Types and Member Definitions" in spec/03-types.md.
      */
    def baseTypeSeq: BaseTypeSeq = baseTypeSingletonSeq(this)

    /** The maximum depth (@see typeDepth)
     *  of each type in the BaseTypeSeq of this type except the first.
     */
    def baseTypeSeqDepth: Depth = Depth(1)

    /** The list of all baseclasses of this type (including its own typeSymbol)
     *  in linearization order, starting with the class itself and ending
     *  in class Any.
     */
    def baseClasses: List[Symbol] = List()

    /**
     *  @param sym the class symbol
     *  @return    the index of given class symbol in the BaseTypeSeq of this type,
     *             or -1 if no base type with given class symbol exists.
     */
    def baseTypeIndex(sym: Symbol): Int = baseTypeSeq.baseTypeIndex(sym)

    /** If this is a ExistentialType, PolyType or MethodType, a copy with cloned type / value parameters
     *  owned by `owner`. Identity for all other types.
     */
    def cloneInfo(owner: Symbol) = this

    /** Make sure this type is correct as the info of given owner; clone it if not. */
    def atOwner(owner: Symbol) = this

    protected def objectPrefix = "object "
    protected def packagePrefix = "package "
    def trimPrefix(str: String) = str stripPrefix objectPrefix stripPrefix packagePrefix

    /** The string representation of this type used as a prefix */
    def prefixString = {
      val pre = trimPrefix(toString)
      if (isShowAsInfixType) s"($pre)#" else pre + "#"
    }

   /** Convert toString avoiding infinite recursions by cutting off
     *  after `maxToStringRecursions` recursion levels. Uses `safeToString`
     *  to produce a string on each level.
     */
    override final def toString: String = {
      // see comments to internal#Symbol.typeSignature for an explanation why this initializes
      if (!isCompilerUniverse) fullyInitializeType(this)
      typeToString(this)
    }

    /** Method to be implemented in subclasses.
     *  Converts this type to a string in calling toString for its parts.
     */
    def safeToString: String = super.toString

    /** The string representation of this type, with singletypes explained. */
    def toLongString = {
      val str = toString
      if (str == "type") widen.toString
      else if ((str endsWith ".type") && !typeSymbol.isModuleClass)
        widen match {
          case RefinedType(_, _)                      => "" + widen
          case _                                      =>
            if (widen.toString.trim == "") str
            else s"$str (with underlying type $widen)"
        }
      else str
    }

    /** The string representation of this type when the direct object in a sentence.
     *  Normally this is no different from the regular representation, but modules
     *  read better as "object Foo" here and "Foo.type" the rest of the time.
     */
    def directObjectString = safeToString

    def nameAndArgsString = typeSymbol.name.toString

    final def isAny: Boolean = typeSymbolDirect eq AnyClass
    final def isNothing: Boolean = typeSymbolDirect eq NothingClass

    /** A test whether a type contains any unification type variables.
     *  Overridden with custom logic except where trivially true.
     */
    def isGround: Boolean = this match {
      case ThisType(_) | NoPrefix | WildcardType | NoType | ErrorType | ConstantType(_) =>
        true
      case _ =>
        typeVarToOriginMap(this) eq this
    }

    /** If this is a symbol loader type, load and assign a new type to `sym`. */
    def load(sym: Symbol): Unit = {}

    private def findDecl(name: Name, excludedFlags: Long): Symbol = {
      var alts: List[Symbol] = List()
      var sym: Symbol = NoSymbol
      var e: ScopeEntry = decls.lookupEntry(name)
      while (e ne null) {
        if (!e.sym.hasFlag(excludedFlags)) {
          if (sym == NoSymbol) sym = e.sym
          else {
            if (alts.isEmpty) alts = sym :: Nil
            alts = e.sym :: alts
          }
        }
        e = decls.lookupNextEntry(e)
      }
      if (alts.isEmpty) sym
      else (baseClasses.head.newOverloaded(this, alts))
    }

    /** Find all members meeting the flag requirements.
     *
     * If you require a DEFERRED member, you will get it if it exists -- even if there's an overriding concrete member.
     * If you exclude DEFERRED members, or don't specify any requirements,
     *    you won't get deferred members (whether they have an overriding concrete member or not)
     *
     * Thus, findMember requiring DEFERRED flags yields deferred members,
     * while `findMember(excludedFlags = 0, requiredFlags = 0).filter(_.isDeferred)` may not (if there's a corresponding concrete member)
     *
     * Requirements take precedence over exclusions, so requiring and excluding DEFERRED will yield a DEFERRED member (if there is one).
     *
     */
    def findMembers(excludedFlags: Long, requiredFlags: Long): Scope = {
      def findMembersInternal = new FindMembers(this, excludedFlags, requiredFlags).apply()
      if (this.isGround) findMembersInternal
      else suspendingTypeVars(typeVarsInTypeRev(this))(findMembersInternal)
    }

    /**
     *  Find member(s) in this type. If several members matching criteria are found, they are
     *  returned in an OverloadedSymbol
     *
     *  @param name           The member's name
     *  @param excludedFlags  Returned members do not have these flags
     *  @param requiredFlags  Returned members do have these flags
     *  @param stableOnly     If set, return only members that are types or stable values
     */
    def findMember(name: Name, excludedFlags: Long, requiredFlags: Long, stableOnly: Boolean): Symbol = {
      def findMemberInternal = findMemberInstance.using { findMember =>
        findMember.init(this, name, excludedFlags, requiredFlags, stableOnly)
        findMember()
      }

      if (this.isGround) findMemberInternal
      else suspendingTypeVars(typeVarsInTypeRev(this))(findMemberInternal)
    }

    /** The (existential or otherwise) skolems and existentially quantified variables which are free in this type */
    def skolemsExceptMethodTypeParams: List[Symbol] = {
      var boundSyms: List[Symbol] = List()
      var skolems: List[Symbol] = List()
      for (t <- this) {
        t match {
          case ExistentialType(quantified, qtpe) =>
            boundSyms = boundSyms ::: quantified
          case TypeRef(_, sym, _) =>
            if ((sym.isExistentialSkolem || sym.isGADTSkolem) && // treat GADT skolems like existential skolems
                !((boundSyms contains sym) || (skolems contains sym)))
              skolems = sym :: skolems
          case _ =>
        }
      }
      skolems
    }

    // Implementation of Annotatable for all types but AnnotatedType, which
    // overrides these.
    def annotations: List[AnnotationInfo] = Nil
    def withoutAnnotations: Type = this
    def filterAnnotations(p: AnnotationInfo => Boolean): Type = this
    def setAnnotations(annots: List[AnnotationInfo]): Type  = annotatedType(annots, this)
    def withAnnotations(annots: List[AnnotationInfo]): Type = annotatedType(annots, this)
    def withAnnotation(anno: AnnotationInfo): Type = withAnnotations(List(anno))

    /** The kind of this type; used for debugging */
    def kind: String = "unknown type of class "+getClass()

    def mapOver(map: TypeMap): Type = this
    def foldOver(folder: TypeFolder): Unit = {}
  }

// Subclasses ------------------------------------------------------------

  /**
   *  A type that can be passed to unique(..) and be stored in the uniques map.
   */
  abstract class UniqueType extends Type with Product {
    final override val hashCode = computeHashCode
    protected def computeHashCode = scala.runtime.ScalaRunTime._hashCode(this)
  }

 /** A base class for types that defer some operations
   *  to their immediate supertype.
   */
  abstract class SubType extends UniqueType {
    def supertype: Type
    override def parents: List[Type] = supertype.parents
    override def decls: Scope = supertype.decls
    override def baseType(clazz: Symbol): Type = supertype.baseType(clazz)
    override def baseTypeSeq: BaseTypeSeq = supertype.baseTypeSeq
    override def baseTypeSeqDepth: Depth = supertype.baseTypeSeqDepth
    override def baseClasses: List[Symbol] = supertype.baseClasses
 }

  /** A base class for types that represent a single value
   *  (single-types and this-types).
   */
  abstract class SingletonType extends SubType with SimpleTypeProxy with SingletonTypeApi {
    def supertype = underlying
    override def isTrivial = false
// Spec: "The base types of a singleton type `$p$.type` are the base types of the type of $p$."
//    override def baseTypeSeq: BaseTypeSeq = underlying.baseTypeSeq
    override def isHigherKinded = false // singleton type classifies objects, thus must be kind *
    // Avoid printing Predef.type and scala.package.type as "type",
    // since in all other cases we omit those prefixes. Do not skipPackageObject.
    override def safeToString: String =
      termSymbol match {
        case s if s.isOmittablePrefix => s"${if (s.isPackageObjectOrClass || s.isJavaDefined) s.fullNameString else s.nameString}.type"
        case _ => s"${prefixString}type"
      }
  }

  /** An object representing an erroneous type */
  case object ErrorType extends Type {
    // todo see whether we can do without
    override def isError: Boolean = true
    override def decls: Scope = new ErrorScope(NoSymbol)
    override def findMember(name: Name, excludedFlags: Long, requiredFlags: Long, stableOnly: Boolean): Symbol = {
      var sym = decls lookup name
      if (sym == NoSymbol) {
        sym = NoSymbol.newErrorSymbol(name)
        decls enter sym
      }
      sym
    }
    override def baseType(clazz: Symbol): Type = this
    override def safeToString: String = "<error>"
    override def narrow: Type = this
    override def kind = "ErrorType"
  }

  /** An object representing an unknown type, used during type inference.
   *  If you see WildcardType outside of inference it is almost certainly a bug.
   */
  case object WildcardType extends ProtoType {
    override def safeToString: String = "?"
    override def kind = "WildcardType"

    /** Equivalent to `List.fill(WildcardType)`, but more efficient as short lists are drawn from a cache. */
    def fillList(n: Int): List[WildcardType.type] = if (n < FillListCacheLimit) FillListCache(n) else List.fill(n)(WildcardType)
    private[this] final val FillListCacheLimit = 32
    private[this] lazy val FillListCache: Array[List[WildcardType.type]] = Array.iterate(List[WildcardType.type](), FillListCacheLimit)(WildcardType :: _)
  }
  /** BoundedWildcardTypes, used only during type inference, are created in
   *  two places that I can find:
   *
   *    1. If the expected type of an expression is an existential type,
   *       its hidden symbols are replaced with bounded wildcards.
   *    2. When an implicit conversion is being sought based in part on
   *       the name of a method in the converted type, a HasMethodMatching
   *       type is created: a MethodType with parameters typed as
   *       BoundedWildcardTypes.
   */
  case class BoundedWildcardType(override val bounds: TypeBounds) extends ProtoType with BoundedWildcardTypeApi {
    override def upperBound: Type = bounds.hi
    override def lowerBound: Type = bounds.lo
    override def isMatchedBy(tp: Type, depth: Depth)= isSubType(tp, bounds.hi, depth)
    override def canMatch(tp: Type, depth: Depth): Boolean = isSubType(bounds.lo, tp, depth)
    override def registerTypeEquality(tp: Type): Boolean = bounds.containsType(tp)
    override def toBounds: TypeBounds = bounds
    override def members = bounds.lo.members

    override def toVariantType: Type = bounds
    override def safeToString: String = "?" + bounds
    override def kind = "BoundedWildcardType"
    override def mapOver(map: TypeMap): Type = {
      val bounds1 = map(bounds)
      if (bounds1 eq bounds) this
      else BoundedWildcardType(bounds1.asInstanceOf[TypeBounds])
    }
    override def foldOver(folder: TypeFolder): Unit = folder(bounds)
  }

  object BoundedWildcardType extends BoundedWildcardTypeExtractor

  abstract class ProtoType extends Type {
    def toBounds: TypeBounds = TypeBounds.empty

    override def isWildcard = true
    override def members = ErrorType.decls

    // tp <:< this prototype?
    def isMatchedBy(tp: Type, depth: Depth): Boolean = true

    // could this prototype <:< tp?
    def canMatch(tp: Type, depth: Depth): Boolean = true

    // when comparing for type equality
    def registerTypeEquality(tp: Type): Boolean = true

    // Does this prototype denote that we're expecting a function?
    def expectsFunctionType: Boolean = false

    def asFunctionType: Type = NoType

    // represent this type as a ground type for use in varianceInType
    def toVariantType: Type = NoType
  }

  /** Lazily compute expected types for arguments to overloaded methods.
    * Primarily to improve parameter type inference for higher-order overloaded methods.
    *
    * Normally, overload resolution types the arguments to the alternatives without an expected type.
    * However, typing function literals and eta-expansion are driven by the expected type:
    *   - function literals usually don't have parameter types, which are derived from the expected type;
    *   - eta-expansion right now only happens when a function/sam type is expected.
    *
    * Now that the collections are full of overloaded HO methods, we should try harder to type check them nicely.
    *
    * (This paragraph is conceptually true, but not a spec.) To avoid breaking existing code,
    * we only provide an expected type (for each argument position) when:
    *   - there is at least one FunctionN type expected by one of the overloads:
    *     in this case, the expected type is a FunctionN[Ti, ?], where Ti are the argument types (they must all be =:=),
    *     and the expected result type is elided using a wildcard.
    *     This does not exclude any overloads that expect a SAM, because they conform to a function type through SAM conversion
    *   - OR: all overloads expect a SAM type of the same class, but with potentially varying result types (argument types must be =:=)
    *   - OR: all expected types collapse to the same type (by =:=, pushing down method type params to arguments types)
    *
    * We allow polymorphic cases, taking account any instantiation by the AntiPolyType prefix.
    * Constructors of polymorphic classes are not supported (type param occurrences use fresh symbols, hard to relate to class's type params).
    *
    * In all other cases, the old behavior is maintained: Wildcard is expected.
    */
  final case class OverloadedArgProto(argIdx: Either[Int, Name], pre: Type, alternatives: List[Symbol])(origUndets: List[Symbol]) extends ProtoType with SimpleTypeProxy {
    override def safeToString: String = underlying.safeToString
    override def kind = "OverloadedArgProto"

    override def underlying: Type = protoTp

    // If underlying is not wildcard, we may have constrained a first-try-typing too much,
    // so, when `!isWildcard` implicit search will try again with no expected type at all.
    // See e.g., adaptToArguments's code paths that depend on `isWildcard`
    override def isWildcard = underlying.isWildcard

    // Always match if we couldn't collapse the expected types contributed for this argument by the alternatives.
    // TODO: could we just match all function-ish types as an optimization? We previously used WildcardType
    override def isMatchedBy(tp: Type, depth: Depth): Boolean =
      isPastTyper || underlying == WildcardType ||
        isSubType(tp, underlying, depth) ||
        // NOTE: converting tp to a function type won't work, since `tp` need not be an actual sam type,
        // just some subclass of the sam expected by one of our overloads
        sameTypesFoldedSam.exists { underlyingSam => isSubType(tp, underlyingSam, depth) } // overload_proto_collapse.scala:55

    // Empty signals failure. We don't consider the 0-ary HOF case, since we are only concerned with inferring param types for these functions anyway
    def hofParamTypes = functionOrPfOrSamArgTypes(underlying)

    override def expectsFunctionType: Boolean = !hofParamTypes.isEmpty

    // TODO: include result type?
    override def asFunctionType =
      if (expectsFunctionType) functionType(hofParamTypes, WildcardType)
      else NoType

    override def mapOver(map: TypeMap): Type = {
      val pre1 = pre.mapOver(map)
      val alts1 = map.mapOver(alternatives)
      if ((pre ne pre1) || (alternatives ne alts1)) OverloadedArgProto(argIdx, pre1, alts1)(origUndets)
      else this
    }

    override def foldOver(folder: TypeFolder): Unit = {
      pre.foldOver(folder)
      folder.foldOver(alternatives)
    }

    // TODO
    // override def registerTypeEquality(tp: Type): Boolean = protoTp =:= tp


    // TODO: use =:=, but `!(typeOf[String with AnyRef] =:= typeOf[String])` (https://github.com/scala/scala-dev/issues/530)
    private def same(x: Type, y: Type) = (x <:< y) && (y <:< x)

    private object ParamAtIdx {
      def unapply(params: List[Symbol]): Option[Type] = {
        lazy val lastParamTp = params.last.tpe

        val argIdxMapped = argIdx match {
          case Left(idx) => idx
          case Right(name) => params.indexWhere(p => p.name == name && !p.isSynthetic)
        }

        // if we're asking for the last argument, or past, and it happens to be a repeated param -- strip the vararg marker and return the type
        if (!params.isEmpty && params.lengthCompare(argIdxMapped + 1) <= 0 && isRepeatedParamType(lastParamTp)) {
          Some(lastParamTp.dealiasWiden.typeArgs.head)
        } else if (params.isDefinedAt(argIdxMapped)) {
          Some(dropByName(params(argIdxMapped).tpe))
        } else None
      }
    }

    // replace origUndets: in chained calls, drop undets coming from earlier parts of the chain -- see pos/t11511
    // replace tparams in top-level PolyType: we don't want bounded wildcards showing up for an f-bounded type param...
    private def toWild(tp: Type): Type = tp match {
      case PolyType(tparams, tp) =>
        val undets = tparams ++ origUndets
        new SubstTypeMap(undets, WildcardType.fillList(undets.length)).apply(tp)
      case tp                    =>
        new SubstTypeMap(origUndets, WildcardType.fillList(origUndets.length)).apply(tp)
    }

    private lazy val sameTypesFolded = {
      // Collect all expected types contributed by the various alternatives for this argument (TODO: repeated params?)
      // Relative to `pre` at `alt.owner`, with `alt`'s type params approximated.
      def typeOfAlt(alt: Symbol): Type =
        // Use memberType so that a pre: AntiPolyType can instantiate its type params
        pre.memberType(alt) match {
          case PolyType(tparams, MethodType(ParamAtIdx(paramTp), res)) => PolyType(tparams, paramTp.asSeenFrom(pre, alt.owner))
          case MethodType(ParamAtIdx(paramTp), res)
              if !(alt.isConstructor && alt.owner.info.isInstanceOf[PolyType]) => paramTp.asSeenFrom(pre, alt.owner) // TODO: can we simplify this (Are those params in origUndets by chance?)
          // this is just too ugly, but the type params are out of whack and thus toWild won't catch them unless we rewrite as follows:
          // if (alt.isConstructor && alt.owner.info.isInstanceOf[PolyType]) {
          //   PolyType(alt.owner.info.typeParams.map(_.tpe.asSeenFrom(pre, alt.owner).typeSymbol), paramTp.asSeenFrom(pre, alt.owner))
          // } else paramTp.asSeenFrom(pre, alt.owner)
          case _ => NoType
        }
      // alternatives.map(fili).contains(NoType) implies sameTypesFolded.contains(NoType)
      // so, if one alternative did not contribute an argument type, we'll not collapse this column
      alternatives.foldLeft(Nil: List[Type]) { case (acc, alter) =>
        typeOfAlt(alter) match {
          case WildcardType => acc
          case tp => if (acc.exists(same(tp, _))) acc else tp :: acc
        }
      }
    }

    private lazy val sameTypesFoldedSam =
      sameTypesFolded.iterator.map(toWild).filter(tp => samOf(tp).exists).toList

    // Try to collapse all expected argument types (already distinct by =:=) into a single expected type,
    // so that we can use it to as the expected type to drive parameter type inference for a function literal argument.
    private lazy val protoTp = {
      val ABORT = (NoType, false, false)

      // we also consider any function-ish type equal as long as the argument types are
      def sameHOArgTypes(tp1: Type, tp2: Type) = tp1 == WildcardType || {
        val hoArgTypes1 = functionOrPfOrSamArgTypes(tp1.resultType)
        // println(s"sameHOArgTypes($tp1, $tp2) --> $hoArgTypes1 === $hoArgTypes2 : $res")
        !hoArgTypes1.isEmpty && hoArgTypes1.corresponds(functionOrPfOrSamArgTypes(tp2.resultType))(same)
      }

      // TODO: compute functionOrPfOrSamArgTypes during fold?
      val (sameHoArgTypesFolded, partialFun, regularFun) =
        sameTypesFolded.foldLeft((WildcardType: Type, false, false)) {
          case (ABORT, _)              => ABORT
          case ((acc, partialFun, regularFun), tp) if sameHOArgTypes(acc, tp) =>
            val wild = toWild(tp)
            (tp, partialFun || isPartialFunctionType(wild), regularFun || isFunctionType(wild))
          case _                       => ABORT // different HO argument types encountered
        }

      if ((sameHoArgTypesFolded eq WildcardType) || (sameHoArgTypesFolded eq NoType)) WildcardType
      else functionOrPfOrSamArgTypes(toWild(sameHoArgTypesFolded)) match {
        case Nil     =>
          // Ok, it's not a function proto, but we did collapse to only one type -- why not use that as our expected type?
          // we exclude constructors because a polymorphic class's type params are not represented as part of the constructor method's type, and thus toWild won't work
          sameTypesFolded match {
            case onlyType :: Nil =>
              //              println(s"collapsed argument types at index $argIdx to ${toWild(onlyType)} for ${alternatives map (alt => (alt, pre memberType alt))} ")
              toWild(onlyType)
            case _               => WildcardType
          }
        case hofArgs =>
          if (partialFun) appliedType(PartialFunctionClass, hofArgs :+ WildcardType)
          else if (regularFun) functionType(hofArgs, WildcardType)
               // if we saw a variety of SAMs, can't collapse them -- what if they were accidental sams and we're not going to supply a function literal?
          else if (sameTypesFolded.lengthCompare(1) == 0) toWild(sameTypesFolded.head)
          else WildcardType
      }
    }
  }

  /** An object representing a non-existing type */
  case object NoType extends Type {
    override def isTrivial: Boolean = true
    override def safeToString: String = "<notype>"
    override def kind = "NoType"
  }

  /** An object representing a non-existing prefix */
  case object NoPrefix extends Type {
    override def isTrivial: Boolean = true
    override def prefixString = ""
    override def safeToString: String = "<noprefix>"
    override def kind = "NoPrefixType"
  }

  /** A class for this-types of the form <sym>.this.type
   */
  abstract case class ThisType(sym: Symbol) extends SingletonType with ThisTypeApi {
    if (!sym.isClass && !sym.isFreeType) {
      // scala/bug#6640 allow StubSymbols to reveal what's missing from the classpath before we trip the assertion.
      sym.failIfStub()
      abort(s"ThisType($sym) for sym which is not a class")
    }

    override def isTrivial: Boolean = sym.isPackageClass
    override def typeSymbol = sym
    override def underlying: Type = sym.typeOfThis
    override def isHigherKinded = sym.isRefinementClass && underlying.isHigherKinded
    override def prefixString =
      if (settings.isDebug) sym.nameString + ".this."
      else if (sym.isAnonOrRefinementClass) "this."
      else if (sym.isOmittablePrefix) ""
      else if (sym.isModuleClass) sym.fullNameString + "."
      else sym.nameString + ".this."
    override def safeToString: String =
      if (sym.isEffectiveRoot) "" + sym.name
      else super.safeToString
    override def narrow: Type = this
    override def kind = "ThisType"
  }

  final class UniqueThisType(sym: Symbol) extends ThisType(sym) { }

  object ThisType extends ThisTypeExtractor {
    def apply(sym: Symbol): Type = (
      if (!phase.erasedTypes) unique(new UniqueThisType(sym))
      else sym.tpe_*
    )
  }

  /** A class for singleton types of the form `<prefix>.<sym.name>.type`.
   *  Cannot be created directly; one should always use `singleType` for creation.
   */
  abstract case class SingleType(pre: Type, sym: Symbol) extends SingletonType with SingleTypeApi {
    private[this] var trivial: ThreeValue = UNKNOWN
    override def isTrivial: Boolean = {
      if (trivial == UNKNOWN) trivial = fromBoolean(pre.isTrivial)
      toBoolean(trivial)
    }
    override def isGround = sym.isPackageClass || pre.isGround

    @volatile private[reflect] var underlyingCache: Type = NoType
    @volatile private[reflect] var underlyingPeriod = NoPeriod
    private[Types] def invalidateSingleTypeCaches(): Unit = {
      underlyingCache = NoType
      underlyingPeriod = NoPeriod
    }
    override def underlying: Type = {
      val cache = underlyingCache
      if (underlyingPeriod == currentPeriod && cache != null) cache
      else {
        defineUnderlyingOfSingleType(this) // this line is synchronized in runtime reflection
        underlyingCache
      }
    }

    // more precise conceptually, but causes cyclic errors:    (paramss exists (_ contains sym))
    override def isImmediatelyDependent = (sym ne NoSymbol) && (sym.owner.isMethod && sym.isValueParameter)
/*
    override def narrow: Type = {
      if (phase.erasedTypes) this
      else {
        val thissym = refinedType(List(this), sym.owner, EmptyScope).typeSymbol
        if (sym.owner != NoSymbol) {
          //Console.println("narrowing module " + sym + thissym.owner);
          thissym.typeOfThis = this
        }
        thissym.thisType
      }
    }
*/
    override def narrow: Type = this

    override def termSymbol = sym
    override def prefix: Type = pre
    override def prefixString = (
      if (sym.skipPackageObject.isOmittablePrefix) ""
      else if (sym.isPackageObjectOrClass) pre.prefixString
      else pre.prefixString + sym.nameString + "."
    )
    override def kind = "SingleType"
    override def mapOver(map: TypeMap): Type = {
      if (sym.isPackageClass) this // short path
      else {
        val pre1 = map(pre)
        if (pre1 eq pre) this
        else singleType(pre1, sym)
      }
    }
    override def foldOver(folder: TypeFolder): Unit = folder(pre)
  }

  final class UniqueSingleType(pre: Type, sym: Symbol) extends SingleType(pre, sym)

  object SingleType extends SingleTypeExtractor {
    def apply(pre: Type, sym: Symbol): Type = {
      unique(new UniqueSingleType(pre, sym))
    }
  }

  protected def defineUnderlyingOfSingleType(tpe: SingleType): Unit = {
    val period = tpe.underlyingPeriod
    if (period != currentPeriod) {
      tpe.underlyingPeriod = currentPeriod
      if (!isValid(period)) {
        // [Eugene to Paul] needs review
        tpe.underlyingCache = if (tpe.sym == NoSymbol) ThisType(rootMirror.RootClass) else {
          val result = tpe.pre.memberType(tpe.sym).resultType
          if (isScalaRepeatedParamType(result)) repeatedToSeq(result) else result
        }
        assert(tpe.underlyingCache ne tpe, tpe)
      }
    }
  }

  abstract case class SuperType(thistpe: Type, supertpe: Type) extends SingletonType with SuperTypeApi {
    private[this] var trivial: ThreeValue = UNKNOWN
    override def isTrivial: Boolean = {
      if (trivial == UNKNOWN) trivial = fromBoolean(thistpe.isTrivial && supertpe.isTrivial)
      toBoolean(trivial)
    }
    override def typeSymbol = thistpe.typeSymbol
    override def underlying = supertpe
    override def prefix: Type = supertpe.prefix
    override def prefixString = thistpe.prefixString.replaceAll("""\bthis\.$""", "super.")
    override def narrow: Type = thistpe.narrow
    override def kind = "SuperType"
    override def mapOver(map: TypeMap): Type = {
      val thistp1 = map(thistpe)
      val supertp1 = map(supertpe)
      if ((thistp1 eq thistpe) && (supertp1 eq supertpe)) this
      else SuperType(thistp1, supertp1)
    }
    override def foldOver(folder: TypeFolder): Unit = {
      folder(thistpe)
      folder(supertpe)
    }
  }

  final class UniqueSuperType(thistp: Type, supertp: Type) extends SuperType(thistp, supertp)

  object SuperType extends SuperTypeExtractor {
    def apply(thistp: Type, supertp: Type): Type = {
      if (phase.erasedTypes) supertp
      else unique(new UniqueSuperType(thistp, supertp))
    }
  }

  /** A class for the bounds of abstract types and type parameters
   */
  abstract case class TypeBounds(lo: Type, hi: Type) extends SubType with TypeBoundsApi {
    def supertype = hi
    override def isTrivial: Boolean = lo.isTrivial && hi.isTrivial
    override def bounds: TypeBounds = this
    override def upperBound: Type = hi
    override def lowerBound: Type = lo
    def containsType(that: Type) = that match {
      case TypeBounds(_, _) => that <:< this
      case _                => lo <:< that && that <:< hi
    }
    private def emptyLowerBound = typeIsNothing(lo) || lo.isWildcard
    private def emptyUpperBound = typeIsAnyOrJavaObject(hi) || hi.isWildcard
    def isEmptyBounds = emptyLowerBound && emptyUpperBound

    override def safeToString = scalaNotation(_.toString)

    /** Bounds notation used in Scala syntax.
      * For example +This <: scala.collection.generic.Sorted[K,This].
      */
    private[internal] def scalaNotation(typeString: Type => String): String =
      (if (emptyLowerBound) "" else " >: " + typeString(lo)) +
      (if (emptyUpperBound) "" else " <: " + typeString(hi))
    /** Bounds notation used in https://adriaanm.github.com/files/higher.pdf.
      * For example *(scala.collection.generic.Sorted[K,This]).
      */
    private[internal] def starNotation(typeString: Type => String): String =
      if (emptyLowerBound && emptyUpperBound) ""
      else if (emptyLowerBound) s"(${typeString(hi)})"
      else s"(${typeString(lo)}, ${typeString(hi)})"
    override def kind = "TypeBoundsType"
    override def mapOver(map: TypeMap): Type = {
      val lo1 = map match {
        case vtm: VariancedTypeMap => vtm.flipped(vtm(lo))
        case _ => map(lo)
      }
      val hi1 = map(hi)
      if ((lo1 eq lo) && (hi1 eq hi)) this
      else TypeBounds(lo1, hi1)
    }
    override def foldOver(folder: TypeFolder): Unit = { folder(lo); folder(hi) }
  }

  final class UniqueTypeBounds(lo: Type, hi: Type) extends TypeBounds(lo, hi)

  object TypeBounds extends TypeBoundsExtractor {
    def empty: TypeBounds           = apply(NothingTpe, AnyTpe)
    def upper(hi: Type): TypeBounds = apply(NothingTpe, hi)
    def lower(lo: Type): TypeBounds = apply(lo, AnyTpe)
    def apply(lo: Type, hi: Type): TypeBounds = {
      unique(new UniqueTypeBounds(lo, hi)).asInstanceOf[TypeBounds]
    }
  }

  object CompoundType {
    def unapply(tp: Type): Option[(List[Type], Scope, Symbol)] = tp match {
      case ClassInfoType(parents, decls, clazz) => Some((parents, decls, clazz))
      case RefinedType(parents, decls)          => Some((parents, decls, tp.typeSymbol))
      case _                                    => None
    }
  }

  /** A common base class for intersection types and class types
   */
  abstract class CompoundType extends Type with CompoundTypeApi {

    private[reflect] var baseTypeSeqCache: BaseTypeSeq = _
    private[reflect] var baseTypeSeqPeriod = NoPeriod
    private[reflect] var baseClassesCache: List[Symbol] = _
    private[reflect] var baseClassesPeriod = NoPeriod
    private[Types] def invalidatedCompoundTypeCaches(): Unit = {
      baseTypeSeqCache = null
      baseTypeSeqPeriod = NoPeriod
      baseClassesCache = null
      baseClassesPeriod = NoPeriod
    }

    override def baseTypeSeq: BaseTypeSeq = {
      val cached = baseTypeSeqCache
      if (baseTypeSeqPeriod == currentPeriod && cached != null && cached != undetBaseTypeSeq)
        cached
      else {
        defineBaseTypeSeqOfCompoundType(this)
        if (baseTypeSeqCache eq undetBaseTypeSeq)
          throw new RecoverableCyclicReference(typeSymbol)

        baseTypeSeqCache
      }
    }

    override def baseTypeSeqDepth: Depth = baseTypeSeq.maxDepth

    override def baseClasses: List[Symbol] = {
      val cached = baseClassesCache
      if (baseClassesPeriod == currentPeriod && cached != null) cached
      else {
        defineBaseClassesOfCompoundType(this)
        if (baseClassesCache eq null)
          throw new RecoverableCyclicReference(typeSymbol)

        baseClassesCache
      }
    }

    /** The slightly less idiomatic use of Options is due to
     *  performance considerations. A version using for comprehensions
     *  might be too slow (this is deemed a hotspot of the type checker).
     *
     *  See with Martin before changing this method.
     */
    def memo[A](op1: => A)(op2: Type => A): A = {
      def updateCache(): A = {
        intersectionWitness(parents) = new WeakReference(this)
        op1
      }

      intersectionWitness get parents match {
        case Some(ref) =>
          ref.get match {
            case Some(w) => if (w eq this) op1 else op2(w)
            case None => updateCache()
          }
        case None => updateCache()
      }
    }

    override def baseType(sym: Symbol): Type = {
      val index = baseTypeIndex(sym)
      if (index >= 0) baseTypeSeq(index) else NoType
    }

    override def narrow: Type = typeSymbol.thisType

    override def isStructuralRefinement: Boolean =
      typeSymbol.isAnonOrRefinementClass && (decls exists symbolIsPossibleInRefinement)

    protected def shouldForceScope = settings.isDebug || parents.isEmpty || !decls.isEmpty
    protected def initDecls        = fullyInitializeScope(decls)
    protected def scopeString      = if (shouldForceScope) initDecls.mkString("{", "; ", "}") else ""
    override def safeToString      = parentsString(parents) + scopeString
  }

  protected def computeBaseClasses(tpe: Type): List[Symbol] = {
    val parents = tpe.parents // adriaan says tpe.parents does work sometimes, so call it only once
    val baseTail = (
      if (parents.isEmpty || parents.head.isInstanceOf[PackageTypeRef]) Nil
      else {
        //Console.println("computing base classes of " + typeSymbol + " at phase " + phase);//DEBUG
        // optimized, since this seems to be performance critical
        val superclazz = parents.head // parents.isEmpty was already excluded
        var mixins     = parents.tail
        val sbcs       = superclazz.baseClasses
        var bcs        = sbcs
        def isNew(clazz: Symbol): Boolean = (
          superclazz.baseTypeIndex(clazz) < 0 &&
          { var p = bcs
            while ((p ne sbcs) && (p.head != clazz)) p = p.tail
            p eq sbcs
          }
        )
        while (!mixins.isEmpty) {
          def addMixinBaseClasses(mbcs: List[Symbol]): List[Symbol] =
            if (mbcs.isEmpty) bcs
            else if (isNew(mbcs.head)) mbcs.head :: addMixinBaseClasses(mbcs.tail)
            else addMixinBaseClasses(mbcs.tail)
          bcs = addMixinBaseClasses(mixins.head.baseClasses)
          mixins = mixins.tail
        }
        bcs
      }
    )
    tpe.typeSymbol :: baseTail
  }

  protected def defineBaseTypeSeqOfCompoundType(tpe: CompoundType) = {
    val period = tpe.baseTypeSeqPeriod
    if (period != currentPeriod) {
      tpe.baseTypeSeqPeriod = currentPeriod
      if (!isValidForBaseClasses(period)) {
        // If the BTS contains TypeVars, replace those with typerefs to the original type params before taking BTS,
        // after BTS, map them back.
        // TODO: rework BTS to deal with TypeVars in the same way on the fly
        if (tpe.parents exists typeContainsTypeVar) {
          val tvarFor = mutable.Map.empty[Type, TypeVar]
          // After this TypeMap, it's safe to recurse (`tpe.parents exists typeContainsTypeVar` above is `false`)
          val varToParam = new TypeMap {
            def apply(tp: Type) = tp match {
              case tv: TypeVar => // Applying a type constructor variable to arguments results in a new instance of AppliedTypeVar each time
                val toOrigin = appliedType(tv.origin.typeSymbol.typeConstructor, tv.typeArgs.mapConserve(this))
                tvarFor(toOrigin) = tv
                toOrigin
              case _ => tp.mapOver(this)
            }
          }
          // computes tvarFor
          val tpWithoutTypeVars = copyRefinedType(tpe.asInstanceOf[RefinedType], tpe.parents map varToParam, varToParam mapOver tpe.decls)

          val paramToVar = new TypeMap {
            val paramToVarMap = tvarFor.toMap // capture the map so we can undo the rewrite when the BTS is queried later
            def apply(tp: Type): Type = tp match {
              case tr: TypeRef => paramToVarMap.getOrElse(tr, mapOver(tp))
              case _ => tp.mapOver(this)
            }
          }

          tpe.baseTypeSeqCache = tpWithoutTypeVars.baseTypeSeq lateMap paramToVar
        } else {
          if (settings.areStatisticsEnabled) statistics.incCounter(compoundBaseTypeSeqCount)
          val start = if (settings.areStatisticsEnabled) statistics.pushTimer(typeOpsStack, baseTypeSeqNanos) else null
          try {
            tpe.baseTypeSeqCache = undetBaseTypeSeq
            tpe.baseTypeSeqCache =
              if (tpe.typeSymbol.isRefinementClass)
                tpe.memo(compoundBaseTypeSeq(tpe))(_.baseTypeSeq updateHead tpe.typeSymbol.tpe_*)
              else
                compoundBaseTypeSeq(tpe)
          } finally {
            if (settings.areStatisticsEnabled) statistics.popTimer(typeOpsStack, start)
          }
          // [Martin] suppressing memoization solves the problem with "same type after erasure" errors
          // when compiling with
          // scalac scala.collection.IterableViewLike.scala scala.collection.IterableLike.scala
          // I have not yet figured out precisely why this is the case.
          // My current assumption is that taking memos forces baseTypeSeqs to be computed
          // at stale types (i.e. the underlying typeSymbol has already another type).
          // I do not yet see precisely why this would cause a problem, but it looks
          // fishy in any case.
        }
      }
    }
    //Console.println(s"baseTypeSeq(${tpe.typeSymbol}) = ${tpe.baseTypeSeqCache.toList}") //DEBUG
    if (tpe.baseTypeSeqCache eq undetBaseTypeSeq)
      throw new TypeError(s"illegal cyclic inheritance involving ${tpe.typeSymbol}")
  }

  protected def defineBaseClassesOfCompoundType(tpe: CompoundType): Unit = {
    val period = tpe.baseClassesPeriod
    if (period != currentPeriod) {
      tpe.baseClassesPeriod = currentPeriod
      if (!isValidForBaseClasses(period)) {
        val start = if (settings.areStatisticsEnabled) statistics.pushTimer(typeOpsStack, baseClassesNanos) else null
        try {
          tpe.baseClassesCache = null
          tpe.baseClassesCache = tpe.memo(computeBaseClasses(tpe))(tpe.typeSymbol :: _.baseClasses.tail)
        }
        finally {
          if (settings.areStatisticsEnabled) statistics.popTimer(typeOpsStack, start)
        }
      }
    }
    if (tpe.baseClassesCache eq null)
      throw new TypeError("illegal cyclic reference involving " + tpe.typeSymbol)
  }

  /** A class representing intersection types with refinements of the form
   *    `<parents_0> with ... with <parents_n> { decls }`
   *  Cannot be created directly;
   *  one should always use `refinedType` for creation.
   */
  case class RefinedType(override val parents: List[Type],
                         override val decls: Scope) extends CompoundType with RefinedTypeApi {
    override def isHigherKinded = (
      !parents.isEmpty &&
      (parents.forall(_.isHigherKinded)) &&
      !phase.erasedTypes
    )
    override def typeParams =
      if (isHigherKinded) firstParent.typeParams
      else super.typeParams

    //@M may result in an invalid type (references to higher-order args become dangling )
    override def typeConstructor =
      copyRefinedType(this, parents map (_.typeConstructor), decls)

    final override def normalize: Type =
      if (phase.erasedTypes) normalizeImpl
      else {
        if (normalized eq null) normalized = normalizeImpl
        normalized
      }

    private[this] var normalized: Type = _
    private def normalizeImpl = {
      // TODO see comments around def intersectionType and def merge
      // scala/bug#8575 The dealias is needed here to keep subtyping transitive, example in run/t8575b.scala
      val flattened: LinkedHashSet[Type] = LinkedHashSet.empty[Type]
      def dealiasRefinement(tp: Type) = if (tp.dealias.isInstanceOf[RefinedType]) tp.dealias else tp
      def loop(tp: Type): Unit = dealiasRefinement(tp) match {
        case RefinedType(parents, decls) if decls.isEmpty => parents.foreach(loop)
        case tp1 => flattened.add(tp1)
      }
      parents foreach loop
      if (decls.isEmpty && flattened.size == 1) {
        flattened.head
      } else if (!flattened.iterator.sameElements(parents)) {
        refinedType(flattened.toList, if (typeSymbol eq NoSymbol) NoSymbol else typeSymbol.owner, decls, NoPosition)
      } else if (isHigherKinded) {
        etaExpand
      } else super.normalize
    }

    final override def etaExpand: Type = {
      // MO to AM: This is probably not correct
      // If they are several higher-kinded parents with different bounds we need
      // to take the intersection of their bounds
      // !!! inconsistent with TypeRef.etaExpand that uses initializedTypeParams
      if (!isHigherKinded) this
      else typeFun(
        typeParams,
        RefinedType(
          parents map {
            case TypeRef(pre, sym, List()) => TypeRef(pre, sym, dummyArgs)
            case p => p
          },
          decls,
          typeSymbol))
    }

    override def kind = "RefinedType"
    override def mapOver(map: TypeMap): Type = {
      val parents1 = parents mapConserve map
      val decls1 = map.mapOver(decls)
      copyRefinedType(this, parents1, decls1)
    }
    override def foldOver(folder: TypeFolder): Unit = {
      parents.foreach(folder)
      folder.foldOver(decls)
    }
  }

  final class RefinedType0(parents: List[Type], decls: Scope, clazz: Symbol) extends RefinedType(parents, decls) {
    override def typeSymbol = clazz
  }

  object RefinedType extends RefinedTypeExtractor {
    def apply(parents: List[Type], decls: Scope, clazz: Symbol): RefinedType =
      new RefinedType0(parents, decls, clazz)
  }

  /** Overridden in reflection compiler */
  def validateClassInfo(tp: ClassInfoType): Unit = {}

  /** A class representing a class info
   */
  case class ClassInfoType(
    override val parents: List[Type],
    override val decls: Scope,
    override val typeSymbol: Symbol) extends CompoundType with ClassInfoTypeApi
  {
    validateClassInfo(this)

    /** refs indices */
    private final val NonExpansive = 0
    private final val Expansive = 1

    /** initialization states */
    private final val UnInitialized = 0
    private final val Initializing = 1
    private final val Initialized = 2

    private type RefMap = Map[Symbol, Set[Symbol]]

    /** All type parameters reachable from given type parameter
     *  by a path which contains at least one expansive reference.
     *  @See Kennedy, Pierce: On Decidability of Nominal Subtyping with Variance
     */
    private[scala] def expansiveRefs(tparam: Symbol) = {
      if (state == UnInitialized) {
        computeRefs()
        while (state != Initialized) propagate()
      }
      getRefs(Expansive, tparam)
    }

    /* The rest of this class is auxiliary code for `expansiveRefs`
     */

    /** The type parameters which are referenced type parameters of this class.
     *  Two entries: refs(0): Non-expansive references
     *               refs(1): Expansive references
     *  Syncnote: This var need not be protected with synchronized, because
     *  it is accessed only from expansiveRefs, which is called only from
     *  Typer.
     */
    private[this] var refs: Array[RefMap] = _

    /** The initialization state of the class: UnInitialized --> Initializing --> Initialized
     *  Syncnote: This var need not be protected with synchronized, because
     *  it is accessed only from expansiveRefs, which is called only from
     *  Typer.
     */
    private var state = UnInitialized

    /** Get references for given type parameter
     *  @param  which in {NonExpansive, Expansive}
     *  @param  from  The type parameter from which references originate.
     */
    private def getRefs(which: Int, from: Symbol): Set[Symbol] = refs(which) get from match {
      case Some(set) => set
      case none => Set()
    }

    /** Augment existing refs map with reference <pre>from -> to</pre>
     *  @param  which <- {NonExpansive, Expansive}
     */
    private def addRef(which: Int, from: Symbol, to: Symbol): Unit = {
      refs(which) = refs(which) + (from -> (getRefs(which, from) + to))
    }

    /** Augment existing refs map with references <pre>from -> sym</pre>, for
     *  all elements <pre>sym</pre> of set `to`.
     *  @param  which <- {NonExpansive, Expansive}
     */
    private def addRefs(which: Int, from: Symbol, to: Set[Symbol]): Unit = {
      refs(which) = refs(which) + (from -> (getRefs(which, from) ++ to))
    }

    /** The ClassInfoType which belongs to the class containing given type parameter
     */
    @tailrec
    private def classInfo(tparam: Symbol): ClassInfoType =
      tparam.owner.info.resultType match {
        case ci: ClassInfoType => ci
        case _ => classInfo(ObjectClass) // something's wrong; fall back to safe value
                                         // (this can happen only for erroneous programs).
      }

    // TODO should we pull this out to reduce memory footprint of ClassInfoType?
    private object enterRefs extends TypeMap {
      private[this] var tparam: Symbol = _

      def apply(tp: Type): Type = {
        tp match {
          case tr @ TypeRef(_, sym, args) if !args.isEmpty =>
            val tparams = tr.initializedTypeParams
            devWarningIf(!sameLength(tparams, args)) {
              s"Mismatched zip in computeRefs(): ${sym.info.typeParams}, $args"
            }

            foreach2(tparams, args) { (tparam1, arg) =>
              if (arg contains tparam) {
                addRef(NonExpansive, tparam, tparam1)
                if (arg.typeSymbol != tparam)
                  addRef(Expansive, tparam, tparam1)
              }
            }
          case _ =>
        }
        tp.mapOver(this)
      }
      def enter(tparam0: Symbol, parent: Type): Unit = {
        this.tparam = tparam0
        this(parent)
      }
    }

    /** Compute initial (one-step) references and set state to `Initializing`.
     */
    private def computeRefs(): Unit = {
      refs = Array(Map(), Map())
      typeSymbol.typeParams foreach { tparam =>
        parents foreach { p =>
          enterRefs.enter(tparam, p)
        }
      }
      state = Initializing
    }

    /** Propagate to form transitive closure.
     *  Set state to Initialized if no change resulted from propagation.
     *  @return   true iff there as a change in last iteration
     */
    private def propagate(): Boolean = {
      if (state == UnInitialized) computeRefs()
      //Console.println("Propagate "+symbol+", initial expansive = "+refs(Expansive)+", nonexpansive = "+refs(NonExpansive))//DEBUG
      val lastRefs = Array(refs(0), refs(1))
      state = Initialized
      var change = false
      for ((from, targets) <- refs(NonExpansive).iterator)
        for (target <- targets) {
          val thatInfo = classInfo(target)
          if (thatInfo.state != Initialized)
            change = change | thatInfo.propagate()
          addRefs(NonExpansive, from, thatInfo.getRefs(NonExpansive, target))
          addRefs(Expansive, from, thatInfo.getRefs(Expansive, target))
        }
      for ((from, targets) <- refs(Expansive).iterator)
        for (target <- targets) {
          val thatInfo = classInfo(target)
          if (thatInfo.state != Initialized)
            change = change | thatInfo.propagate()
          addRefs(Expansive, from, thatInfo.getRefs(NonExpansive, target))
        }
      change = change || refs(0) != lastRefs(0) || refs(1) != lastRefs(1)
      if (change) state = Initializing
      //else Console.println("Propagate "+symbol+", final expansive = "+refs(Expansive)+", nonexpansive = "+refs(NonExpansive))//DEBUG
      change
    }

    override def kind = "ClassInfoType"
    /** A nicely formatted string with newlines and such.
     */
    def formattedToString = parents.mkString("\n        with ") + scopeString
    override protected def shouldForceScope = settings.isDebug || decls.size > 1
    override protected def scopeString      = initDecls.mkString(" {\n  ", "\n  ", "\n}")
    override def safeToString               = if (shouldForceScope) formattedToString else super.safeToString
  }

  object ClassInfoType extends ClassInfoTypeExtractor

  class PackageClassInfoType(decls: Scope, clazz: Symbol)
  extends ClassInfoType(List(), decls, clazz)

  /** A class representing a constant type. A constant type is either the inferred type of a constant
   *  value or an explicit or inferred literal type. Both may be constant folded at the type level,
   *  however literal types are not folded at the term level and do not elide effects.
   */
  abstract class ConstantType extends SingletonType with ConstantTypeApi {
    //assert(underlying.typeSymbol != UnitClass)
    val value: Constant

    override def isTrivial: Boolean = true
    override def kind = "ConstantType"
  }

  object ConstantType extends ConstantTypeExtractor {
    def apply(c: Constant): ConstantType = FoldableConstantType(c)
    def unapply(tpe: ConstantType): Some[Constant] = Some(tpe.value)
  }

  /** A class representing the inferred type of a constant value. Constant types and their
   *  corresponding terms are constant-folded during type checking. To avoid constant folding, use
   *  the type returned by `deconst` instead.
   */
  abstract case class FoldableConstantType(value: Constant) extends ConstantType {
    override def underlying: Type =
      if (value.isSuitableLiteralType) LiteralType(value) else value.tpe
    override def deconst: Type = underlying.deconst
    override def safeToString: String = underlying.widen.toString + "(" + value.escapedStringValue + ")"
  }

  final class UniqueConstantType(value: Constant) extends FoldableConstantType(value)

  object FoldableConstantType {
    def apply(value: Constant) = unique(new UniqueConstantType(value))
  }

  /** A class representing an explicit or inferred literal type. Literal types may be be folded at
   *  at the type level during type checking, however they will not be folded at the term level and
   *  effects will not be elided.
   */
  abstract case class LiteralType(value: Constant) extends ConstantType {
    override def underlying: Type = value.tpe
    override def deconst: Type = this
    override def safeToString: String = value.escapedStringValue
  }

  final class UniqueLiteralType(value: Constant) extends LiteralType(value)

  object LiteralType {
    def apply(value: Constant) = unique(new UniqueLiteralType(value))
  }

  class ArgsTypeRef(pre0: Type, sym0: Symbol, args0: List[Type]) extends TypeRef(pre0, sym0, args0) {
    require(args0 ne Nil, this)

    /** No unapplied type params size it has (should have) equally as many args. */
    override def isHigherKinded = false
    override def typeParams = Nil

    // note: does not go through typeRef. There's no need to because
    // neither `pre` nor `sym` changes.  And there's a performance
    // advantage to call TypeRef directly.
    override def typeConstructor = TypeRef(pre, sym, Nil)
  }

  class ModuleTypeRef(pre0: Type, sym0: Symbol) extends NoArgsTypeRef(pre0, sym0) {
    require(sym.isModuleClass, sym)
    private[this] var narrowedCache: Type = _
    override def narrow = {
      if (narrowedCache eq null)
        narrowedCache = singleType(pre, sym.sourceModule)

      narrowedCache
    }
    override private[Types] def invalidateTypeRefCaches(): Unit = {
      super.invalidateTypeRefCaches()
      narrowedCache = null
    }
    override def forceDirectSuperclasses() =
      sym0.rawInfo.decls.foreach { decl =>
        if (decl.isModule || !decl.isTerm) decl.rawInfo.forceDirectSuperclasses()
      }
    override protected def finishPrefix(rest: String) = objectPrefix + rest
    override def directObjectString = super.safeToString
    override def toLongString = toString
    override def safeToString =
      if (sym.isOmittablePrefix) s"${if (sym.isPackageObjectOrClass || sym.isJavaDefined) sym.fullNameString else sym.nameString}.type"
      else s"${prefixString}type"
    override def prefixString = if (sym.isOmittablePrefix) "" else prefix.prefixString + sym.nameString + "."
  }
  class PackageTypeRef(pre0: Type, sym0: Symbol) extends ModuleTypeRef(pre0, sym0) {
    require(sym.isPackageClass, sym)
    override protected def finishPrefix(rest: String) = packagePrefix + rest
  }
  class RefinementTypeRef(pre0: Type, sym0: Symbol) extends NoArgsTypeRef(pre0, sym0) {
    require(sym.isRefinementClass, sym)

    // I think this is okay, but see #1241 (r12414), #2208, and typedTypeConstructor in Typers
    override protected def normalizeImpl: Type = pre.memberInfo(sym).normalize
    override protected def finishPrefix(rest: String) = "" + sym.info
  }

  class NoArgsTypeRef(pre0: Type, sym0: Symbol) extends TypeRef(pre0, sym0, Nil) {
    // A reference (in a Scala program) to a type that has type parameters, but where the reference
    // does not include type arguments. Note that it doesn't matter whether the symbol refers
    // to a java or scala symbol, but it does matter whether it occurs in java or scala code.
    // TypeRefs w/o type params that occur in java signatures/code are considered raw types, and are
    // represented as existential types.
    override def isHigherKinded = (typeParams ne Nil)
    override def typeParams     = if (isDefinitionsInitialized) sym.typeParams else sym.unsafeTypeParams

    override def instantiateTypeParams(formals: List[Symbol], actuals: List[Type]): Type =
      if (isHigherKinded) {
        if (typeParams.forall(formals.contains))
          copyTypeRef(this, pre, sym, actuals)
        // partial application (needed in infer when bunching type arguments from classes and methods together)
        else
          copyTypeRef(this, pre, sym, dummyArgs).instantiateTypeParams(formals, actuals)
      }
      else
        super.instantiateTypeParams(formals, actuals)

    override def narrow =
      if (sym.isModuleClass) singleType(pre, sym.sourceModule)
      else super.narrow

    override def typeConstructor = this
    // eta-expand, subtyping relies on eta-expansion of higher-kinded types

    override protected def normalizeImpl: Type =
      if (isHigherKinded) etaExpand else super.normalizeImpl
  }

  trait NonClassTypeRef extends TypeRef {
    require(sym.isNonClassType, sym)

    /** Syncnote: These are pure caches for performance; no problem to evaluate these
      * several times. Hence, no need to protected with synchronized in a multi-threaded
      * usage scenario.
      */
    private[this] var relativeInfoCache: Type = _
    private[this] var relativeInfoCacheValidForPeriod: Period = NoPeriod
    private[this] var relativeInfoCacheValidForSymInfo: Type = _

    override private[Types] def invalidateTypeRefCaches(): Unit = {
      super.invalidateTypeRefCaches()
      relativeInfoCache = NoType
      relativeInfoCacheValidForPeriod = NoPeriod
      relativeInfoCacheValidForSymInfo = null
    }

    final override protected def relativeInfo = {
      val symInfo = sym.info
      if ((relativeInfoCache eq null) || (relativeInfoCacheValidForSymInfo ne symInfo) || (relativeInfoCacheValidForPeriod != currentPeriod)) {
        relativeInfoCache = super.relativeInfo

        if (this.isInstanceOf[AbstractTypeRef]) validateRelativeInfo()

        relativeInfoCacheValidForSymInfo = symInfo
        relativeInfoCacheValidForPeriod = currentPeriod
      }
      relativeInfoCache
    }

    private def validateRelativeInfo(): Unit = relativeInfoCache match {
      // If a subtyping cycle is not detected here, we'll likely enter an infinite
      // loop before a sensible error can be issued.  scala/bug#5093 is one example.
      case x: SubType if x.supertype eq this =>
        relativeInfoCache = null
        throw new RecoverableCyclicReference(sym)
      case _ =>
    }
  }


  trait AliasTypeRef extends NonClassTypeRef {
    require(sym.isAliasType, sym)

    override def dealias    = if (typeParamsMatchArgs) betaReduce.dealias else super.dealias
    override def narrow     = normalize.narrow
    override def prefix     = if (this ne normalize) normalize.prefix else pre
    override def termSymbol = if (this ne normalize) normalize.termSymbol else super.termSymbol
    override def typeSymbol = if (this ne normalize) normalize.typeSymbol else sym

    // Avoid calling super.isError when we're a type constructor, as that will eta-expand, which can cause spurious cycles,
    // without resulting in additional information about our error state in any case
    override def isError: Boolean = sym.isError || !isHigherKinded && super.isError

    override protected[Types] def parentsImpl: List[Type] = normalize.parents map relativize

    // `baseClasses` is sensitive to type args when referencing type members
    // consider `type foo[x] = x`, `typeOf[foo[String]].baseClasses` should be the same as `typeOf[String].baseClasses`,
    // which would be lost by looking at `sym.info` without propagating args
    // since classes cannot be overridden, the prefix can be ignored
    //  (in fact, taking the prefix into account by replacing `normalize`
    //   with `relativeInfo` breaks pos/t8177g.scala, which is probably a bug, but a tricky one...
    override def baseClasses  = normalize.baseClasses

    // similar reasoning holds here as for baseClasses
    // as another example, consider the type alias `Foo` in `class O { o => type Foo = X { val bla: o.Bar }; type Bar }`
    // o1.Foo and o2.Foo have different decls `val bla: o1.Bar` versus `val bla: o2.Bar`
    // In principle, you should only call `sym.info.decls` when you know `sym.isClass`,
    // and you should `relativize` the infos of the resulting members.
    // The latter is certainly violated in multiple spots in the codebase (the members are usually transformed correctly, though).
    override def decls: Scope = normalize.decls

    // beta-reduce, but don't do partial application -- cycles have been checked in typeRef
    override protected def normalizeImpl =
      if (typeParamsMatchArgs){
        val br = betaReduce
        if (br ne this)
          br.normalize
        else
          throw new MalformedType(pre, sym.nameString)
      }
      else if (isHigherKinded) super.normalizeImpl
      else {
        // if we are overriding a type alias in an erroneous way, don't just
        // return an ErrorType since that will result in useless error msg.
        // Instead let's try to recover from it and rely on refcheck reporting the correct error,
        // if that fails fallback to the old behaviour.
        val overriddenSym = sym.nextOverriddenSymbol
        if (overriddenSym != NoSymbol) pre.memberType(overriddenSym).normalize
        else ErrorType
      }

    // isHKSubType introduces synthetic type params so that
    // betaReduce can first apply sym.info to typeArgs before calling
    // asSeenFrom.  asSeenFrom then skips synthetic type params, which
    // are used to reduce HO subtyping to first-order subtyping, but
    // which can't be instantiated from the given prefix and class.
    //
    // this crashes pos/depmet_implicit_tpbetareduce.scala
    // appliedType(sym.info, typeArgs).asSeenFrom(pre, sym.owner)
    override def betaReduce = relativize(sym.info.resultType)

    /** scala/bug#3731, scala/bug#8177: when prefix is changed to `newPre`, maintain consistency of prefix and sym
     *  (where the symbol refers to a declaration "embedded" in the prefix).
     *
     *  @return newSym so that `newPre` binds `sym.name` to `newSym`,
     *                  to remain consistent with `pre` previously binding `sym.name` to `sym`.
     *
     *  `newSym` and `sym` are conceptually the same symbols, but some change to our `prefix`
     *  got them out of whack. (Usually triggered by substitution or `asSeenFrom`.)
     *  The only kind of "binds" we consider is where `prefix` (or its underlying type)
     *  is a refined type that declares `sym` (since the old prefix was discarded,
     *  the old symbol is now stale and we should update it, like in `def rebind`,
     *  except this is not for overriding symbols -- a vertical move -- but a "lateral" change.)
     *
     *  The reason for this hack is that substitution and asSeenFrom clone RefinedTypes and
     *  their members, without updating the potential references to those members -- here, we aim to patch
     *  this up, so that: when changing a TypeRef(pre, sym, args) to a TypeRef(pre', sym', args'), and pre
     *  embeds a symbol sym (pre is a RefinedType(_, Scope(..., sym,...)) or a SingleType with such an
     *  underlying type), make sure that we update sym' to compensate for the change of pre -> pre' (which may
     *  have created a new symbol for the one the original sym referred to)
     */
    override def coevolveSym(newPre: Type): Symbol =
      if ((pre ne newPre) && embeddedSymbol(pre, sym.name) == sym) {
        val newSym = embeddedSymbol(newPre, sym.name)
        debuglog(s"co-evolve: ${pre} -> ${newPre}, $sym : ${sym.info} -> $newSym : ${newSym.info}")
        // To deal with erroneous `preNew`, fallback via `orElse sym`, in case `preNew` does not have a decl named `sym.name`.
        newSym orElse sym
      } else sym

    override def kind = "AliasTypeRef"
  }

  // Return the symbol named `name` that's "embedded" in tp
  // This is the case if `tp` is a `T{...; type/val $name ; ...}`,
  // or a singleton type with such an underlying type.
  private def embeddedSymbol(tp: Type, name: Name): Symbol =
    // normalize to flatten nested RefinedTypes
    // don't check whether tp is a RefinedType -- it may be a ThisType of one, for example
    // TODO: check the resulting symbol is owned by the refinement class? likely an invariant...
    if (tp.typeSymbol.isRefinementClass) tp.normalize.decls lookup name
    else {
      debuglog(s"no embedded symbol $name found in ${showRaw(tp)} --> ${tp.normalize.decls lookup name}")
      NoSymbol
    }


  trait AbstractTypeRef extends NonClassTypeRef {
    require(sym.isAbstractType, sym)

    override def baseClasses = relativeInfo.baseClasses
    override def decls       = relativeInfo.decls
    override def bounds      = relativeInfo.bounds
    override def upperBound  = relativeInfo.upperBound
    override def lowerBound  = relativeInfo.lowerBound

    // TODO: this deviates from the spec "The base types of an abstract type are the base types of its upper bound."
    override protected[Types] def baseTypeSeqImpl: BaseTypeSeq = bounds.hi.baseTypeSeq prepend this
    override protected[Types] def parentsImpl: List[Type] = relativeInfo.parents

    override def kind = "AbstractTypeRef"
  }

  /** A class for named types of the form
   *    `<prefix>.<sym.name>[args]`
   *  Cannot be created directly; one should always use `typeRef`
   *  for creation. (\@M: Otherwise hashing breaks)
   *
   * \@M: a higher-kinded type is represented as a TypeRef with sym.typeParams.nonEmpty, but args.isEmpty
   */
  abstract case class TypeRef(pre: Type, sym: Symbol, args: List[Type]) extends UniqueType with TypeRefApi {
    override def mapOver(map: TypeMap): Type = {
      val pre1 = map(pre)
      val args1 = map match {
        case map: VariancedTypeMap if !args.isEmpty && ! map.variance.isInvariant =>
          val tparams = sym.typeParams
          if (tparams.isEmpty)
            args mapConserve map
          else
            map.mapOverArgs(args, tparams)
        case _ =>
          args mapConserve map
      }
      if ((pre1 eq pre) && (args1 eq args)) this
      else copyTypeRef(this, pre1, this.coevolveSym(pre1), args1)
    }
    override def foldOver(folder: TypeFolder): Unit = {
      folder(pre)
      args.foreach(folder)
    }
    private[this] var trivial: ThreeValue = UNKNOWN
    override def isTrivial: Boolean = {
      if (trivial == UNKNOWN)
        trivial = fromBoolean(!sym.isTypeParameter && pre.isTrivial && areTrivialTypes(args))
      toBoolean(trivial)
    }

    /* It only makes sense to show 2-ary type constructors infix.
     * By default we do only if it's a symbolic name. */
    override def isShowAsInfixType: Boolean =
      hasLength(args, 2) &&
        sym.getAnnotation(ShowAsInfixAnnotationClass)
         .map(_.booleanArg(0).getOrElse(true))
         .getOrElse(!Character.isUnicodeIdentifierStart(sym.decodedName.head))

    private[Types] def invalidateTypeRefCaches(): Unit = {
      parentsCache = null
      parentsPeriod = NoPeriod
      baseTypeSeqCache = null
      baseTypeSeqPeriod = NoPeriod
      normalized = null
    }
    private[reflect] var parentsCache: List[Type]      = _
    private[reflect] var parentsPeriod                 = NoPeriod
    private[reflect] var baseTypeSeqCache: BaseTypeSeq = _
    private[reflect] var baseTypeSeqPeriod             = NoPeriod
    @volatile private[this] var normalized: Type       = _

    //OPT specialize hashCode
    override final def computeHashCode = {
      import scala.util.hashing.MurmurHash3._
      var h = productSeed
      h = mix(h, pre.hashCode)
      h = mix(h, sym.hashCode)
      var length = 2
      var elems = args
      while (elems ne Nil) {
        h = mix(h, elems.head.hashCode())
        elems = elems.tail
        length += 1
      }
      finalizeHash(h, length)
    }
    //OPT specialize equals
    override final def equals(other: Any): Boolean = {
      if (this eq other.asInstanceOf[AnyRef]) true
      else other match {
        case otherTypeRef: TypeRef =>
          Objects.equals(pre, otherTypeRef.pre) &&
            sym.eq(otherTypeRef.sym) &&
            sameElementsEquals(args, otherTypeRef.args) &&
            // `ObjectTpeJavaRef` is not structurally equal to `ObjectTpe` -- they should not be collapsed by `unique`
            !(this.isInstanceOf[ObjectTpeJavaRef] || otherTypeRef.isInstanceOf[ObjectTpeJavaRef])
        case _ => false
      }
    }


    // interpret symbol's info in terms of the type's prefix and type args
    protected def relativeInfo: Type = appliedType(sym.info.asSeenFrom(pre, sym.owner), args)

    // @M: propagate actual type params (args) to `tp`, by replacing
    // formal type parameters with actual ones. If tp is higher kinded,
    // the "actual" type arguments are types that simply reference the
    // corresponding type parameters (unbound type variables)
    //
    // NOTE: for performance, as well as correctness, we do not attempt
    // to reframe trivial types in terms of our prefix and args.
    // asSeenFrom, by construction, is the identity for trivial types,
    // and substitution cannot change them either (abstract types are non-trivial, specifically because they may need to be replaced)
    // For correctness, the result for `tp == NoType` must be `NoType`,
    // if we don't shield against this, and apply instantiateTypeParams to it,
    // this would result in an ErrorType, which behaves differently during subtyping
    // (and thus on recursion, subtyping would go from false -- since a NoType is involved --
    //  to true, as ErrorType is always a sub/super type....)
    final def relativize(tp: Type): Type =
      if (tp.isTrivial) tp
      else if (args.isEmpty && (phase.erasedTypes || !isHigherKinded || isRawIfWithoutArgs(sym))) tp.asSeenFrom(pre, sym.owner)
      else {
        // The type params and type args should always match in length,
        // though a mismatch can arise when a typevar is encountered for which
        // too little information is known to determine its kind, and
        // it later turns out not to have kind *. See scala/bug#4070.
        val formals = sym.typeParams

        // If we're called with a poly type, and we were to run the `asSeenFrom`, over the entire
        // type, we can end up with new symbols for the type parameters (clones from TypeMap).
        // The subsequent substitution of type arguments would fail. This problem showed up during
        // the fix for scala/bug#8046, however the solution taken there wasn't quite right, and led to
        // scala/bug#8170.
        //
        // Now, we detect the PolyType before both the ASF *and* the substitution, and just operate
        // on the result type.
        //
        // TODO: Revisit this and explore the questions raised:
        //
        //  AM: I like this better than the old code, but is there any way the tparams would need the ASF treatment as well?
        //  JZ: I think its largely irrelevant, as they are no longer referred to in the result type.
        //      In fact, you can get away with returning a type of kind * here and the sky doesn't fall:
        //        `case PolyType(`tparams`, result) => asSeenFromInstantiated(result)`
        //      But I thought it was better to retain the kind.
        //  AM: I've been experimenting with apply-type-args-then-ASF, but running into cycles.
        //      In general, it seems iffy the tparams can never occur in the result
        //      then we might as well represent the type as a no-arg typeref.
        //  AM: I've also been trying to track down uses of transform (pretty generic name for something that
        //      does not seem that widely applicable).
        //      It's kind of a helper for computing baseType (since it tries to propagate our type args to some
        //      other type, which has to be related to this type for that to make sense).
        //
        def seenFromOwnerInstantiated(tp: Type): Type =
          tp.asSeenFrom(pre, sym.owner).instantiateTypeParams(formals, argsOrDummies)

        tp match {
          case PolyType(`formals`, result) => PolyType(formals, seenFromOwnerInstantiated(result))
          case _ => seenFromOwnerInstantiated(tp)
        }
      }

    private def argsOrDummies = if (args.isEmpty) dummyArgs else args

    final override def baseType(clazz: Symbol): Type =
      if (clazz eq sym) this
      // NOTE: this first goes to requested base type, *then* does asSeenFrom prefix & instantiates args
      else if (sym.isClass) relativize(sym.info.baseType(clazz))
      else baseTypeOfNonClassTypeRef(clazz)

    // two differences with class type basetype:
    // (1) first relativize the type, then go to the requested base type
    // (2) cache for cycle robustness
    private def baseTypeOfNonClassTypeRef(clazz: Symbol) =
      try {
        basetypeRecursions += 1
        if (basetypeRecursions >= LogPendingBaseTypesThreshold) baseTypeOfNonClassTypeRefLogged(clazz)
        else relativeInfo.baseType(clazz)
      } finally basetypeRecursions -= 1

    private def baseTypeOfNonClassTypeRefLogged(clazz: Symbol) =
      if (pendingBaseTypes add this) try relativeInfo.baseType(clazz) finally { pendingBaseTypes remove this }
      // TODO: is this optimization for AnyClass worth it? (or is it playing last-ditch cycle defense?)
      // NOTE: for correctness, it only applies for non-class types
      // (e.g., a package class should not get AnyTpe as its supertype, ever)
      else if (clazz eq AnyClass) AnyTpe
      else NoType

    // eta-expand, subtyping relies on eta-expansion of higher-kinded types
    protected def normalizeImpl: Type = if (isHigherKinded) etaExpand else super.normalize

    final override def normalize: Type = {
      // arises when argument-dependent types are approximated (see def depoly in implicits)
      if (pre eq WildcardType) WildcardType
      else if (phase.erasedTypes) normalizeImpl
      else {
        if (normalized eq null) {
          Types.this.defineNormalized(this)
        }
        normalized
      }
    }

    // TODO: test case that is compiled in a specific order and in different runs
    private[Types] final def defineNormalized() : Unit =
      // In runtime reflection, this null check is part of double-checked locking
      if (normalized eq null) normalized = normalizeImpl

    override def isGround = (
         sym.isPackageClass
      || pre.isGround && args.forall(_.isGround)
    )

    final override def etaExpand: Type = {
      // must initialise symbol, see test/files/pos/ticket0137.scala
      val tpars = initializedTypeParams
      if (tpars.isEmpty) this
      else  {
        // It's not clear which owner we should use (we don't know the context we're in),
        // but pos/t10762 shows it can't be the class (`sym`) that owns the type params,
        // as that will confuse ASF during separate compilation.
        //
        // During pickling, a pickle-local symbol (the type param) that has a non-pickle-local owner (the class),
        // will get a new owner (the pickle root, a class) assigned to it by localizedOwner.
        // This causes spurious recompilation, as well as confusion in ASF.
        // Thus, use a pickle-local term symbol owner and avoid this whole owner-rejiggering.
        val pickleLocalOwner = sym.newLocalDummy(sym.pos)

        // Since we're going to lose the information denoted by the prefix when pulling the type params
        // out for use as binders in the PolyType, we must eagerly rewrite their infos using relativize
        // to preserve that knowledge.
        val denotedLocallyOwnedTpars = cloneSymbolsAtOwnerAndModify(tpars, pickleLocalOwner, relativize)

        // @PP: use typeConstructor! #3343, #4018, #4347.
        PolyType(denotedLocallyOwnedTpars, TypeRef(pre, sym, denotedLocallyOwnedTpars map (_.typeConstructor)))
      }
    }

    // only need to rebind type aliases, as typeRef already handles abstract types
    // (they are allowed to be rebound more liberally)
    def coevolveSym(pre1: Type): Symbol = sym

    def initializedTypeParams     = sym.info.typeParams
    def typeParamsMatchArgs       = sameLength(initializedTypeParams, args)


    override def baseTypeSeqDepth = baseTypeSeq.maxDepth
    override def prefix           = pre
    override def prefixDirect     = pre
    override def termSymbol       = super.termSymbol
    override def termSymbolDirect = super.termSymbol
    override def typeArgs         = args
    override def typeOfThis       = relativize(sym.typeOfThis)
    override def typeSymbol       = sym
    override def typeSymbolDirect = sym

    override def parents: List[Type] = {
      val cache = parentsCache
      if (parentsPeriod == currentPeriod && cache != null) cache
      else {
        defineParentsOfTypeRef(this)
        parentsCache
      }
    }

    protected[Types] def parentsImpl: List[Type] = sym.info.parents map relativize

    // Since type parameters cannot occur in super types, no need to relativize before looking at base *classes*.
    // Similarly, our prefix can occur in super class types, but it cannot influence which classes those types resolve to.
    // For example, `class Outer { outer => class Inner extends outer.Foo; class Foo }`
    // `outer`'s value has no impact on which `Foo` is selected, since classes cannot be overridden.
    // besides being faster, we can't use relativeInfo because it causes cycles
    override def baseClasses      = sym.info.baseClasses

    // in principle, we should use `relativeInfo.decls`, but I believe all uses of `decls` will correctly `relativize` the individual members
    override def decls: Scope = sym.info.decls

    protected[Types] def baseTypeSeqImpl: BaseTypeSeq =
      if (sym.info.baseTypeSeq exists (_.typeSymbolDirect.isAbstractType))
        // scala/bug#8046 base type sequence might have more elements in a subclass, we can't map it element wise.
        relativize(sym.info).baseTypeSeq
      else
        // Optimization: no abstract types, we can compute the BTS of this TypeRef as an element-wise map
        //               of the BTS of the referenced symbol.
        sym.info.baseTypeSeq map relativize

    override def baseTypeSeq: BaseTypeSeq = {
      val cache = baseTypeSeqCache
      if (baseTypeSeqPeriod == currentPeriod && cache != null && cache != undetBaseTypeSeq)
        cache
      else {
        defineBaseTypeSeqOfTypeRef(this)
        if (baseTypeSeqCache == undetBaseTypeSeq)
          throw new RecoverableCyclicReference(sym)

        baseTypeSeqCache
      }
    }
    // ensure that symbol is not a local copy with a name coincidence
    private def needsPreString = (
         settings.isDebug
      || !shorthands(sym.fullName)
      || (sym.ownersIterator exists (s => !s.isClass))
    )
    private def preString  = if (needsPreString) pre.prefixString else ""
    private def argsString = if (args.isEmpty) "" else args.mkString("[", ",", "]")

    override def nameAndArgsString = typeSymbol.name.toString + argsString

    private def refinementDecls = fullyInitializeScope(decls) filter (sym => sym.isPossibleInRefinement && sym.isPublic)
    private def refinementString = (
      if (sym.isStructuralRefinement)
        refinementDecls.map(_.defString).mkString("{", "; ", "}")
      else ""
    )
    protected def finishPrefix(rest: String) = (
      if (sym.isInitialized && sym.isAnonymousClass && !phase.erasedTypes)
        parentsString(sym.info.parents) + refinementString
      else rest
      )

    private def noArgsString = finishPrefix(preString + sym.nameString)
    private def tupleTypeString: String = args match {
      case Nil        => noArgsString
      case arg :: Nil => s"($arg,)"
      case _          => args.mkString("(", ", ", ")")
    }
    private def infixTypeString: String = {
      /* SLS 3.2.8: all infix types have the same precedence.
       * In A op B op' C, op and op' need the same associativity.
       * Therefore, if op is left associative, anything on its right
       * needs to be parenthesized if it's an infix type, and vice versa. */
      // we should only get here after `isShowInfixType` says we have 2 args
      val l :: r :: Nil = args: @unchecked

      val isRightAssoc = typeSymbol.decodedName endsWith ":"

      val lstr = if (isRightAssoc && l.isShowAsInfixType) s"($l)" else l.toString

      val rstr = if (!isRightAssoc && r.isShowAsInfixType) s"($r)" else r.toString

      s"$lstr ${sym.decodedName} $rstr"
    }
    private def customToString = sym match {
      case RepeatedParamClass | JavaRepeatedParamClass => args.head.toString + "*"
      case ByNameParamClass if !args.isEmpty           => "=> " + args.head
      case _ if isFunctionTypeDirect(this)             =>
          // Aesthetics: printing Function1 as T => R rather than (T) => R
          // ...but only if it's not a tuple, so ((T1, T2)) => R is distinguishable
          // from (T1, T2) => R.
          unspecializedTypeArgs(this) match {
            // See neg/t588 for an example which arrives here - printing
            // the type of a Function1 after erasure.
            case Nil => noArgsString
            case in :: out :: Nil if !isTupleTypeDirect(in) =>
              // A => B => C should be (A => B) => C or A => (B => C).
              // Also if A is byname, then we want (=> A) => B because => is right associative and => A => B
              // would mean => (A => B) which is a different type
              val in_s  = if (isFunctionTypeDirect(in) || isByNameParamType(in)) "(" + in + ")" else "" + in
              val out_s = if (isFunctionTypeDirect(out)) "(" + out + ")" else "" + out
              in_s + " => " + out_s
            case xs =>
              xs.init.mkString("(", ", ", ")") + " => " + xs.last
          }
        case _ if isShowAsInfixType                    => infixTypeString
        case _ if isTupleTypeDirect(this)              => tupleTypeString
        case _ if sym.isAliasType && (this ne dealias) && prefixChain.exists(_.termSymbol.isSynthetic)
                                                       => "" + dealias
        case _                                         => ""
    }
    override def safeToString = {
      val custom = if (settings.isDebug) "" else customToString
      if (custom != "") custom
      else finishPrefix(preString + sym.nameString + argsString)
    }
    override def prefixString = "" + (
      if (settings.isDebug)
        super.prefixString
      else if (sym.isOmittablePrefix)
        ""
      else if (sym.isPackageClass || sym.isPackageObjectOrClass)
        sym.skipPackageObject.fullName + "."
      else if (isStable && nme.isSingletonName(sym.name))
        tpnme.dropSingletonName(sym.name).toString + "."
      else
        super.prefixString
    )
    // Suppressing case class copy method which risks subverting our single point of creation.
    @deprecated("Suppressing case class copy method", since="forever")
    @unused private def copy = null
    override def kind = "TypeRef"
  }

  // No longer defined as anonymous classes in `object TypeRef` to avoid an unnecessary outer pointer.
  private final class AliasArgsTypeRef(pre: Type, sym: Symbol, args: List[Type]) extends ArgsTypeRef(pre, sym, args) with AliasTypeRef
  private final class AbstractArgsTypeRef(pre: Type, sym: Symbol, args: List[Type]) extends ArgsTypeRef(pre, sym, args) with AbstractTypeRef
  private final class ClassArgsTypeRef(pre: Type, sym: Symbol, args: List[Type]) extends ArgsTypeRef(pre, sym, args)
  private final class AliasNoArgsTypeRef(pre: Type, sym: Symbol) extends NoArgsTypeRef(pre, sym) with AliasTypeRef
  private final class AbstractNoArgsTypeRef(pre: Type, sym: Symbol) extends NoArgsTypeRef(pre, sym) with AbstractTypeRef
  private final class ClassNoArgsTypeRef(pre: Type, sym: Symbol) extends NoArgsTypeRef(pre, sym) {
    override def contains(sym0: Symbol): Boolean = (sym eq sym0) || pre.contains(sym0)
  }

  /** Expose ObjectTpeJavaRef so we can create a non-uniqued ObjectTpeJava
    * (using a type test rather than `eq`, which causes cycles).
    *
    * NOTE:
    *   - definitions.ObjectTpe is forced first, so that it ends up in the unique cache.
    *   - the created TypeRef is structurally equal to ObjectTpe, but with its own identity
    *   - we don't want the TypeRef we create here to be unique'd
    */
  private[internal] final class ObjectTpeJavaRef extends NoArgsTypeRef(definitions.ObjectTpe.prefix, definitions.ObjectClass) {
    override def contains(sym0: Symbol): Boolean = (sym eq sym0) || pre.contains(sym0)
  }

  object TypeRef extends TypeRefExtractor {
    def apply(pre: Type, sym: Symbol, args: List[Type]): Type = unique({
      if (args ne Nil) {
        if (sym.isAliasType)              new AliasArgsTypeRef(pre, sym, args)
        else if (sym.isAbstractType)      new AbstractArgsTypeRef(pre, sym, args)
        else                              new ClassArgsTypeRef(pre, sym, args)
      }
      else {
        if (sym.isAliasType)              new AliasNoArgsTypeRef(pre, sym)
        else if (sym.isAbstractType)      new AbstractNoArgsTypeRef(pre, sym)
        else if (sym.isRefinementClass)   new RefinementTypeRef(pre, sym)
        else if (sym.isPackageClass)      new PackageTypeRef(pre, sym)
        else if (sym.isModuleClass)       new ModuleTypeRef(pre, sym)
        else                              new ClassNoArgsTypeRef(pre, sym)
      }
    })
  }

  protected def defineNormalized(tr: TypeRef): Unit = {
    tr.defineNormalized()
  }

  protected def defineParentsOfTypeRef(tpe: TypeRef) = {
    val period = tpe.parentsPeriod
    if (period != currentPeriod) {
      tpe.parentsPeriod = currentPeriod
      if (!isValidForBaseClasses(period)) {
        tpe.parentsCache = tpe.parentsImpl
      } else if (tpe.parentsCache == null) { // seems this can happen if things are corrupted enough, see #2641
        tpe.parentsCache = List(AnyTpe)
      }
    }
  }

  protected def defineBaseTypeSeqOfTypeRef(tpe: TypeRef) = {
    val period = tpe.baseTypeSeqPeriod
    if (period != currentPeriod) {
      tpe.baseTypeSeqPeriod = currentPeriod
      if (!isValidForBaseClasses(period)) {
        if (settings.areStatisticsEnabled) statistics.incCounter(typerefBaseTypeSeqCount)
        val start = if (settings.areStatisticsEnabled) statistics.pushTimer(typeOpsStack, baseTypeSeqNanos) else null
        try {
          tpe.baseTypeSeqCache = undetBaseTypeSeq
          tpe.baseTypeSeqCache = tpe.baseTypeSeqImpl
        } finally {
          if (settings.areStatisticsEnabled) statistics.popTimer(typeOpsStack, start)
        }
      }
    }
    //Console.println(s"baseTypeSeq(${tpe.typeSymbol}) = ${tpe.baseTypeSeqCache.toList}") //DEBUG
    if (tpe.baseTypeSeqCache == undetBaseTypeSeq)
      throw new TypeError(s"illegal cyclic inheritance involving ${tpe.sym}")
  }

  /** A class representing a method type with parameters.
   *  Note that a parameterless method is represented by a NullaryMethodType:
   *
   *    def m(): Int        MethodType(Nil, Int)
   *    def m: Int          NullaryMethodType(Int)
   */
  case class MethodType(override val params: List[Symbol],
                        override val resultType: Type) extends Type with MethodTypeApi {

    private[this] var trivial: ThreeValue = UNKNOWN
    override def isTrivial: Boolean = {
      if (trivial == UNKNOWN) trivial = fromBoolean(isTrivialResult && areTrivialParams)
      toBoolean(trivial)
    }

    private def isTrivialResult =
      resultType.isTrivial && (resultType eq resultType.withoutAnnotations)

    private def areTrivialParams: Boolean =
      if (params.isEmpty) true else {
        val len = params.length
        val paramsTpes: Array[Type] = new Array[Type](len)

        // returns the result of `params.forall(_.tpe.isTrivial))`
        // along the way, it loads each param' tpe into array
        def forallIsTrivial: Boolean = {
          var res = true
          var pps = params
          var ix = 0
          while(res && ix < len){
            paramsTpes(ix) = pps.head.tpe
            res = paramsTpes(ix).isTrivial
            pps = pps.tail
            ix += 1
          }
          res
        }

        def typeContains(pcc: ContainsCollector, tp: Type): Boolean = {
          pcc.result = false
          pcc.collect(tp)
        }

        // Imperative rewrite of paramsTpes.exists( typeContains(pcc, _) )
        def anyTypeContains(pcc: ContainsCollector): Boolean = {
          var existsContains = false
          var tpeIx = 0
          while(tpeIx < len && !existsContains){
            existsContains = typeContains(pcc, paramsTpes(tpeIx) )
            tpeIx += 1
          }
          existsContains
        }

        @tailrec
        def forallParamsNoTypeContains(params: List[Symbol]): Boolean =
          params match {
            case Nil => true
            case pp :: pps =>
              val pcc = new ContainsCollector(pp)
              !typeContains(pcc, resultType) && ! anyTypeContains(pcc) &&
              forallParamsNoTypeContains(pps)
          }

        forallIsTrivial && forallParamsNoTypeContains(params)
      }

    def isImplicit = (params ne Nil) && params.head.isImplicit

    override def paramSectionCount: Int = resultType.paramSectionCount + 1

    override def paramss: List[List[Symbol]] = params :: resultType.paramss

    override def paramTypes = mapList(params)(_.tpe) // OPT use mapList rather than .map

    final def resultTypeOwnParamTypes: Type =
      if (isTrivial || phase.erasedTypes) resultType
      else resultType0(paramTypes)

    override def resultType(actuals: List[Type]) =
      if (isTrivial || phase.erasedTypes) resultType
      else resultType0(actuals)

    private def resultType0(actuals: List[Type]): Type =
      if (/*isDependentMethodType &&*/ sameLength(actuals, params)) {
        val idm = new InstantiateDependentMap(params, actuals)
        val res = idm(resultType).deconst
        existentialAbstraction(idm.existentialsNeeded, res)
      }
      else existentialAbstraction(params, resultType)

    private[this] var isdepmeth: ThreeValue = UNKNOWN
    override def isDependentMethodType: Boolean = {
      if (isdepmeth == UNKNOWN) isdepmeth = fromBoolean(IsDependentCollector.collect(resultType.dealias))
      toBoolean(isdepmeth)
    }

    // implicit args can only be depended on in result type:
    //TODO this may be generalised so that the only constraint is dependencies are acyclic
    def approximate: MethodType = MethodType(params, resultApprox)

    //Format (a: A)(b: B)(implicit c: C, d: D): E
    override def safeToString = {
      s"${paramString(this)}${
        resultType match { case _: MethodType => "" case _ => ": "}
      }$resultType"
    }

    override def cloneInfo(owner: Symbol) = {
      val vparams = cloneSymbolsAtOwner(params, owner)
      copyMethodType(this, vparams, resultType.substSym(params, vparams).cloneInfo(owner))
    }

    override def atOwner(owner: Symbol) =
      if (!allSymbolsHaveOwner(params, owner) || (resultType.atOwner(owner) ne resultType))
        cloneInfo(owner)
      else
        this

    override def kind = "MethodType"
    override def mapOver(map: TypeMap): Type = {
      val params1 = map match {
        case vtm: VariancedTypeMap => vtm.flipped(vtm.mapOver(params))
        case _ => map.mapOver(params)
      }
      val result1 = map(resultType)
      if ((params1 eq params) && (result1 eq resultType)) this
      else copyMethodType(this, params1, result1.substSym(params, params1))
    }
    override def foldOver(folder: TypeFolder): Unit = {
      folder.foldOver(params)
      folder(resultType)
    }
  }

  object MethodType extends MethodTypeExtractor

  // Nullary not nilary, i.e., parameterlistless not merely parameterless
  case class NullaryMethodType(override val resultType: Type) extends Type with NullaryMethodTypeApi {
    override def isTrivial = resultType.isTrivial && (resultType eq resultType.withoutAnnotations)
    override def prefix: Type = resultType.prefix
    override def narrow: Type = resultType.narrow
    override def termSymbol: Symbol = resultType.termSymbol
    override def typeSymbol: Symbol = resultType.typeSymbol
    override def parents: List[Type] = resultType.parents
    override def decls: Scope = resultType.decls
    override def baseTypeSeq: BaseTypeSeq = resultType.baseTypeSeq
    override def baseTypeSeqDepth: Depth = resultType.baseTypeSeqDepth
    override def baseClasses: List[Symbol] = resultType.baseClasses
    override def baseType(clazz: Symbol): Type = resultType.baseType(clazz)
    override def safeToString: String = resultType.toString
    override def kind = "NullaryMethodType"
    override def mapOver(map: TypeMap): Type = {
      val result1 = map(resultType)
      if (result1 eq resultType) this
      else NullaryMethodType(result1)
    }
    override def foldOver(folder: TypeFolder): Unit = folder(resultType)

  }

  object NullaryMethodType extends NullaryMethodTypeExtractor

  /** A type function or the type of a polymorphic value (and thus of kind *).
   *
   * Before the introduction of NullaryMethodType, a polymorphic nullary method (e.g, def isInstanceOf[T]: Boolean)
   * used to be typed as PolyType(tps, restpe), and a monomorphic one as PolyType(Nil, restpe)
   * This is now: PolyType(tps, NullaryMethodType(restpe)) and NullaryMethodType(restpe)
   * by symmetry to MethodTypes: PolyType(tps, MethodType(params, restpe)) and MethodType(params, restpe)
   *
   * Thus, a PolyType(tps, TypeRef(...)) unambiguously indicates a type function (which results from eta-expanding a type constructor alias).
   * Similarly, PolyType(tps, ClassInfoType(...)) is a type constructor.
   *
   * A polytype is of kind * iff its resultType is a (nullary) method type.
   */
  case class PolyType(override val typeParams: List[Symbol], override val resultType: Type)
       extends Type with PolyTypeApi {
    //assert(!(typeParams contains NoSymbol), this)
    assert(!typeParams.isEmpty, this) // used to be a marker for nullary method type, illegal now (see @NullaryMethodType)

    override def paramSectionCount: Int = resultType.paramSectionCount
    override def paramss: List[List[Symbol]] = resultType.paramss
    override def params: List[Symbol] = resultType.params
    override def paramTypes: List[Type] = resultType.paramTypes
    override def parents: List[Type] = resultType.parents
    override def decls: Scope = resultType.decls
    override def termSymbol: Symbol = resultType.termSymbol
    override def typeSymbol: Symbol = resultType.typeSymbol
    override def prefix: Type = resultType.prefix
    override def baseTypeSeq: BaseTypeSeq = resultType.baseTypeSeq
    override def baseTypeSeqDepth: Depth = resultType.baseTypeSeqDepth
    override def baseClasses: List[Symbol] = resultType.baseClasses
    override def baseType(clazz: Symbol): Type = resultType.baseType(clazz)
    override def narrow: Type = resultType.narrow

    // scala/bug#9475: PolyTypes with dependent method types are still dependent
    override def isDependentMethodType = resultType.isDependentMethodType

    /** @M: typeDefSig wraps a TypeBounds in a PolyType
     *  to represent a higher-kinded type parameter
     *  wrap lo&hi in polytypes to bind variables
     */
    override def lowerBound: Type = typeFun(typeParams, resultType.lowerBound)
    override def upperBound: Type = typeFun(typeParams, resultType.upperBound)

    override def isHigherKinded = !typeParams.isEmpty

    override def safeToString = typeParamsString(this) + resultType

    override def cloneInfo(owner: Symbol) = {
      val tparams = cloneSymbolsAtOwner(typeParams, owner)
      PolyType(tparams, resultType.substSym(typeParams, tparams).cloneInfo(owner))
    }

    override def atOwner(owner: Symbol) =
      if (!allSymbolsHaveOwner(typeParams, owner) || (resultType.atOwner(owner) ne resultType))
        cloneInfo(owner)
      else
        this

    override def kind = "PolyType"
    override def mapOver(map: TypeMap): Type = {
      val tparams1 = map match {
        case vtm: VariancedTypeMap => vtm.flipped(vtm.mapOver(typeParams))
        case _ => map.mapOver(typeParams)
      }
      val result1 = map(resultType)
      if ((tparams1 eq typeParams) && (result1 eq resultType)) this
      else PolyType(tparams1, result1.substSym(typeParams, tparams1))
    }
    override def foldOver(folder: TypeFolder): Unit = {
      folder.foldOver(typeParams)
      folder(resultType)
    }

  }

  object PolyType extends PolyTypeExtractor

  /** A creator for existential types which flattens nested existentials.
   */
  @tailrec
  final def newExistentialType(quantified: List[Symbol], underlying: Type): Type =
    if (quantified.isEmpty) underlying
    else underlying match {
      case ExistentialType(qs, restpe) => newExistentialType(quantified ::: qs, restpe)
      case _                           => ExistentialType(quantified, underlying)
    }

  case class ExistentialType(quantified: List[Symbol],
                             override val underlying: Type) extends RewrappingTypeProxy with ExistentialTypeApi
  {
    override protected def rewrap(newtp: Type) = existentialAbstraction(quantified, newtp)

    override def isTrivial = false
    override def bounds = TypeBounds(lowerBound, upperBound)
    override def lowerBound = maybeRewrap(underlying.lowerBound)
    override def upperBound = maybeRewrap(underlying.upperBound)
    override def parents = underlying.parents map maybeRewrap
    override def prefix = maybeRewrap(underlying.prefix)
    override def typeArgs = underlying.typeArgs map maybeRewrap
    override def params = underlying.params mapConserve { param =>
      val tpe1 = rewrap(param.tpeHK)
      if (tpe1 eq param.tpeHK) param else param.cloneSymbol.setInfo(tpe1)
    }
    override def paramTypes = underlying.paramTypes map maybeRewrap
    override def instantiateTypeParams(formals: List[Symbol], actuals: List[Type]) = {
//      maybeRewrap(underlying.instantiateTypeParams(formals, actuals))

      val quantified1 = new SubstTypeMap(formals, actuals) mapOver quantified
      val underlying1 = underlying.instantiateTypeParams(formals, actuals)
      if ((quantified1 eq quantified) && (underlying1 eq underlying)) this
      else existentialAbstraction(quantified1, underlying1.substSym(quantified, quantified1))

    }
    override def baseType(clazz: Symbol) = maybeRewrap(underlying.baseType(clazz))
    override def baseTypeSeq = underlying.baseTypeSeq map maybeRewrap
    override def isHigherKinded = false

    // TODO: check invariant that all quantifiers have the same (existing) owner
    private def quantifierOwner = quantified collectFirst { case q if q.owner.exists => q.owner } getOrElse NoSymbol

    // Is this existential of the form: T[Q1, ..., QN] forSome { type Q1 >: L1 <: U1, ..., QN >: LN <: UN}
    private def isStraightApplication = (quantified corresponds underlying.typeArgs){ (q, a) => q.tpe =:= a }

    /** [scala/bug#6169, scala/bug#8197 -- companion to scala/bug#1786]
     *
     * Approximation to improve the bounds of a Java-defined existential type,
     * based on the bounds of the type parameters of the quantified type
     * In Scala syntax, given a java-defined class C[T <: String], the existential type C[_]
     * is improved to C[_ <: String] before skolemization, which captures (get it?) what Java does:
     * enter the type parameters' bounds into the context when checking subtyping/type equality of existential types
     *
     * Also tried doing this once during class file parsing or when creating the existential type,
     * but that causes cyclic errors because it happens too early.
     *
     * NOTE: we're only modifying the skolems to avoid leaking the sharper bounds to `quantified` (scala/bug#8283)
     *
     * TODO: figure out how to do this earlier without running into cycles, so this can subsume the fix for scala/bug#1786
     */
    override def skolemizeExistential(owner0: Symbol, origin: AnyRef) = {
      val owner = owner0 orElse quantifierOwner

      // do this here because it's quite close to what Java does:
      // when checking subtyping/type equality, enter constraints
      // derived from the existentially quantified type into the typing environment
      // (aka \Gamma, which tracks types for variables and constraints/kinds for types)
      // as a nice bonus, delaying this until we need it avoids cyclic errors
      def tpars = underlying.typeSymbolDirect.initialize.typeParams

      def newSkolem(quant: Symbol) = owner.newExistentialSkolem(quant, origin)
      def newSharpenedSkolem(quant: Symbol, tparam: Symbol): Symbol = {
        def emptyBounds(sym: Symbol) = sym.info.bounds.isEmptyBounds

        // avoid creating cycles [pos/t2940] that consist of an existential quantifier's
        // bounded by an existential type that unhygienically has that quantifier as its own quantifier
        // (TODO: clone latter existential with fresh quantifiers -- not covering this case for now)
        val canSharpen = (
             emptyBounds(quant) && !emptyBounds(tparam)
          && existentialsInType(tparam.info).forall(et => !quantified.contains(et))
        )

        val skolemInfo = if (!canSharpen) quant.info else tparam.info.substSym(tpars, quantified)

        owner.newExistentialSkolem(quant.name.toTypeName, skolemInfo, quant.flags, quant.pos, origin)
      }

      val canSharpenBounds = (underlying.typeSymbol.isJavaDefined || sharperSkolems) && isStraightApplication

      if (canSharpenBounds) deriveType2(quantified, tpars, newSharpenedSkolem)(underlying)
      else deriveType(quantified, newSkolem)(underlying)
    }

    private def wildcardArgsString(qset: Set[Symbol], args: List[Type]): List[String] = args map {
      case TypeRef(_, sym, _) if (qset contains sym) =>
        "_"+sym.infoString(sym.info)
      case arg =>
        arg.toString
    }

    override def nameAndArgsString: String = underlying match {
      case TypeRef(_, sym, args) if !settings.isDebug && isRepresentableWithWildcards =>
        sym.name.toString + wildcardArgsString(quantified.toSet, args).mkString("[", ",", "]")
      case TypeRef(_, sym, args) =>
        sym.name.toString + args.mkString("[", ",", "]") + existentialClauses
      case _ => underlying.typeSymbol.name.toString + existentialClauses
    }

    private def existentialClauses = {
      val str = quantified.map(_.existentialToString).mkString(" forSome { ", "; ", " }")
      if (settings.explaintypes.value) "(" + str + ")" else str
    }

    /** An existential can only be printed with wildcards if:
     *   - the underlying type is a typeref
     *   - every quantified variable appears at most once as a type argument and
     *     nowhere inside a type argument
     *   - no quantified type argument contains a quantified variable in its bound
     *   - the typeref's symbol is not itself quantified
     *   - the prefix is not quantified
     */
    def isRepresentableWithWildcards = {
      val qset = quantified.toSet
      underlying match {
        case _: RefinementTypeRef => false
        case TypeRef(pre, sym, args) =>
          def isQuantified(tpe: Type): Boolean = {
            (tpe exists (t => qset contains t.typeSymbol)) ||
            tpe.typeSymbol.isRefinementClass && (tpe.parents exists isQuantified)
          }
          val (wildcardArgs, otherArgs) = args partition (arg => qset contains arg.typeSymbol)
          wildcardArgs.toSet.size == wildcardArgs.size &&
          !(otherArgs exists (arg => isQuantified(arg))) &&
          !(wildcardArgs exists (arg => isQuantified(arg.typeSymbol.info.bounds))) &&
          !(qset contains sym) &&
          !isQuantified(pre)
        case _ => false
      }
    }

    override def safeToString: String = underlying match {
      case TypeRef(pre, sym, args) if !settings.isDebug && isRepresentableWithWildcards =>
        val ref = typeRef(pre, sym, Nil).toString
        val wildcards = wildcardArgsString(quantified.toSet, args)
        if (wildcards.isEmpty) ref else ref + wildcards.mkString("[", ", ", "]")
      case MethodType(_, _) | NullaryMethodType(_) | PolyType(_, _) =>
        "(" + underlying + ")" + existentialClauses
      case _ =>
        underlying.toString + existentialClauses
    }

    override def cloneInfo(owner: Symbol) =
      createFromClonedSymbolsAtOwner(quantified, owner, underlying)(newExistentialType)

    override def atOwner(owner: Symbol) =
      if (!allSymbolsHaveOwner(quantified, owner)) cloneInfo(owner) else this

    override def kind = "ExistentialType"

    def withTypeVars(op: Type => Boolean): Boolean = withTypeVars(op, AnyDepth)

    def withTypeVars(op: Type => Boolean, depth: Depth): Boolean = {
      val quantifiedFresh = cloneSymbols(quantified)
      val tvars = quantifiedFresh map (tparam => TypeVar(tparam))
      val underlying1 = underlying.instantiateTypeParams(quantified, tvars) // fuse subst quantified -> quantifiedFresh -> tvars
      op(underlying1) && {
        solve(tvars, quantifiedFresh, (_ => Invariant), upper = false, depth) &&
        isWithinBounds(NoPrefix, NoSymbol, quantifiedFresh, tvars map (_.inst))
      }
    }
    override def mapOver(map: TypeMap): Type = {
      val quantified1 = map.mapOver(quantified)
      val underlying1 = map(underlying)
      if ((quantified1 eq quantified) && (underlying1 eq underlying)) this
      else newExistentialType(quantified1, underlying1.substSym(quantified, quantified1))
    }
    override def foldOver(folder: TypeFolder): Unit = {
      folder.foldOver(quantified)
      folder(underlying)
    }
  }

  object ExistentialType extends ExistentialTypeExtractor

  /** A class containing the alternatives and type prefix of an overloaded symbol.
   *  Not used after phase `typer`.
   */
  case class OverloadedType(pre: Type, alternatives: List[Symbol]) extends Type {
    override def prefix: Type = pre
    override def safeToString =
      (alternatives map pre.memberType).mkString("", " <and> ", "")
    override def kind = "OverloadedType"
    override def mapOver(map: TypeMap): Type = {
      val pre1 = if (pre.isInstanceOf[ClassInfoType]) pre else map(pre)
      if (pre1 eq pre) this
      else OverloadedType(pre1, alternatives)
    }
    override def foldOver(folder: TypeFolder): Unit =
      if (! pre.isInstanceOf[ClassInfoType]) folder(pre)
  }

  /** The canonical creator for OverloadedTypes.
   */
  def overloadedType(pre: Type, alternatives: List[Symbol]): Type = alternatives match {
    case Nil        => NoType
    case alt :: Nil => pre memberType alt
    case _          => OverloadedType(pre, alternatives)
  }

  case class ImportType(expr: Tree) extends Type {
    override def safeToString = "ImportType("+expr+")"
  }

  /** A class remembering a type instantiation for some a set of overloaded
   *  polymorphic symbols.
   *  Not used after phase `typer`.
   */
  case class AntiPolyType(pre: Type, targs: List[Type]) extends Type {
    override def safeToString =
      pre.toString + targs.mkString("(with type arguments ", ", ", ")")

    override def memberType(sym: Symbol) = appliedType(pre.memberType(sym), targs)
    override def kind = "AntiPolyType"
    override def mapOver(map: TypeMap): Type = {
      val pre1 = map(pre)
      val targs1 = targs mapConserve map
      if ((pre1 eq pre) && (targs1 eq targs)) this
      else AntiPolyType(pre1, targs1)
    }
    override def foldOver(folder: TypeFolder): Unit = {
      folder(pre)
      targs.foreach(folder)
    }
  }

  object HasTypeMember {
    def apply(name: TypeName, tp: Type): Type = {
      val bound = refinedType(List(WildcardType), NoSymbol)
      val bsym = bound.typeSymbol.newAliasType(name)
      bsym setInfo tp
      bound.decls enter bsym
      bound
    }
    def unapply(tp: Type): Boolean = tp match {
      case RefinedType(List(WildcardType), scope) => scope.size == 1
      case _ => false
    }
  }

  object ArrayTypeRef {
    def unapply(tp: Type) = tp match {
      case TypeRef(_, ArrayClass, arg :: Nil) => Some(arg)
      case _                                  => None
    }
  }

  //@M
  // a TypeVar used to be a case class with only an origin and a constr
  // then, constr became mutable (to support UndoLog, I guess),
  // but pattern-matching returned the original constr0 (a bug)
  // now, pattern-matching returns the most recent constr
  object TypeVar {
    @inline final def trace[T](action: String, msg: => String)(value: T): T = {
      // Uncomment the following for a compiler that has some diagnostics about type inference
      // I doubt this is ever useful in the wild, so a recompile will be needed
//    val s = msg match {
//      case ""   => ""
//      case str  => "( " + str + " )"
//    }
//    Console.err.println("[%10s] %-25s%s".format(action, value, s))
      value
    }

    def precludesWidening(tp: Type) = tp.isStable || tp.typeSymbol.isSubClass(SingletonClass)

    def untouchable(tparam: Symbol): TypeVar                 = createTypeVar(tparam, untouchable = true)
    def apply(tparam: Symbol): TypeVar                       = createTypeVar(tparam, untouchable = false)
    def apply(origin: Type, constr: TypeConstraint): TypeVar = apply(origin, constr, Nil, Nil)
    def apply(origin: Type, constr: TypeConstraint, args: List[Type], params: List[Symbol]): TypeVar =
      createTypeVar(origin, constr, args, params, untouchable = false)

    /** This is the only place TypeVars should be instantiated.
     */
    private def createTypeVar(origin: Type, constr: TypeConstraint, args: List[Type], params: List[Symbol], untouchable: Boolean): TypeVar = {
      val tv = (
        if (args.isEmpty && params.isEmpty) {
          if (untouchable) new TypeVar(origin, constr) with UntouchableTypeVar
          else new TypeVar(origin, constr) {}
        }
        else if (args.size == params.size) {
          if (untouchable) new AppliedTypeVar(origin, constr, params, args) with UntouchableTypeVar
          else new AppliedTypeVar(origin, constr, params, args)
        }
        else if (args.isEmpty) {
          if (untouchable) new HKTypeVar(origin, constr, params) with UntouchableTypeVar
          else new HKTypeVar(origin, constr, params)
        }
        else throw new Error("Invalid TypeVar construction: " + ((origin, constr, args, params)))
      )

      trace("create", "In " + tv.originLocation)(
        tv
      )
    }
    private def createTypeVar(tparam: Symbol, untouchable: Boolean): TypeVar = {
      val constr = new TypeConstraint
      if (precludesWidening(tparam.info.upperBound)) {
        constr.stopWidening()
        constr.addHiBound(SingletonClass.typeConstructor) // TODO: why do we need the additional hi-bound? see sip23-widen
      }

      createTypeVar(tparam.typeConstructor, constr, Nil, tparam.typeParams, untouchable)
    }
  }

  /** Precondition: !params.isEmpty.  (args.nonEmpty enforced structurally.)
   */
  class HKTypeVar(
    _origin: Type,
    _constr: TypeConstraint,
    override val params: List[Symbol]
  ) extends TypeVar(_origin, _constr) {
    require(!params.isEmpty, this)
    override def isHigherKinded: Boolean = true
    override def typeParams: List[Symbol] = params
  }

  /** Precondition: `params.length == typeArgs.length > 0` (enforced structurally). */
  class AppliedTypeVar(
    _origin: Type,
    _constr: TypeConstraint,
    override val params: List[Symbol],
    override val typeArgs: List[Type]
  ) extends TypeVar(_origin, _constr) {
    require(!params.isEmpty && sameLength(params, typeArgs), this)
    override def safeToString: String = super.safeToString + typeArgs.map(_.safeToString).mkString("[", ", ", "]")
    override def setInst(tp: Type): this.type =
      super.setInst(if (isSubArgs(typeArgs, tp.typeArgs, params, Depth.AnyDepth)) tp.typeConstructor else NoType)
  }

  trait UntouchableTypeVar extends TypeVar {
    override def untouchable = true
    override def isGround = true
    override def registerTypeEquality(tp: Type, typeVarLHS: Boolean) = tp match {
      case t: TypeVar if !t.untouchable =>
        t.registerTypeEquality(this, !typeVarLHS)
      case _ =>
        super.registerTypeEquality(tp, typeVarLHS)
    }
    override def registerBound(tp: Type, isLowerBound: Boolean, isNumericBound: Boolean = false): Boolean = tp match {
      case t: TypeVar if !t.untouchable =>
        t.registerBound(this, !isLowerBound, isNumericBound)
      case _ =>
        super.registerBound(tp, isLowerBound, isNumericBound)
    }
  }

  /** A class representing a type variable: not used after phase `typer`.
   *
   *  A higher-kinded TypeVar has params (Symbols) and typeArgs (Types).
   *  A TypeVar with nonEmpty typeArgs can only be instantiated by a higher-kinded
   *  type that can be applied to those args.  A TypeVar is much like a TypeRef,
   *  except it has special logic for equality and subtyping.
   *
   *  Precondition for this class, enforced structurally: args.isEmpty && params.isEmpty.
   */
  abstract case class TypeVar(
                               origin: Type,
    var constr: TypeConstraint
  ) extends Type {

    // We don't want case class equality/hashing as TypeVar-s are mutable,
    // and TypeRefs based on them get wrongly `uniqued` otherwise. See scala/bug#7226.
    override def hashCode(): Int = System.identityHashCode(this)
    override def equals(other: Any): Boolean = this eq other.asInstanceOf[AnyRef]

    def untouchable = false   // by other typevars
    override def params: List[Symbol] = Nil
    override def typeArgs: List[Type] = Nil
    override def isHigherKinded = false

    /** The constraint associated with the variable
     *  Syncnote: Type variables are assumed to be used from only one
     *  thread. They are not exposed in api.Types and are used only locally
     *  in operations that are exposed from types. Hence, no syncing of `constr`
     *  or `encounteredHigherLevel` or `suspended` accesses should be necessary.
     */
    def instValid = constr.instValid
    def inst = constr.inst
    def instWithinBounds = constr.instWithinBounds
    override def isGround = instValid && inst.isGround

    /** The variable's skolemization level */
    val level = skolemizationLevel

    /** Applies this TypeVar to type arguments, if arity matches.
     *
     * Different applications of the same type constructor variable `?CC`,
     * e.g. `?CC[Int]` and `?CC[String]`, are modeled as distinct instances of `TypeVar`
     * that share a `TypeConstraint`, so that the comparisons `?CC[Int] <:< List[Int]`
     * and `?CC[String] <:< Iterable[String]` result in `?CC` being upper-bounded by `List` and `Iterable`.
     *
     * Applying the wrong number of type args results in a TypeVar whose instance is set to `ErrorType`.
     */
    def applyArgs(newArgs: List[Type]): TypeVar = (
      if (newArgs.isEmpty && typeArgs.isEmpty)
        this
      else if (newArgs.size == params.size) {
        val tv = TypeVar(origin, constr, newArgs, params)
        tv.linkSuspended(this)
        TypeVar.trace("applyArgs", s"In $originLocation, apply args ${newArgs.mkString(", ")} to $originName")(
          tv
        )
      }
      else
        TypeVar(typeSymbol).setInst(ErrorType)
    )
    // newArgs.length may differ from args.length (could've been empty before)
    //
    // !!! @PP - I need an example of this, since this exception never triggers
    // even though I am requiring the size match.
    //
    // example: when making new typevars, you start out with C[A], then you replace C by ?C, which should yield ?C[A], then A by ?A, ?C[?A]
    // we need to track a TypeVar's arguments, and map over them (see TypeMap::mapOver)
    // TypeVars get applied to different arguments over time (in asSeenFrom)
     // -- see pos/tcpoly_infer_implicit_tuplewrapper.scala
    // thus: make new TypeVar's for every application of a TV to args,
    // inference may generate several TypeVar's for a single type parameter that must be inferred,
    // only one of them is in the set of tvars that need to be solved, but
    // they share the same TypeConstraint instance

    // <region name="constraint mutators + undoLog">
    // invariant: before mutating constr, save old state in undoLog
    // (undoLog is used to reset constraints to avoid piling up unrelated ones)
    def setInst(tp: Type): this.type =
      if (tp ne this) {
        undoLog record this
        constr.inst = TypeVar.trace("setInst", s"In $originLocation, $originName=$tp")(
          tp
        )
        this
      } else {
        log(s"TypeVar cycle: called setInst passing $this to itself.")
        this
      }

    def addLoBound(tp: Type, isNumericBound: Boolean = false): Unit = {
      assert(tp != this, tp) // implies there is a cycle somewhere (?)
      //println("addLoBound: "+(safeToString, debugString(tp))) //DEBUG
      if (!sharesConstraints(tp)) {
        undoLog record this
        constr.addLoBound(tp, isNumericBound)
      }
    }

    def addHiBound(tp: Type, isNumericBound: Boolean = false): Unit = {
      // assert(tp != this)
      //println("addHiBound: "+(safeToString, debugString(tp))) //DEBUG
      if (!sharesConstraints(tp)) {
        undoLog record this
        constr.addHiBound(tp, isNumericBound)
      }
    }
    // </region>

    // ignore subtyping&equality checks while true -- see findMember
    // OPT: This could be Either[TypeVar, Boolean], but this encoding was chosen instead to save allocations.
    private[this] var _suspended: Type = ConstantFalse
    @tailrec
    private[Types] final def suspended: Boolean = (_suspended: @unchecked) match {
      case ConstantFalse => false
      case ConstantTrue  => true
      case tv: TypeVar   => tv.suspended
    }

    /** `AppliedTypeVar`s share the same `TypeConstraint` with the `HKTypeVar` that it was spawned from.
     *   A type inference session can also have more than one ATV.
     *   If we don't detect that, we end up with "cyclic constraint" when we try to instantiate type parameters
     *   after solving in, pos/t8237
     */
    protected final def sharesConstraints(other: Type): Boolean = other match {
      case other: TypeVar => constr == other.constr // scala/bug#8237 avoid cycles. Details in pos/t8237.scala
      case PolyType(_, other: TypeVar) => constr == other.constr
      case _ => false
    }
    private[Types] def suspended_=(b: Boolean): Unit = _suspended = if (b) ConstantTrue else ConstantFalse
    // scala/bug#7785 Link the suspended attribute of a TypeVar created in, say, a TypeMap (e.g. AsSeenFrom) to its originator
    private[Types] def linkSuspended(origin: TypeVar): Unit = _suspended = origin

    /** Called when a TypeVar is involved in a subtyping check.  Result is whether
     *  this TypeVar could plausibly be a [super/sub]type of argument `tp` and if so,
     *  tracks tp as a [lower/upper] bound of this TypeVar.
     *
     *  if (isLowerBound)   this typevar could be a subtype, track tp as a lower bound
     *  if (!isLowerBound)  this typevar could be a supertype, track tp as an upper bound
     *
     *  If isNumericBound is true, the subtype check is performed with weak_<:< instead of <:<.
     */
    def registerBound(tp: Type, isLowerBound: Boolean, isNumericBound: Boolean = false): Boolean = {
      // println("regBound: "+(safeToString, debugString(tp), isLowerBound)) //@MDEBUG
      if (isLowerBound) assert(tp != this, "Lower bound of this type")

      // side effect: adds the type to upper or lower bounds
      def addBound(tp: Type): Unit = {
        if (isLowerBound) addLoBound(tp, isNumericBound)
        else addHiBound(tp, isNumericBound)
      }
      // swaps the arguments if it's an upper bound
      def checkSubtype(tp1: Type, tp2: Type) = {
        val lhs = if (isLowerBound) tp1 else tp2
        val rhs = if (isLowerBound) tp2 else tp1

        if (isNumericBound) lhs weak_<:< rhs
        else lhs <:< rhs
      }

      /*  Simple case: type arguments can be ignored, because either this typevar has
       *  no type parameters, or we are comparing to Any/Nothing.
       *
       *  The latter condition is needed because HK unification is limited to constraints of the shape
       *  {{{
       *    TC1[T1,..., TN] <: TC2[T'1,...,T'N]
       *  }}}
       *  which would preclude the following important constraints:
       *  {{{
       *    Nothing <: ?TC[?T]
       *    ?TC[?T] <: Any
       *  }}}
       */
      def unifySimple = {
        // scala/bug#7126 if we register some type alias `T=Any`, we can later end
        // with malformed types like `T[T]` during type inference in
        // `handlePolymorphicCall`. No such problem if we register `Any`.
        if (typeIsNothing(tp)) { // kind-polymorphic
          addBound(NothingTpe)
          true
        } else if(typeIsAnyExactly(tp)) { // kind-polymorphic
          addBound(AnyTpe)
          true
        } else if (params.isEmpty) {
          addBound(tp)
          true
        } else false
      }

      /*  Full case: involving a check of the form
       *  {{{
       *    TC1[T1,..., TN] <: TC2[T'1,...,T'N]
       *  }}}
       *  Checks subtyping of higher-order type vars, and uses variances as defined in the
       *  type parameter we're trying to infer (the result will be confidence-checked later).
       */
      def unifyFull(tpe: Type): Boolean = {
        def unifiableKinds(lhs: List[Symbol], rhs: List[Symbol]): Boolean =
          sameLength(lhs, rhs) && !exists2(lhs, rhs)((l, r) => !unifiableKinds(l.typeParams, r.typeParams))

        def unifySpecific(tp: Type) = {
          val tpTypeArgs = tp.typeArgs
          val numCaptured = tpTypeArgs.length - typeArgs.length
          val tpSym = tp.typeSymbolDirect
          val abstractedTypeParams = tpSym.typeParams.drop(numCaptured)
          if(!unifiableKinds(typeSymbolDirect.typeParams, abstractedTypeParams)) false
          else {
            if (numCaptured == 0) {
              val lhs = if (isLowerBound) tpTypeArgs else typeArgs
              val rhs = if (isLowerBound) typeArgs else tpTypeArgs
              // This is a higher-kinded type var with same arity as tp.
              // If so (see scala/bug#7517), side effect: adds the type constructor itself as a bound.
              isSubArgs(lhs, rhs, params, AnyDepth) && {addBound(tp.typeConstructor); true}
            } else if (numCaptured > 0) {
              // Simple algorithm as suggested by Paul Chiusano in the comments on scala/bug#2712
              //
              //   https://github.com/scala/bug/issues/2712#issuecomment-292374655
              //
              // Treat the type constructor as curried and partially applied, we treat a prefix
              // as constants and solve for the suffix. For the example in the ticket, unifying
              // M[A] with Int => Int this unifies as,
              //
              //   M[t] = [t][Int => t]  --> abstract on the right to match the expected arity
              //   A = Int               --> capture the remainder on the left
              //
              // A more "natural" unifier might be M[t] = [t][t => t]. There's lots of scope for
              // experimenting with alternatives here.
              val abstractedArgs = tpTypeArgs.drop(numCaptured)

              val (lhs, rhs) =
                if (isLowerBound) (abstractedArgs, typeArgs)
                else (typeArgs, abstractedArgs)

              isSubArgs(lhs, rhs, params, AnyDepth) && {
                val captured = tpTypeArgs.take(numCaptured)
                val clonedParams = abstractedTypeParams.map(_.cloneSymbol(tpSym))
                addBound(PolyType(clonedParams, appliedType(tp.typeConstructor, captured ++ clonedParams.map(_.tpeHK))))
                true
              }
            } else false
          }
        }
        // The type with which we can successfully unify can be hidden
        // behind singleton types and type aliases.
        tpe.dealiasWidenChain exists unifySpecific
      }

      // There's a <: test taking place right now, where tp is a concrete type and this is a typevar
      // attempting to satisfy that test. Either the test will be unsatisfiable, in which case
      // registerBound will return false; or the upper or lower bounds of this type var will be
      // supplemented with the type being tested against.
      //
      // Eventually the types which have accumulated in the upper and lower bounds will be lubbed
      // (resp. glbbed) to instantiate the typevar.
      //
      // The only types which are eligible for unification are those with the same number of
      // typeArgs as this typevar, or Any/Nothing, which are kind-polymorphic. For the upper bound,
      // any parent or base type of `tp` may be tested here (leading to a corresponding relaxation
      // in the upper bound.) The universe of possible glbs, being somewhat more infinite, is not
      // addressed here: all lower bounds are retained and their intersection calculated when the
      // bounds are solved.
      //
      // In a side-effect free universe, checking tp and tp.parents before checking tp.baseTypeSeq
      // would be pointless. In this case, each check we perform causes us to lose specificity: in
      // the end the best we'll do is the least specific type we tested against, since the typevar
      // does not see these checks as "probes" but as requirements to fulfill.
      // TODO: can the `suspended` flag be used to poke around without leaving a trace?
      //
      // So the strategy used here is to test first the type, then the direct parents, and finally
      // to fall back on the individual base types. This warrants eventual re-examination.

      // AM: I think we could use the `suspended` flag to avoid side-effecting during unification
      if (suspended)         // constraint accumulation is disabled
        checkSubtype(tp, origin)
      else if (instValid)  // type var is already set
        checkSubtype(tp, inst)
      else unrelatable(tp) match {
        case Nil => unifySimple || unifyFull(tp) || (
          // only look harder if our gaze is oriented toward Any
          isLowerBound && (
            (tp.parents exists unifyFull) || (
              // @PP: Is it going to be faster to filter out the parents we just checked?
              // That's what's done here but I'm not sure it matters.
              tp.baseTypeSeq.toIterator.drop(1).exists(bt => !tp.parents.contains(bt) && unifyFull(bt))
            )
          )
        )

        case skolems =>
          val existential = existentialTransform(skolems, tp)(existentialAbstraction(_, _, flipVariance = !isLowerBound))
          // `isRelatable(existential)` defends against F-bounds. We've added one layer of existential abstraction to remove
          // skolems that occur immediately in the underlying type `tp`. If after this transformation, the type still
          // contains skolems from another level, it could be F-bounded, and we give up to avoid looping.
          isRelatable(existential) &&
            registerBound(existential, isLowerBound = isLowerBound, isNumericBound = isNumericBound)
      }
    }

    def registerTypeEquality(tp: Type, typeVarLHS: Boolean): Boolean = {
//      println("regTypeEq: "+(safeToString, debugString(tp), tp.getClass, if (typeVarLHS) "in LHS" else "in RHS", if (suspended) "ZZ" else if (instValid) "IV" else "")) //@MDEBUG
      def checkIsSameType(tp: Type) = (
        if (typeVarLHS) inst =:= tp
        else            tp   =:= inst
      )

      if (suspended) tp =:= origin
      else if (instValid) checkIsSameType(tp)
      else isRelatable(tp) && {
        // Calling `identityTypeMap` instantiates valid type vars (see `TypeVar.mapOver`).
        val newInst = identityTypeMap(tp)
        constr.isWithinBounds(newInst) && setInst(newInst).instValid
      }
    }

    /**
     * `?A.T =:= tp` is rewritten as the constraint `?A <: {type T = tp}`
     *
     * TODO: make these constraints count (incorporate them into implicit search in `applyImplicitArgs`)
     * (`T` corresponds to @param sym)
     */
    def registerTypeSelection(sym: Symbol, tp: Type): Boolean = {
      registerBound(HasTypeMember(sym.name.toTypeName, tp), isLowerBound = false)
    }

    private def unrelatable(tp: Type): List[TypeSkolem] = {
      UnrelatableCollector.barLevel = level
      UnrelatableCollector.collect(tp)
    }


    /** Can this variable be related in a constraint to type `tp`?
      *  This is not the case if `tp` contains type skolems whose
      *  skolemization level is higher than the level of this variable.
      */
    def isRelatable(tp: Type): Boolean = {
      IsRelatableCollector.barLevel = level
      IsRelatableCollector.collect(tp)
    }

    override def normalize: Type = (
      if (instValid) inst
      // get here when checking higher-order subtyping of the typevar by itself
      // TODO: check whether this ever happens?
      else if (isHigherKinded) etaExpand
      else super.normalize
    )
    override def etaExpand: Type = (
      if (!isHigherKinded) this
      else logResult(s"Normalizing HK $this")(typeFun(params, applyArgs(params map (_.typeConstructor))))
    )
    override def typeSymbol = origin.typeSymbol

    private def tparamsOfSym(sym: Symbol) = sym.info match {
      case PolyType(tparams, _) if !tparams.isEmpty =>
        tparams.map(_.defString).mkString("[", ",", "]")
      case _ => ""
    }
    def originName = origin.typeSymbolDirect.decodedName
    def originLocation = {
      val sym  = origin.typeSymbolDirect
      val encl = sym.owner.logicallyEnclosingMember

      // This should display somewhere between one and three
      // things which enclose the origin: at most, a class, a
      // a method, and a term.  At least, a class.
      List(
        Some(encl.enclClass),
        if (encl.isMethod) Some(encl) else None,
        if (sym.owner.isTerm && (sym.owner != encl)) Some(sym.owner) else None
      ).flatten map (s => s.decodedName + tparamsOfSym(s)) mkString "#"
    }
    private def levelString = if (settings.explaintypes.value) level else ""
    override def safeToString = (
      if ((constr eq null) || (inst eq null)) "TVar<" + originName + "=null>"
      else if (inst ne NoType) "=?" + inst
      else (if(untouchable) "!?" else "?") + levelString + originName
    )
    def originString = s"$originName in $originLocation"
    override def kind = "TypeVar"

    def cloneInternal = {
      // cloning a suspended type variable when it's suspended will cause the clone
      // to never be resumed with the current implementation
      assert(!suspended, this)
      TypeVar.trace("clone", originLocation)(
        TypeVar(origin, constr.cloneInternal, typeArgs, params)
      )
    }

    override def mapOver(map: TypeMap): Type =
      if (constr.instValid) {
        // ideally TypeVar.inst should handle this,
        // but it would have to be disentangled from TypeVar.constr.inst
        map(appliedType(constr.inst, typeArgs))
      } else map match {
        case map: VariancedTypeMap =>
          //@M !args.isEmpty implies !typeParams.isEmpty
          applyArgs(map.mapOverArgs(typeArgs, params))
        case _ => applyArgs(typeArgs mapConserve map)
      }

    override def foldOver(folder: TypeFolder): Unit =
      if (constr.instValid) folder(constr.inst)
      else this.typeArgs.foreach(folder)
  }

  /** A type carrying some annotations. Created by the typechecker
   *  when eliminating ''Annotated'' trees (see typedAnnotated).
   *
   *  @param annotations the list of annotations on the type
   *  @param underlying the type without the annotation
   */
  case class AnnotatedType(override val annotations: List[AnnotationInfo],
                           override val underlying: Type)
  extends RewrappingTypeProxy with AnnotatedTypeApi {

    assert(!annotations.isEmpty, "" + underlying)

    override protected def rewrap(tp: Type) = copy(underlying = tp)

    override def isTrivial: Boolean = underlying.isTrivial && annotations.forall(_.isTrivial)

    override def safeToString = annotations.mkString(underlying.toString + " @", " @", "")

    override def filterAnnotations(p: AnnotationInfo => Boolean): Type = {
      val (yes, no) = annotations partition p
      if (yes.isEmpty) underlying
      else if (no.isEmpty) this
      else copy(annotations = yes)
    }
    override def setAnnotations(annots: List[AnnotationInfo]): Type =
      if (annots.isEmpty) underlying
      else copy(annotations = annots)

    /** Add a number of annotations to this type */
    override def withAnnotations(annots: List[AnnotationInfo]): Type =
      if (annots.isEmpty) this
      else copy(annots ::: this.annotations)

    override def withAnnotation(anno: AnnotationInfo): Type =
      copy(anno :: this.annotations)

    /** Remove any annotations from this type.
     *  TODO - is it allowed to nest AnnotatedTypes? If not then let's enforce
     *  that at creation.  At the moment if they do ever turn up nested this
     *  recursively calls withoutAnnotations.
     */
    override def withoutAnnotations = underlying.withoutAnnotations

    /** Drop the annotations on the bounds, unless the low and high
     *  bounds are exactly tp.
     */
    override def bounds: TypeBounds = underlying.bounds match {
      case TypeBounds(_: this.type, _: this.type) => TypeBounds(this, this)
      case oftp                                   => oftp
    }
    override def lowerBound: Type = bounds.lo
    override def upperBound: Type = bounds.hi


    // ** Replace formal type parameter symbols with actual type arguments. * /
    override def instantiateTypeParams(formals: List[Symbol], actuals: List[Type]) = {
      val annotations1 = annotations.map(info => AnnotationInfo(info.atp.instantiateTypeParams(
          formals, actuals), info.args, info.assocs).setPos(info.pos))
      val underlying1 = underlying.instantiateTypeParams(formals, actuals)
      if ((annotations1 eq annotations) && (underlying1 eq underlying)) this
      else AnnotatedType(annotations1, underlying1)
    }

    /** Return the base type sequence of tp, dropping the annotations, unless the base type sequence of tp
      * is precisely tp itself. */
    override def baseTypeSeq: BaseTypeSeq = {
       val oftp = underlying.baseTypeSeq
       if ((oftp.length == 1) && (oftp(0) eq underlying))
         baseTypeSingletonSeq(this)
       else
         oftp
     }

    override def kind = "AnnotatedType"
    override def mapOver(map: TypeMap): Type = {
      val annotations1 = map.mapOverAnnotations(annotations)
      val underlying1 = map(underlying)
      if ((annotations1 eq annotations) && (underlying1 eq underlying)) this
      else if (annotations1.isEmpty) underlying1
      else AnnotatedType(annotations1, underlying1)
    }
    override def foldOver(folder: TypeFolder): Unit = {
      folder.foldOverAnnotations(annotations)
      folder(underlying)
    }
  }

  /** Creator for AnnotatedTypes.  It returns the underlying type if annotations.isEmpty
   *  rather than walking into the assertion.
   */
  def annotatedType(annots: List[AnnotationInfo], underlying: Type): Type =
    if (annots.isEmpty) underlying
    else AnnotatedType(annots, underlying)

  object AnnotatedType extends AnnotatedTypeExtractor

  object StaticallyAnnotatedType {
    def unapply(tp: Type): Option[(List[AnnotationInfo], Type)] = tp.staticAnnotations match {
      case Nil    => None
      case annots => Some((annots, tp.withoutAnnotations))
    }
  }

  /** A class representing types with a name. When an application uses
   *  named arguments, the named argument types for calling isApplicable
   *  are represented as NamedType.
   */
  case class NamedType(name: Name, tp: Type) extends Type {
    override def safeToString: String = name.toString +": "+ tp
    // TODO is this needed? We only seem to get here in ContainsCollector in error message generation
    // override def mapOver(map: TypeMap): Type = map.apply(tp)
  }
  /** As with NamedType, used only when calling isApplicable.
   *  Records that the application has a wildcard star (aka _*)
   *  at the end of it.
   */
  case class RepeatedType(tp: Type) extends Type {
    override def safeToString: String = tp.toString + ": _*"
    // TODO is this needed? We only seem to get here in ContainsCollector in error message generation
    // override def mapOver(map: TypeMap): Type = map.apply(tp)
  }

  /** A temporary type representing the erasure of a user-defined value type.
   *  Created during phase erasure, eliminated again in posterasure.
   *
   *  scala/bug#6385 Erasure's creation of bridges considers method signatures `exitingErasure`,
   *          which contain `ErasedValueType`-s. In order to correctly consider the overriding
   *          and overridden signatures as equivalent in `run/t6385.scala`, it is critical that
   *          this type contains the erasure of the wrapped type, rather than the unerased type
   *          of the value class itself, as was originally done.
   *
   *  @param   valueClazz        The value class symbol
   *  @param   erasedUnderlying  The erased type of the unboxed value
   */
  abstract case class ErasedValueType(valueClazz: Symbol, erasedUnderlying: Type) extends UniqueType {
    override def safeToString = s"ErasedValueType($valueClazz, $erasedUnderlying)"
  }

  final class UniqueErasedValueType(valueClazz: Symbol, erasedUnderlying: Type) extends ErasedValueType(valueClazz, erasedUnderlying)

  object ErasedValueType {
    def apply(valueClazz: Symbol, erasedUnderlying: Type): Type = {
      assert(valueClazz ne NoSymbol, "ErasedValueType over NoSymbol")
      unique(new UniqueErasedValueType(valueClazz, erasedUnderlying))
    }
  }

  /** A class representing an as-yet unevaluated type.
   */
  abstract class LazyType extends Type {
    override def isComplete: Boolean = false
    override def complete(sym: Symbol): Unit
    override def safeToString = "<?>"
    override def kind = "LazyType"
    def isJavaVarargsMethod: Boolean = false
    def javaThrownExceptions: List[Symbol] = Nil
  }

  /** A marker trait representing an as-yet unevaluated type
   *  which doesn't assign flags to the underlying symbol.
   */
  trait FlagAgnosticCompleter extends LazyType

  /** A marker trait representing an as-yet unevaluated type
   *  which assigns flags to the underlying symbol.
   */
  trait FlagAssigningCompleter extends LazyType

  abstract class LazyPolyType(override val typeParams: List[Symbol]) extends LazyType {
    override def safeToString =
      (if (typeParams.isEmpty) "" else typeParamsString(this)) + super.safeToString
  }

// Creators ---------------------------------------------------------------

  /** Rebind symbol `sym` to an overriding member in type `pre`. */
  private def rebind(pre: Type, sym: Symbol): Symbol = {
    if (!sym.isOverridableMember || sym.owner == pre.typeSymbol) sym
    else pre.nonPrivateMember(sym.name).suchThat { sym =>
      // scala/bug#7928 `isModuleNotMethod` is here to avoid crashing with spuriously "overloaded" module accessor and module symbols.
      //         These appear after the fields phase eliminates ModuleDefs that implement an interface.
      //         Here, we exclude the module symbol, which allows us to bind to the accessor.
      // scala/bug#8054 We must only do this after fields, otherwise we exclude the module symbol which does not yet have an accessor!
      val isModuleWithAccessor = phase.assignsFields && sym.isModuleNotMethod
      sym.isType || (!isModuleWithAccessor && sym.isStable && !sym.hasVolatileType)
    } orElse sym
  }

  /** Convert a `super` prefix to a this-type if `sym` is abstract or final. */
  private def removeSuper(tp: Type, sym: Symbol): Type = tp match {
    case SuperType(thistp, _) =>
      if (sym.isEffectivelyFinal || sym.isDeferred) thistp
      else tp
    case _ =>
      tp
  }

  /** The canonical creator for single-types */
  def singleType(pre: Type, sym: Symbol): Type = {
    if (phase.erasedTypes)
      sym.tpe.resultType
    else if (sym.isRootPackage)
      ThisType(sym.moduleClass)
    else {
      var sym1 = rebind(pre, sym)
      val pre1 = removeSuper(pre, sym1)
      if (pre1 ne pre) sym1 = rebind(pre1, sym1)
      SingleType(pre1, sym1)
    }
  }

  /** the canonical creator for a refined type with a given scope */
  def refinedType(parents: List[Type], owner: Symbol, decls: Scope, pos: Position): Type = {
    if (phase.erasedTypes)
      if (parents.isEmpty) ObjectTpe else parents.head
    else {
      val clazz = owner.newRefinementClass(pos)
      val result = RefinedType(parents, decls, clazz)
      clazz.setInfo(result)
      result
    }
  }

  /** The canonical creator for a refined type with an initially empty scope.
   */
  def refinedType(parents: List[Type], owner: Symbol): Type =
    refinedType(parents, owner, newScope, owner.pos)

  private[this] val copyRefinedTypeSSM: ReusableInstance[SubstSymMap] =
    ReusableInstance[SubstSymMap](SubstSymMap(), enabled = isCompilerUniverse)

  def copyRefinedType(original: RefinedType, parents: List[Type], decls: Scope) =
    if ((parents eq original.parents) && (decls eq original.decls)) original
    else {
      val owner = original.typeSymbol.owner
      val result =
        if (isIntersectionTypeForLazyBaseType(original)) intersectionTypeForLazyBaseType(parents)
        else refinedType(parents, owner)
      if (! decls.isEmpty){
        val syms1 = decls.toList
        for (sym <- syms1)
          result.decls.enter(sym.cloneSymbol(result.typeSymbol).resetFlag(OVERRIDE))
        val syms2 = result.decls.toList
        val resultThis = result.typeSymbol.thisType
        val substThisMap = new SubstThisMap(original.typeSymbol, resultThis)
        copyRefinedTypeSSM.using { (msm: SubstSymMap) =>
          msm.reset(syms1, syms2)
          syms2.foreach(_.modifyInfo(info => msm.apply(substThisMap.apply(info))))
        }
      }
      result
    }

  /** The canonical creator for typerefs
   *  todo: see how we can clean this up a bit
   */
  @tailrec
  final def typeRef(pre: Type, sym: Symbol, args: List[Type]): Type = {
    // type alias selections are rebound in TypeMap ("coevolved",
    // actually -- see #3731) e.g., when type parameters that are
    // referenced by the alias are instantiated in the prefix. See
    // pos/depmet_rebind_typealias.

    val sym1 = if (sym.isAbstractType) rebind(pre, sym) else sym
    // don't expand cyclical type alias
    // we require that object is initialized, thus info.typeParams instead of typeParams.
    if (sym1.isAliasType && sameLength(sym1.info.typeParams, args) && !sym1.lockOK)
      throw new RecoverableCyclicReference(sym1)

    val pre1 = pre match {
      case x: SuperType if sym1.isEffectivelyFinal || sym1.isDeferred =>
        x.thistpe
      case _ => pre
    }
    if (pre eq pre1)                                TypeRef(pre, sym1, args)
    else if (sym1.isAbstractType && !sym1.isClass)  typeRef(pre1, rebind(pre1, sym1), args)
    else                                            typeRef(pre1, sym1, args)
  }

  // Optimization to avoid creating unnecessary new typerefs.
  def copyTypeRef(tp: Type, pre: Type, sym: Symbol, args: List[Type]): Type = tp match {
    case TypeRef(pre0, sym0, _) if pre == pre0 && sym0.name == sym.name =>
      if (sym.isAliasType && sameLength(sym.info.typeParams, args) && !sym.lockOK)
        throw new RecoverableCyclicReference(sym)

      if ((args eq Nil) && (pre eq NoPrefix))
        sym.tpeHK // opt lean on TypeSymbol#tyconCache, rather than interning a type ref.
      else
        TypeRef(pre, sym, args)
    case _ =>
      typeRef(pre, sym, args)
  }

  /** Create a new MethodType */
  def copyMethodType(tp: Type, params: List[Symbol], restpe: Type): Type = MethodType(params, restpe)

  /** A creator for intersection type where intersections of a single type are
   *  replaced by the type itself, and repeated parent classes are merged.
   *
   *  !!! Repeated parent classes are not merged - is this a bug in the
   *  comment or in the code?
   */
  def intersectionType(tps: List[Type], owner: Symbol): Type = tps match {
    case tp :: Nil => tp
    case _         => refinedType(tps, owner)
  }
  /** A creator for intersection type where intersections of a single type are
   *  replaced by the type itself.
   */
  def intersectionType(tps: List[Type]): Type = tps match {
    case tp :: Nil  => tp
    case _          => refinedType(tps, commonOwner(tps))
  }
  def intersectionTypeForLazyBaseType(tps: List[Type]) = tps match {
    case tp :: Nil  => tp
    case _          => RefinedType(tps, newScope, tps.head.typeSymbolDirect)
  }
  def isIntersectionTypeForLazyBaseType(tp: RefinedType) = tp.parents match {
    case head :: _ => tp.typeSymbolDirect eq head.typeSymbolDirect
    case _ => false
  }

/**** This implementation to merge parents was checked in in commented-out
      form and has languished unaltered for five years.  I think we should
      use it or lose it.

      def merge(tps: List[Type]): List[Type] = tps match {
        case tp :: tps1 =>
          val tps1a = tps1 filter (_.typeSymbol.==(tp.typeSymbol))
          val tps1b = tps1 filter (_.typeSymbol.!=(tp.typeSymbol))
          mergePrefixAndArgs(tps1a, -1) match {
            case Some(tp1) => tp1 :: merge(tps1b)
            case None => throw new MalformedType(
              "malformed type: "+refinedType(tps, owner)+" has repeated parent class "+
              tp.typeSymbol+" with incompatible prefixes or type arguments")
          }
        case _ => tps
      }
      refinedType(merge(tps), owner)
*/

  /** A creator for type applications */
  def appliedType(tycon: Type, args: List[Type]): Type = {
    if (args.isEmpty)
      return tycon //@M! `if (args.isEmpty) tycon' is crucial (otherwise we create new types in phases after typer and then they don't get adapted (??))

    /* Disabled - causes cycles in tcpoly tests. */
    if (false && isDefinitionsInitialized) {
      assert(isUseableAsTypeArgs(args), {
        val tapp_s = s"""$tycon[${args mkString ", "}]"""
        val arg_s  = args filterNot isUseableAsTypeArg map (t => t.toString + "/" + t.getClass) mkString ", "
        s"$tapp_s includes illegal type argument $arg_s"
      })
    }

    tycon match {
      case TypeRef(pre, sym @ (NothingClass|AnyClass), _) => copyTypeRef(tycon, pre, sym, Nil)   //@M drop type args to Any/Nothing
      case TypeRef(pre, sym, Nil)                         => copyTypeRef(tycon, pre, sym, args)
      case TypeRef(pre, sym, bogons)                      => devWarning(s"Dropping $bogons from $tycon in appliedType.") ; copyTypeRef(tycon, pre, sym, args)
      case PolyType(tparams, restpe)                      => restpe.instantiateTypeParams(tparams, args)
      case MethodType(params, resultType)                 => resultType
      case NullaryMethodType(resultType)                  => resultType
      case ExistentialType(tparams, restpe)               => newExistentialType(tparams, appliedType(restpe, args))
      case st: SingletonType                              => appliedType(st.widen, args) // @M TODO: what to do? see bug1
      case RefinedType(parents, decls)                    => RefinedType(parents map (appliedType(_, args)), decls)   // @PP: Can this be right?
      case TypeBounds(lo, hi)                             => TypeBounds(appliedType(lo, args), appliedType(hi, args)) // @PP: Can this be right?
      case tv@TypeVar(_, _)                               => tv.applyArgs(args)
      case AnnotatedType(annots, underlying)              => AnnotatedType(annots, appliedType(underlying, args))
      case ErrorType | WildcardType                       => tycon
      case _                                              => abort(debugString(tycon))
    }
  }

  def appliedType(tycon: Type, args: Type*): Type =
    appliedType(tycon, args.toList)

  def appliedType(tyconSym: Symbol, args: List[Type]): Type =
    appliedType(tyconSym.typeConstructor, args)

  /** Very convenient. */
  def appliedType(tyconSym: Symbol, args: Type*): Type =
    appliedType(tyconSym.typeConstructor, args.toList)

  /** A creator and extractor for type parameterizations that strips empty type parameter lists.
   *  Use this factory method to indicate the type has kind * (it's a polymorphic value)
   */
  object GenPolyType {
    def apply(tparams: List[Symbol], tpe: Type): Type =
      if (tparams.isEmpty) tpe else PolyType(tparams, tpe)

    def unapply(tpe: Type): Some[(List[Symbol], Type)] = tpe match {
      case PolyType(tparams, restpe) => Some((tparams, restpe))
      case _                         => Some((Nil, tpe))
    }
  }
  def genPolyType(params: List[Symbol], tpe: Type): Type = GenPolyType(params, tpe)

  @deprecated("use genPolyType(...) instead", "2.10.0") // Used in reflection API
  def polyType(params: List[Symbol], tpe: Type): Type = GenPolyType(params, tpe)

  /** A creator for a type functions, assuming the type parameters tps already have the right owner. */
  def typeFun(tps: List[Symbol], body: Type): Type = PolyType(tps, body)

  /** We will need to clone the info of the original method (which obtains clones
   *  of the method type parameters), clone the type parameters of the value class,
   *  and create a new polymethod with the union of all those type parameters, with
   *  their infos adjusted to be consistent with their new home. Example:
   *
   *    class Foo[+A <: AnyRef](val xs: List[A]) extends AnyVal {
   *      def baz[B >: A](x: B): List[B] = x :: xs
   *      // baz has to be transformed into this extension method, where
   *      // A is cloned from class Foo and  B is cloned from method baz:
   *      // def extension\$baz[B >: A <: Any, A >: Nothing <: AnyRef](\$this: Foo[A])(x: B): List[B]
   *    }
   *
   *  TODO: factor out the logic for consolidating type parameters from a class
   *  and a method for re-use elsewhere, because nobody will get this right without
   *  some higher level facilities.
   */
  def extensionMethInfo(currentOwner: Symbol, extensionMeth: Symbol, origInfo: Type, clazz: Symbol): Type = {
    val GenPolyType(tparamsFromMethod, methodResult) = origInfo cloneInfo extensionMeth
    // Start with the class type parameters - clones will be method type parameters
    // so must drop their variance.
    val tparamsFromClass = cloneSymbolsAtOwner(clazz.typeParams, extensionMeth) map (_ resetFlag COVARIANT | CONTRAVARIANT)

    val thisParamType = appliedType(clazz, tparamsFromClass.map(_.tpeHK))
    val thisParam     = extensionMeth.newValueParameter(nme.SELF, extensionMeth.pos) setInfo thisParamType
    val resultType    = MethodType(List(thisParam), dropNullaryMethod(methodResult))
    val selfParamType = singleType(currentOwner.companionModule.thisType, thisParam)

    def fixres(tp: Type)    = tp.substThisAndSym(clazz, selfParamType, clazz.typeParams, tparamsFromClass)
    def fixtparam(tp: Type) = tp.substSym(clazz.typeParams, tparamsFromClass)

    // We can't substitute symbols on the entire polytype because we
    // need to modify the bounds of the cloned type parameters, but we
    // don't want to substitute for the cloned type parameters themselves.
    val tparams = tparamsFromMethod ::: tparamsFromClass
    tparams.foreach(_ modifyInfo fixtparam)
    GenPolyType(tparams, fixres(resultType))

    // For reference, calling fix on the GenPolyType plays out like this:
    // error: scala.reflect.internal.Types$TypeError: type arguments [B#7344,A#6966]
    // do not conform to method extension$baz#16148's type parameter bounds
    //
    // And the difference is visible here.  See how B is bounded from below by A#16149
    // in both cases, but in the failing case, the other type parameter has turned into
    // a different A. (What is that A? It is a clone of the original A created in
    // SubstMap during the call to substSym, but I am not clear on all the particulars.)
    //
    //  bad: [B#16154 >: A#16149, A#16155 <: AnyRef#2189]($this#16156: Foo#6965[A#16155])(x#16157: B#16154)List#2457[B#16154]
    // good: [B#16151 >: A#16149, A#16149 <: AnyRef#2189]($this#16150: Foo#6965[A#16149])(x#16153: B#16151)List#2457[B#16151]
  }


  /** A creator for existential types. This generates:
   *
   *  tpe1 where { tparams }
   *
   *  where `tpe1` is the result of extrapolating `tpe` with respect to `tparams`.
   *  Extrapolating means that type variables in `tparams` occurring
   *  in covariant positions are replaced by upper bounds, (minus any
   *  SingletonClass markers), type variables in `tparams` occurring in
   *  contravariant positions are replaced by upper bounds, provided the
   *  resulting type is legal with regard to stability, and does not contain any type
   *  variable in `tparams`.
   *
   *  The abstraction drops all type parameters that are not directly or
   *  indirectly referenced by type `tpe1`. If there are no remaining type
   *  parameters, simply returns result type `tpe`.
   */
  def existentialAbstraction(tparams: List[Symbol], tpe0: Type, flipVariance: Boolean = false): Type = {

    /* We want to narrow the list of type parameters tparams to only those which are either
     * (a) directly contained by tpe, or
     * (b) contained by the typeInfo of another parameter from tparams, known to be referred by tpe
     */
    def transitiveReferredFrom(tpe: Type): List[Symbol] = tparams match {
      case tparam :: Nil =>
        // This is for optimisation: should be equivalent to general one.
        if (tpe contains tparam) tparams else Nil
      case _ =>
        /* Algorithm to compute transitive closure, using several temporary lists (mutable ListBuffer)
         *  - pending: elements from tparams not yet known to be in the transitiveClosure
         *  - border: we know they are in closure, but we use them for search new elements
         *  - closed: already in closure, and we already searched for new elements.
         *
         * Invariant: pending, closed, and border form a partition of `tparams`.
         * Each element in tparams goes from pending to border, and from border to closed.
         * We separate border from closed to avoid recomputing `Type.contains` for same elements.
         */
        val pending = ListBuffer.empty[Symbol]
        var border  = ListBuffer.empty[Symbol]
        partitionInto(tparams, tpe.contains, border, pending)
        val closed     = ListBuffer.empty[Symbol]
        var nextBorder = ListBuffer.empty[Symbol]
        while (!border.isEmpty) {
          nextBorder.clear()
          pending.filterInPlace { paramTodo =>
            !border.exists(_.info contains paramTodo) || {
              nextBorder += paramTodo;
              false
            }
          }
          closed ++= border
          val swap = border
          border = nextBorder
          nextBorder = swap
        }
        if (closed.length == tparams.length) tparams else closed.toList
    }

    if (tparams.isEmpty || (tpe0 eq NoType)) tpe0
    else {
      val tpe           = normalizeAliases(tpe0)
      val extrapolation = new ExistentialExtrapolation(tparams)
      if (flipVariance) extrapolation.variance = Contravariant
      val tpe1          = extrapolation.extrapolate(tpe)
      newExistentialType(transitiveReferredFrom(tpe1), tpe1)
    }
  } // end existentialAbstraction


// Hash consing --------------------------------------------------------------

  private[this] val initialUniquesCapacity = 4096
  private[this] var uniques: util.WeakHashSet[Type] = _
  private[this] var uniqueRunId = NoRunId

  final def howManyUniqueTypes: Int = if (uniques == null) 0 else uniques.size

  protected def unique[T <: Type](tp: T): T =  {
    if (uniqueRunId != currentRunId) {
      uniques = util.WeakHashSet[Type](initialUniquesCapacity)
      // JZ: We used to register this as a perRunCache so it would be cleared eagerly at
      // the end of the compilation run. But, that facility didn't actually clear this map (scala/bug#8129)!
      // When i fixed that bug, run/tpeCache-tyconCache.scala started failing. Why was that?
      // I've removed the registration for now. I don't think it's particularly harmful anymore
      // as a) this is now a weak set, and b) it is discarded completely before the next run.
      uniqueRunId = currentRunId
    }
    (uniques findEntryOrUpdate tp).asInstanceOf[T]
  }

// Helper Classes ---------------------------------------------------------

  class TypeUnwrapper(poly: Boolean, existential: Boolean, annotated: Boolean, nullary: Boolean) extends (Type => Type) {
    def apply(tp: Type): Type = tp match {
      case AnnotatedType(_, underlying) if annotated      => apply(underlying)
      case ExistentialType(_, underlying) if existential  => apply(underlying)
      case PolyType(_, underlying) if poly                => apply(underlying)
      case NullaryMethodType(underlying) if nullary       => apply(underlying)
      case tp                                             => tp
    }
  }
  class ClassUnwrapper(existential: Boolean) extends TypeUnwrapper(poly = true, existential, annotated = true, nullary = false) {
    override def apply(tp: Type) = super.apply(tp.normalize) // normalize is required here
  }

  object        unwrapToClass extends ClassUnwrapper(existential = true)
  object  unwrapToStableClass extends ClassUnwrapper(existential = false)
  object   unwrapWrapperTypes extends  TypeUnwrapper(poly = true, existential = true, annotated = true, nullary = true)

  def elementExtract(container: Symbol, tp: Type): Type = {
    assert(!container.isAliasType, container)
    unwrapWrapperTypes(tp baseType container).dealiasWiden match {
      case TypeRef(_, `container`, arg :: Nil)  => arg
      case _                                    => NoType
    }
  }
  def elementExtractOption(container: Symbol, tp: Type): Option[Type] =
    elementExtract(container, tp) match {
      case NoType => None
      case tp1    => Some(tp1)
    }
  def elementTest(container: Symbol, tp: Type)(f: Type => Boolean): Boolean =
    elementExtract(container, tp) match {
      case NoType => false
      case tp1    => f(tp1)
    }
  def elementTransform(container: Symbol, tp: Type)(f: Type => Type): Type =
    elementExtract(container, tp) match {
      case NoType => NoType
      case tp1    => f(tp1)
    }

  def transparentShallowTransform(container: Symbol, tp: Type)(f: Type => Type): Type = {
    def loop(tp: Type): Type = tp match {
      case tp @ AnnotatedType(_, underlying)        => tp.copy(underlying = loop(underlying))
      case tp @ ExistentialType(_, underlying)      => tp.copy(underlying = loop(underlying))
      case tp @ PolyType(_, resultType)             => tp.copy(resultType = loop(resultType))
      case tp @ NullaryMethodType(resultType)       => tp.copy(resultType = loop(resultType))
      case tp                                       => elementTransform(container, tp)(el => appliedType(container, f(el))).orElse(f(tp))
    }
    loop(tp)
  }

  /** Repack existential types, otherwise they sometimes get unpacked in the
   *  wrong location (type inference comes up with an unexpected skolem)
   */
  def repackExistential(tp: Type): Type = (
    if (tp == NoType) tp
    else existentialAbstraction(existentialsInType(tp), tp)
  )

  def containsExistential(tpe: Type) = tpe.exists(_.typeSymbol.isExistentiallyBound)
  def existentialsInType(tpe: Type) = tpe.withFilter(_.typeSymbol.isExistentiallyBound).map(_.typeSymbol)

  def typeParamsToExistentials(clazz: Symbol, tparams: List[Symbol]): List[Symbol] = {
    val eparams = tparams map (tparam =>
      clazz.newExistential(tparam.name.toTypeName, clazz.pos) setInfo tparam.info.bounds)

    eparams foreach (_.substInfo(tparams, eparams))
    eparams
  }
  def typeParamsToExistentials(clazz: Symbol): List[Symbol] =
    typeParamsToExistentials(clazz, clazz.typeParams)

  def isRawIfWithoutArgs(sym: Symbol) = sym.isClass && !sym.typeParams.isEmpty && sym.isJavaDefined
  /** Is type tp a ''raw type''? */
  //  note: it's important to write the two tests in this order,
  //  as only typeParams forces the classfile to be read. See #400
  def isRawType(tp: Type) = !phase.erasedTypes && (tp match {
    case TypeRef(_, sym, Nil) => isRawIfWithoutArgs(sym)
    case _                    => false
  })

  @deprecated("use isRawType", "2.10.1") // presently used by sbt
  def isRaw(sym: Symbol, args: List[Type]) = (
       !phase.erasedTypes
    && args.isEmpty
    && isRawIfWithoutArgs(sym)
  )

  def singletonBounds(hi: Type) = TypeBounds.upper(intersectionType(hi :: ListOfSingletonClassTpe))

  /**
   * A more persistent version of `Type#memberType` which does not require
   * that the symbol is a direct member of the prefix.
   *
   * For instance:
   *
   * {{{
   * class C[T] {
   *   sealed trait F[A]
   *   object X {
   *     object S1 extends F[T]
   *   }
   *   class S2 extends F[T]
   * }
   * object O extends C[Int] {
   *   def foo(f: F[Int]) = f match {...} // need to enumerate sealed subtypes of the scrutinee here.
   * }
   * class S3 extends O.F[String]
   *
   * nestedMemberType(<S1>, <O.type>, <C>) = O.X.S1.type
   * nestedMemberType(<S2>, <O.type>, <C>) = O.S2.type
   * nestedMemberType(<S3>, <O.type>, <C>) = S3.type
   * }}}
   *
   * @param sym    The symbol of the subtype
   * @param pre    The prefix from which the symbol is seen
   * @param owner
   */
  def nestedMemberType(sym: Symbol, pre: Type, owner: Symbol): Type = {
    def loop(tp: Type): Type =
      if (tp.isTrivial) tp
      else if (tp.prefix.typeSymbol isNonBottomSubClass owner) {
        val widened = tp match {
          case _: ConstantType => tp // Java enum constants: don't widen to the enum type!
          case _               => tp.widen // C.X.type widens to C.this.X.type, otherwise `tp asSeenFrom (pre, C)` has no effect.
        }
        val memType = widened.asSeenFrom(pre, tp.typeSymbol.owner)
        if (tp eq widened) memType else memType.narrow
      }
      else loop(tp.prefix) memberType tp.typeSymbol

    val result = loop(sym.tpeHK)
    assert(sym.isTerm || result.typeSymbol == sym, s"($result).typeSymbol = ${result.typeSymbol}; expected ${sym}")
    result
  }

  class MissingAliasControl extends ControlThrowable
  val missingAliasException = new MissingAliasControl
  class MissingTypeControl extends ControlThrowable

// Helper Methods  -------------------------------------------------------------

  /** The maximum allowable depth of lubs or glbs over types `ts`.
    */
  def lubDepth(ts: List[Type]): Depth = {
    val td = maxDepth(ts)
    val bd = baseTypeSeqDepth(ts)
    lubDepthAdjust(td, td max bd)
  }

  /** The maximum allowable depth of lubs or glbs over given types,
   *  as a function over the maximum depth `td` of these types, and
   *  the maximum depth `bd` of all types in the base type sequences of these types.
   */
  private def lubDepthAdjust(td: Depth, bd: Depth): Depth =
    if (bd <= Depth(3)) bd
    else if (bd <= Depth(5)) td max bd.decr
    else if (bd <= Depth(7)) td max (bd decr 2)
    else td.decr max (bd decr 3)

  private def infoTypeDepth(sym: Symbol): Depth = typeDepth(sym.info)
  private def symTypeDepth(syms: List[Symbol]): Depth  = Depth.maximumBy(syms)(infoTypeDepth)
  private def baseTypeSeqDepth(tps: List[Type]): Depth = Depth.maximumBy(tps)((t: Type) => t.baseTypeSeqDepth)

  /** Is intersection of given types populated? That is,
   *  for all types tp1, tp2 in intersection
   *    for all common base classes bc of tp1 and tp2
   *      let bt1, bt2 be the base types of tp1, tp2 relative to class bc
   *      Then:
   *        bt1 and bt2 have the same prefix, and
   *        any corresponding non-variant type arguments of bt1 and bt2 are the same
   */
  def isPopulated(tp1: Type, tp2: Type): Boolean = {
    def isConsistent(tp1: Type, tp2: Type): Boolean = (tp1.dealias, tp2.dealias) match {
      case (TypeRef(pre1, sym1, args1), TypeRef(pre2, sym2, args2)) =>
        assert(sym1 == sym2, (sym1, sym2))
        (    pre1 =:= pre2
          && forall3(args1, args2, sym1.typeParams) { (arg1, arg2, tparam) =>
               // if left-hand argument is a typevar, make it compatible with variance
               // this is for more precise pattern matching
               // todo: work this in the spec of this method
               // also: think what happens if there are embedded typevars?
               if (tparam.variance.isInvariant)
                 arg1 =:= arg2
               else !arg1.isInstanceOf[TypeVar] || {
                 if (tparam.variance.isContravariant) arg1 <:< arg2
                 else arg2 <:< arg1
               }
             }
        )
      case (et: ExistentialType, _) =>
        et.withTypeVars(isConsistent(_, tp2))
      case (_, et: ExistentialType) =>
        et.withTypeVars(isConsistent(tp1, _))
      case (_, _) =>
        throw new MatchError((tp1, tp2))
    }

    def check(tp1: Type, tp2: Type) = {
      val sym1 = tp1.typeSymbol
      if (sym1.isClass && sym1.hasFlag(FINAL) && sym1 != SingletonClass)
        tp1 <:< tp2 || isNumericValueClass(sym1) && isNumericValueClass(tp2.typeSymbol)
      else tp1.baseClasses forall (bc =>
        tp2.baseTypeIndex(bc) < 0 || isConsistent(tp1.baseType(bc), tp2.baseType(bc)))
    }

    check(tp1, tp2) && check(tp2, tp1)
  }

  @tailrec
  final def normalizePlus(tp: Type): Type = {
    if (isRawType(tp)) rawToExistential(tp)
    else tp.normalize match {
      // Unify the representations of module classes
      case st @ SingleType(_, _) if st.typeSymbol.isModuleClass => st.underlying.normalize
      case st @ ThisType(sym) if sym.isModuleClass              => normalizePlus(st.underlying)
      case tpNorm                                               => tpNorm
    }
  }

  /*
  todo: change to:
  def normalizePlus(tp: Type) = tp match {
    case TypeRef(pre, sym, List()) =>
      if (!sym.isInitialized) sym.rawInfo.load(sym)
      if (sym.isJavaDefined && !sym.typeParams.isEmpty) rawToExistential(tp)
      else tp.normalize
    case _ => tp.normalize
  }
  */


  /** Are `tps1` and `tps2` lists of pairwise equivalent types? */
  def isSameTypes(tps1: List[Type], tps2: List[Type]): Boolean = {
    // OPT: hand inlined (tps1 corresponds tps2)(_ =:= _) to avoid cost of boolean unboxing (which includes
    // a null check)
    var i = tps1
    var j = tps2
    while (!(i.isEmpty || j.isEmpty)) {
      if (!(i.head =:= j.head))
        return false
      i = i.tail
      j = j.tail
    }
    i.isEmpty && j.isEmpty
  }

  /** Are `tps1` and `tps2` lists of pairwise equivalent symbols according to `_.tpe` ? */
  def isSameSymbolTypes(syms1: List[Symbol], syms2: List[Symbol]): Boolean = {
    // OPT: hand inlined (syms1 corresponds syms1)((x, y) (x.tpe =:= y.tpe)) to avoid cost of boolean unboxing (which includes
    // a null check)
    var i = syms1
    var j = syms2
    while (!(i.isEmpty || j.isEmpty)) {
      if (!(i.head.tpe =:= j.head.tpe))
        return false
      i = i.tail
      j = j.tail
    }
    i.isEmpty && j.isEmpty
  }

  private[this] var _basetypeRecursions: Int = 0
  def basetypeRecursions = _basetypeRecursions
  def basetypeRecursions_=(value: Int) = _basetypeRecursions = value

  private[this] val _pendingBaseTypes = new mutable.HashSet[Type]
  def pendingBaseTypes = _pendingBaseTypes

  /** Does this type have a prefix that begins with a type variable,
   *  or is it a refinement type? For type prefixes that fulfil this condition,
   *  type selections with the same name of equal (as determined by `=:=`) prefixes are
   *  considered equal in regard to `=:=`.
   */
  @tailrec
  final def isEligibleForPrefixUnification(tp: Type): Boolean = tp match {
    case SingleType(pre, sym)  => !(sym hasFlag PACKAGE) && isEligibleForPrefixUnification(pre)
    case tv@TypeVar(_, constr) => !tv.instValid || isEligibleForPrefixUnification(constr.inst)
    case RefinedType(_, _)     => true
    case ThisType(sym)         => sym.hasSelfType
    case _                     => false
  }

  def isErrorOrWildcard(tp: Type) = (tp eq ErrorType) || (tp eq WildcardType)

  /** This appears to be equivalent to tp.isInstanceof[SingletonType],
   *  except it excludes FoldableConstantTypes.
   */
  def isSingleType(tp: Type) = tp match {
    case ThisType(_) | SuperType(_, _) | SingleType(_, _) => true
    case LiteralType(_)                                   => true
    case _                                                => false
  }

  def isConstantType(tp: Type) = tp match {
    case FoldableConstantType(_) => true
    case _                       => false
  }

  @tailrec
  final def isExistentialType(tp: Type): Boolean = tp match {
    case _: ExistentialType           => true
    case tp: Type if tp.dealias ne tp => isExistentialType(tp.dealias)
    case _                            => false
  }

  def isImplicitMethodType(tp: Type) = tp match {
    case mt: MethodType => mt.isImplicit
    case _              => false
  }

  /** This is defined and named as it is because the goal is to exclude source
   *  level types which are not value types (e.g. MethodType) without excluding
   *  necessary internal types such as WildcardType.  There are also non-value
   *  types which can be used as type arguments (e.g. type constructors.)
   */
  def isUseableAsTypeArg(tp: Type) = (
       isInternalTypeUsedAsTypeArg(tp)  // the subset of internal types which can be type args
    || isHKTypeRef(tp)                  // not a value type, but ok as a type arg
    || isValueElseNonValue(tp)          // otherwise only value types
  )

  private def isHKTypeRef(tp: Type) = tp match {
    case TypeRef(_, sym, Nil) => tp.isHigherKinded
    case _                    => false
  }
  @tailrec final def isUseableAsTypeArgs(tps: List[Type]): Boolean = tps match {
    case Nil     => true
    case x :: xs => isUseableAsTypeArg(x) && isUseableAsTypeArgs(xs)
  }

  /** The "third way", types which are neither value types nor
   *  non-value types as defined in the SLS, further divided into
   *  types which are used internally in type applications and
   *  types which are not.
   */
  /**** Not used right now, but kept around to document which Types
   *    land in which bucket.
  private def isInternalTypeNotUsedAsTypeArg(tp: Type): Boolean = tp match {
    case AntiPolyType(pre, targs)            => true
    case ClassInfoType(parents, defs, clazz) => true
    case ErasedValueType(tref)               => true
    case NoPrefix                            => true
    case NoType                              => true
    case SuperType(thistpe, supertpe)        => true
    case TypeBounds(lo, hi)                  => true
    case _                                   => false
  }
  ****/
  private def isInternalTypeUsedAsTypeArg(tp: Type): Boolean = tp match {
    case ErrorType    => true
    case _: ProtoType => true
    case _: TypeVar   => true
    case _            => false
  }
  private def isAlwaysValueType(tp: Type) = tp match {
    case RefinedType(_, _)       => true
    case ExistentialType(_, _)   => true
    case ConstantType(_)         => true
    case _                       => false
  }
  private def isAlwaysNonValueType(tp: Type) = tp match {
    case OverloadedType(_, _)          => true
    case NullaryMethodType(_)          => true
    case MethodType(_, _)              => true
    case PolyType(_, MethodType(_, _)) => true
    case _                             => false
  }
  /** Should be called only with types for which a clear true/false answer
   *  can be given: true == value type, false == non-value type.  Otherwise,
   *  an exception is thrown.
   */
  @tailrec
  private def isValueElseNonValue(tp: Type): Boolean = tp match {
    case tp if isAlwaysValueType(tp)           => true
    case tp if isAlwaysNonValueType(tp)        => false
    case AnnotatedType(_, underlying)          => isValueElseNonValue(underlying)
    case SingleType(_, sym)                    => sym.isValue           // excludes packages and statics
    case TypeRef(_, _, _) if tp.isHigherKinded => false                 // excludes type constructors
    case ThisType(sym)                         => !sym.isPackageClass   // excludes packages
    case TypeRef(_, sym, _)                    => !sym.isPackageClass   // excludes packages
    case PolyType(_, _)                        => true                  // poly-methods excluded earlier
    case tp                                    => throw new IllegalArgumentException("isValueElseNonValue called with third-way type " + tp)
  }

  /** SLS 3.2, Value Types
   *  Is the given type definitely a value type? A true result means
   *  it verifiably is, but a false result does not mean it is not,
   *  only that it cannot be assured.  To avoid false positives, this
   *  defaults to false, but since Type is not sealed, one should take
   *  a false answer with a grain of salt.  This method may be primarily
   *  useful as documentation; it is likely that !isNonValueType(tp)
   *  will serve better than isValueType(tp).
   */
  /** def isValueType(tp: Type) = isValueElseNonValue(tp) */

  /** SLS 3.3, Non-Value Types
   *  Is the given type definitely a non-value type, as defined in SLS 3.3?
   *  The specification-enumerated non-value types are method types, polymorphic
   *  method types, and type constructors.  Supplements to the specified set of
   *  non-value types include: types which wrap non-value symbols (packages
   *  and statics), overloaded types. Varargs and by-name types T* and (=> T) are
   *  not designated non-value types because there is code which depends on using
   *  them as type arguments, but their precise status is unclear.
   */
  /** def isNonValueType(tp: Type) = !isValueElseNonValue(tp) */

  def isNonRefinementClassType(tpe: Type) = tpe match {
    case SingleType(_, sym) => sym.isModuleClass
    case TypeRef(_, sym, _) => sym.isClass && !sym.isRefinementClass
    case ErrorType          => true
    case _                  => false
  }

  def isSubArgs(tps1: List[Type], tps2: List[Type], tparams: List[Symbol], depth: Depth): Boolean = {
    def isSubArg(t1: Type, t2: Type, tparam: Symbol) = (
         (tparam.isCovariant || isSubType(t2, t1, depth))     // The order of these two checks can be material for performance (scala/bug#8478)
      && (tparam.isContravariant || isSubType(t1, t2, depth))
    )

    corresponds3(tps1, tps2, tparams)(isSubArg)
  }

  def specializesSym(tp: Type, sym: Symbol, depth: Depth): Boolean = {
    def directlySpecializedBy(member: Symbol): Boolean = (
         member == sym
      || specializesSym(tp.narrow, member, sym.owner.thisType, sym, depth)
    )
    // Closure reduction, else this would be simply `member exists directlySpecializedBy`
    def specializedBy(member: Symbol): Boolean = (
      if (member eq NoSymbol) false
      else if (member.isOverloaded) member.alternatives exists directlySpecializedBy
      else directlySpecializedBy(member)
    )
    val isHasMember = sym.info == WildcardType // OPT avoid full findMember during search for extension methods, e.g pt = `?{ def extension: ? }`.

    (    (tp.typeSymbol isBottomSubClass sym.owner)
      || (if (isHasMember) tp.hasNonPrivateMember(sym.name) else specializedBy(tp nonPrivateMember sym.name))
      )
  }

  /** Does member `symLo` of `tpLo` have a stronger type
   *  than member `symHi` of `tpHi`?
   */
  protected[internal] def specializesSym(preLo: Type, symLo: Symbol, preHi: Type, symHi: Symbol, depth: Depth): Boolean =
    (symHi.isAliasType || symHi.isTerm || symHi.isAbstractType) && {
      val symHiInfo = symHi.info
      if (symHi.isTerm && symHiInfo == WildcardType) {
        // OPT fast path (avoiding tpLo.memberType) for wildcards which appear here frequently in the search for implicit views.
        !symHi.isStable || symLo.isStable     // sub-member must remain stable
      } else {
        // only now that we know symHi is a viable candidate, do the expensive checks: ----V
        require((symLo ne NoSymbol) && (symHi ne NoSymbol), ((preLo, symLo, preHi, symHi, depth)))

        val tpHi = symHiInfo.asSeenFrom(preHi, symHi.owner).substThis(preHi.typeSymbol, preLo)

        // Should we use memberType or memberInfo?
        // memberType transforms (using `asSeenFrom`) `sym.tpe`,
        // whereas memberInfo performs the same transform on `sym.info`.
        // For term symbols, this ends up being the same thing (`sym.tpe == sym.info`).
        // For type symbols, however, the `.info` of an abstract type member
        // is defined by its bounds, whereas its `.tpe` is a `TypeRef` to that type symbol,
        // so that `sym.tpe <:< sym.info`, but not the other way around.
        //
        // Thus, for the strongest (correct) result,
        // we should use `memberType` on the low side.
        //
        // On the high side, we should use the result appropriate
        // for the right side of the `<:<` above (`memberInfo`).
        val tpLo = preLo.memberType(symLo)

        debuglog(s"specializesSymHi: $preHi . $symHi : $tpHi")
        debuglog(s"specializesSymLo: $preLo . $symLo : $tpLo")

        if (symHi.isTerm)
          (isSubType(tpLo, tpHi, depth)        &&
            (!symHi.isStable || symLo.isStable) &&                                // sub-member must remain stable
            (!symLo.hasVolatileType || symHi.hasVolatileType || tpHi.isWildcard)) // sub-member must not introduce volatility
        else if (symHi.isAbstractType)
          ((tpHi.bounds containsType tpLo) &&
            kindsConform(symHi :: Nil, tpLo :: Nil, preLo, symLo.owner))
        else // we know `symHi.isAliasType` (see above)
          tpLo =:= tpHi
      }
    }

  /** A function implementing `tp1` matches `tp2`. */
  final def matchesType(tp1: Type, tp2: Type, alwaysMatchSimple: Boolean): Boolean = {
    def matchesQuantified(tparams1: List[Symbol], tparams2: List[Symbol], res1: Type, res2: Type): Boolean = (
      sameLength(tparams1, tparams2) &&
      matchesType(res1, res2.substSym(tparams2, tparams1), alwaysMatchSimple)
    )
    def lastTry =
      tp2 match {
        case ExistentialType(_, res2) if alwaysMatchSimple =>
          matchesType(tp1, res2, alwaysMatchSimple = true)
        case MethodType(_, _) =>
          false
        case PolyType(_, _) =>
          false
        case _ =>
          alwaysMatchSimple || tp1 =:= tp2
      }
    tp1 match {
      case mt1 @ MethodType(params1, res1) =>
        tp2 match {
          case mt2 @ MethodType(params2, res2) =>
            // sameLength(params1, params2) was used directly as pre-screening optimization (now done by matchesQuantified -- is that ok, performance-wise?)
            mt1.isImplicit == mt2.isImplicit &&
            matchingParams(params1, params2) &&
            matchesQuantified(params1, params2, res1, res2)
          case NullaryMethodType(res2) =>
            if (params1.isEmpty) matchesType(res1, res2, alwaysMatchSimple)
            else matchesType(tp1, res2, alwaysMatchSimple)
          case ExistentialType(_, res2) =>
            alwaysMatchSimple && matchesType(tp1, res2, alwaysMatchSimple = true)
          case TypeRef(_, sym, Nil) =>
            params1.isEmpty && sym.isModuleClass && matchesType(res1, tp2, alwaysMatchSimple)
          case _ =>
            false
        }
      case mt1 @ NullaryMethodType(res1) =>
        tp2 match {
          case mt2 @ MethodType(Nil, res2)  => // could never match if params nonEmpty, and !mt2.isImplicit is implied by empty param list
            matchesType(res1, res2, alwaysMatchSimple)
          case NullaryMethodType(res2) =>
            matchesType(res1, res2, alwaysMatchSimple)
          case ExistentialType(_, res2) =>
            alwaysMatchSimple && matchesType(tp1, res2, alwaysMatchSimple = true)
          case TypeRef(_, sym, Nil) if sym.isModuleClass =>
            matchesType(res1, tp2, alwaysMatchSimple)
          case _ =>
            matchesType(res1, tp2, alwaysMatchSimple)
        }
      case PolyType(tparams1, res1) =>
        tp2 match {
          case PolyType(tparams2, res2) =>
            if ((tparams1 corresponds tparams2)(_ eq _))
              matchesType(res1, res2, alwaysMatchSimple)
            else
              matchesQuantified(tparams1, tparams2, res1, res2)
          case ExistentialType(_, res2) =>
            alwaysMatchSimple && matchesType(tp1, res2, alwaysMatchSimple = true)
          case _ =>
            false // remember that tparams1.nonEmpty is now an invariant of PolyType
        }
      case ExistentialType(tparams1, res1) =>
        tp2 match {
          case ExistentialType(tparams2, res2) =>
            matchesQuantified(tparams1, tparams2, res1, res2)
          case _ =>
            if (alwaysMatchSimple) matchesType(res1, tp2, alwaysMatchSimple = true)
            else lastTry
        }
      case TypeRef(_, sym, Nil) if sym.isModuleClass =>
        tp2 match {
          case MethodType(Nil, res2)   => matchesType(tp1, res2, alwaysMatchSimple)
          case NullaryMethodType(res2) => matchesType(tp1, res2, alwaysMatchSimple)
          case _                       => lastTry
        }
      case _ =>
        lastTry
    }
  }

/** matchesType above is an optimized version of the following implementation:

  def matchesType2(tp1: Type, tp2: Type, alwaysMatchSimple: Boolean): Boolean = {
    def matchesQuantified(tparams1: List[Symbol], tparams2: List[Symbol], res1: Type, res2: Type): Boolean =
      tparams1.length == tparams2.length &&
      matchesType(res1, res2.substSym(tparams2, tparams1), alwaysMatchSimple)
    (tp1, tp2) match {
      case (MethodType(params1, res1), MethodType(params2, res2)) =>
        params1.length == params2.length && // useful pre-screening optimization
        matchingParams(params1, params2, tp1.isInstanceOf[JavaMethodType], tp2.isInstanceOf[JavaMethodType]) &&
        matchesType(res1, res2, alwaysMatchSimple) &&
        tp1.isImplicit == tp2.isImplicit
      case (PolyType(tparams1, res1), PolyType(tparams2, res2)) =>
        matchesQuantified(tparams1, tparams2, res1, res2)
      case (NullaryMethodType(rtp1), MethodType(List(), rtp2)) =>
        matchesType(rtp1, rtp2, alwaysMatchSimple)
      case (MethodType(List(), rtp1), NullaryMethodType(rtp2)) =>
        matchesType(rtp1, rtp2, alwaysMatchSimple)
      case (ExistentialType(tparams1, res1), ExistentialType(tparams2, res2)) =>
        matchesQuantified(tparams1, tparams2, res1, res2)
      case (ExistentialType(_, res1), _) if alwaysMatchSimple =>
        matchesType(res1, tp2, alwaysMatchSimple)
      case (_, ExistentialType(_, res2)) if alwaysMatchSimple =>
        matchesType(tp1, res2, alwaysMatchSimple)
      case (NullaryMethodType(rtp1), _) =>
        matchesType(rtp1, tp2, alwaysMatchSimple)
      case (_, NullaryMethodType(rtp2)) =>
        matchesType(tp1, rtp2, alwaysMatchSimple)
      case (MethodType(_, _), _) => false
      case (PolyType(_, _), _)   => false
      case (_, MethodType(_, _)) => false
      case (_, PolyType(_, _))   => false
      case _ =>
        alwaysMatchSimple || tp1 =:= tp2
    }
  }
*/

  /** Are `syms1` and `syms2` parameter lists with pairwise equivalent types? */
  protected[internal] def matchingParams(syms1: List[Symbol], syms2: List[Symbol]): Boolean = syms1 match {
    case Nil =>
      syms2.isEmpty
    case sym1 :: rest1 =>
      syms2 match {
        case Nil =>
          false
        case sym2 :: rest2 =>
          val tp1 = sym1.tpe
          val tp2 = sym2.tpe
          tp1 =:= tp2 && matchingParams(rest1, rest2)
      }
  }

  /** Do type arguments `targs` conform to formal parameters `tparams`?
   */
  def isWithinBounds(pre: Type, owner: Symbol, tparams: List[Symbol], targs: List[Type]): Boolean = {
    def instantiatedBound(tparam: Symbol): TypeBounds =
      tparam.info.asSeenFrom(pre, owner).instantiateTypeParams(tparams, targs).bounds

    if (targs.exists(!_.annotations.isEmpty)){
      var bounds = mapList(tparams)(instantiatedBound)
      bounds = adaptBoundsToAnnotations(bounds, tparams, targs)
      (bounds corresponds targs)(_ containsType _)
    } else
      (tparams corresponds targs){ (tparam, targ) =>
        instantiatedBound(tparam) containsType targ
      }
  }

  def elimAnonymousClass(t: Type) = t match {
    case TypeRef(pre, clazz, Nil) if clazz.isAnonymousClass =>
      clazz.classBound.asSeenFrom(pre, clazz.owner)
    case _ =>
      t
  }

  /** A list of the typevars in a type. */
  def typeVarsInType(tp: Type): List[TypeVar] =
    typeVarsInTypeRev(tp).reverse

  private[this] def typeVarsInTypeRev(tp: Type): List[TypeVar] = {
    var tvs: List[TypeVar] = Nil
    tp foreach {
      case t: TypeVar => tvs ::= t
      case _          =>
    }
    tvs
  }

  // If this type contains type variables, put them to sleep for a while.
  // Don't just wipe them out by replacing them by the corresponding type
  // parameter, as that messes up (e.g.) type variables in type refinements.
  // Without this, the matchesType call would lead to type variables on both
  // sides of a subtyping/equality judgement, which can lead to recursive types
  // being constructed. See pos/t0851 for a situation where this happens.
  @inline final def suspendingTypeVars[T](tvs: List[TypeVar])(op: => T): T = {
    val saved = bitSetByPredicate(tvs)(_.suspended)
    tvs foreach (_.suspended = true)

    try op
    finally {
      var index = 0
      var sss = tvs
      while (sss != Nil) {
        val tv = sss.head
        tv.suspended = saved(index)
        index += 1
        sss = sss.tail
      }
    }
  }

  final def stripExistentialsAndTypeVars(ts: List[Type], expandLazyBaseType: Boolean = false): (List[Type], List[Symbol]) = {
    val needsStripping = ts.exists {
      case _: RefinedType | _: TypeVar | _: ExistentialType => true
      case _ => false
    }
    if (!needsStripping) (ts, Nil) // fast path for common case
    else {
      val tparams  = ListBuffer[Symbol]()
      val stripped = ListBuffer[Type]()
      def stripType(tp: Type): Unit = tp match {
        case rt: RefinedType if isIntersectionTypeForLazyBaseType(rt) =>
          if (expandLazyBaseType)
            rt.parents foreach stripType
          else {
            devWarning(s"Unexpected RefinedType in stripExistentialsAndTypeVars $ts, not expanding")
            stripped += tp
          }
        case ExistentialType(qs, underlying) =>
          tparams ++= qs
          stripType(underlying)
        case tv@TypeVar(_, constr) =>
          if (tv.instValid) stripType(constr.inst)
          else if (tv.untouchable) stripped += tv
          else {} // ignore when this happens (neg/t10514.scala) -- don't abort("trying to do lub/glb of typevar " + tv)
        case tp => stripped += tp
      }
      ts foreach stripType
      (stripped.toList, tparams.toList)
    }
  }

  /** Compute lub (if `variance == Covariant`) or glb (if `variance == Contravariant`) of given list
   *  of types `tps`. All types in `tps` are typerefs or singletypes
   *  with the same symbol.
   *  Return `x` if the computation succeeds with result `x`.
   *  Return `NoType` if the computation fails.
   */
  def mergePrefixAndArgs(tps0: List[Type], variance: Variance, depth: Depth): Type = {
    val (tps, tparams) = stripExistentialsAndTypeVars(tps0, expandLazyBaseType = true)

    val merged = tps match {
      case tp :: Nil => tp
      case TypeRef(_, sym, _) :: rest =>
        val pres = tps map (_.prefix) // prefix normalizes automatically
        val pre = if (variance.isPositive) lub(pres, depth) else glb(pres, depth)
        val argss = tps map (_.normalize.typeArgs) // symbol equality (of the tp in tps) was checked using typeSymbol, which normalizes, so should normalize before retrieving arguments
        val capturedParams = new ListBuffer[Symbol]
        try {
          if (sym == ArrayClass && phase.erasedTypes) {
            // special treatment for lubs of array types after erasure:
            // if argss contain one value type and some other type, the lub is Object
            // if argss contain several reference types, the lub is an array over lub of argtypes
            if (argss.exists(_.isEmpty)) {
              NoType  // something is wrong: an array without a type arg.
            }
            else {
              val argH = argss.head.head
              if (argss.tail forall (_.head =:= argH)) typeRef(pre, sym, List(argH))
              else if (argss exists (args => isPrimitiveValueClass(args.head.typeSymbol))) ObjectTpe
              else typeRef(pre, sym, List(lub(argss.map(_.head))))
            }
          }
          else transposeSafe(argss) match {
            case None =>
              // transpose freaked out because of irregular argss
              // catching just in case (shouldn't happen, but also doesn't cost us)
              // [JZ] It happens: see scala/bug#5683.
              debuglog(s"transposed irregular matrix!? tps=$tps argss=$argss")
              NoType
            case Some(argsst) =>
              var capturedParamIds = 0
              val args = map2(sym.typeParams, argsst) { (tparam, as0) =>
                val as = as0.distinct
                if (as.size == 1) as.head
                else if (depth.isZero) {
                  log("Giving up merging args: can't unify %s under %s".format(as.mkString(", "), tparam.fullLocationString))
                  // Don't return "Any" (or "Nothing") when we have to give up due to
                  // recursion depth. Return NoType, which prevents us from poisoning
                  // lublist's results. It can recognize the recursion and deal with it, but
                  // only if we aren't returning invalid types.
                  NoType
                }
                else {
                  val hktParams = tparam.initialize.typeParams
                  val hktArgs = hktParams.map(_.typeConstructor)
                  def applyHK(tp: Type) = appliedType(tp, hktArgs)
                  def bindHK(tp: Type) = typeFun(hktParams, tp)
                  // Make `as` well-kinded by binding higher-order type params of `tparam`
                  // (so that the type arguments in `as` have the same kind as the type parameter `tparam`).
                  val asKinded = if (hktParams.isEmpty) as else as.map(a => bindHK(applyHK(a)))

                  if (tparam.variance == variance) lub(asKinded, depth.decr)
                  else if (tparam.variance == variance.flip) glb(asKinded, depth.decr)
                  else {
                    val l = lub(asKinded, depth.decr)
                    val g = glb(asKinded, depth.decr)
                    if (l <:< g) l
                    else {
                      // @M this has issues with f-bounds, see #2251
                      // Martin: Not sure there is a good way to fix it. For the moment we
                      // just err on the conservative side, i.e. with a bound that is too high.
                      capturedParamIds += 1
                      val capturedParamId = capturedParamIds
                      val bounds = if (hktParams.isEmpty) TypeBounds(g, l) else bindHK(TypeBounds(applyHK(g), applyHK(l)))
                      val qvar = commonOwner(as).freshExistential("", capturedParamId) setInfo bounds
                      capturedParams += qvar
                      qvar.tpe
                    }
                  }
                }
              }
              if (args contains NoType) NoType
              else existentialAbstraction(capturedParams.toList, typeRef(pre, sym, args))
          }
        } catch {
          case ex: MalformedType => NoType
        }
      case SingleType(_, sym) :: rest =>
        val pres = tps map (_.prefix)
        val pre = if (variance.isPositive) lub(pres, depth) else glb(pres, depth)
        try singleType(pre, sym)
        catch { case ex: MalformedType => NoType }
      case _ =>
        abort(s"mergePrefixAndArgs($tps, $variance, $depth): unsupported tps")
    }
    existentialAbstraction(tparams, merged)
  }

  def addMember(thistp: Type, tp: Type, sym: Symbol): Unit = addMember(thistp, tp, sym, AnyDepth)

  /** Make symbol `sym` a member of scope `tp.decls`
   *  where `thistp` is the narrowed owner type of the scope.
   */
  def addMember(thistp: Type, tp: Type, sym: Symbol, depth: Depth): Unit = {
    assert(sym != NoSymbol, "Adding member NoSymbol")
    // debuglog("add member " + sym+":"+sym.info+" to "+thistp) //DEBUG
    if (!specializesSym(thistp, sym, depth)) {
      if (sym.isTerm)
        for (alt <- tp.nonPrivateDecl(sym.name).alternatives)
          if (specializesSym(thistp, sym, thistp, alt, depth))
            tp.decls unlink alt
      tp.decls enter sym
    }
  }

  def isJavaVarargsAncestor(clazz: Symbol) = (
       clazz.isClass
    && clazz.isJavaDefined
    && (clazz.info.nonPrivateDecls exists isJavaVarArgsMethod)
  )
  def inheritsJavaVarArgsMethod(clazz: Symbol) =
    clazz.thisType.baseClasses exists isJavaVarargsAncestor

// Errors and Diagnostics -----------------------------------------------------

  /** A throwable signalling a type error */
  class TypeError(var pos: Position, val msg: String) extends Throwable(msg) {
    def this(msg: String) = this(NoPosition, msg)

    final override def fillInStackTrace() =
      if (settings.isDebug) super.fillInStackTrace() else this
  }

  // TODO: RecoverableCyclicReference should be separated from TypeError,
  // but that would be a big change. Left for further refactoring.
  /** An exception for cyclic references from which we can recover */
  case class RecoverableCyclicReference(sym: Symbol)
    extends TypeError("illegal cyclic reference involving " + sym) {
    if (settings.isDebug) printStackTrace()
  }

  class NoCommonType(tps: List[Type]) extends ControlThrowable(
    "lub/glb of incompatible types: " + tps.mkString("", " and ", ""))

  /** A throwable signalling a malformed type */
  class MalformedType(msg: String) extends TypeError(msg) {
    def this(pre: Type, tp: String) = this("malformed type: " + pre + "#" + tp)
  }

  /** The current indentation string for traces */
  private[this] var _indent: String = ""
  protected def indent = _indent
  protected def indent_=(value: String) = _indent = value

  /** Perform operation `p` on arguments `tp1`, `arg2` and print trace of computation. */
  protected def explain[T](op: String, p: (Type, T) => Boolean, tp1: Type, arg2: T): Boolean = {
    inform(indent + tp1 + " " + op + " " + arg2 + "?" /* + "("+tp1.getClass+","+arg2.getClass+")"*/)
    indent = indent + "  "
    val result = p(tp1, arg2)
    indent = indent stripSuffix "  "
    inform(indent + result)
    result
  }

  /** If option `explaintypes` is set, print a subtype trace for `found <:< required`. */
  def explainTypes(found: Type, required: Type): Unit = {
    if (settings.explaintypes.value) withTypesExplained(found <:< required)
  }

  /** If option `explaintypes` is set, print a subtype trace for `op(found, required)`. */
  def explainTypes(op: (Type, Type) => Any, found: Type, required: Type): Unit = {
    if (settings.explaintypes.value) withTypesExplained(op(found, required))
  }

  /** Execute `op` while printing a trace of the operations on types executed. */
  def withTypesExplained[A](op: => A): A = {
    val s = explainSwitch
    try { explainSwitch = true; op } finally { explainSwitch = s }
  }

  def isUnboundedGeneric(tp: Type) = tp match {
    case t @ TypeRef(_, sym, _) => sym.isAbstractType && (!(t <:< AnyRefTpe) || (t.upperBound eq ObjectTpeJava))
    case _                      => false
  }
  def isBoundedGeneric(tp: Type) = tp match {
    case TypeRef(_, sym, _) if sym.isAbstractType => tp <:< AnyRefTpe && !(tp.upperBound eq ObjectTpeJava)
    case TypeRef(_, sym, _)                       => !isPrimitiveValueClass(sym)
    case _                                        => false
  }
  // Add serializable to a list of parents, unless one of them already is
  def addSerializable(ps: Type*): List[Type] = (
    if (ps exists typeIsSubTypeOfSerializable) ps.toList
    else (ps :+ SerializableTpe).toList
  )

  /** Adds the @uncheckedBound annotation if the given `tp` has type arguments */
  final def uncheckedBounds(tp: Type): Type = {
    if (tp.typeArgs.isEmpty || UncheckedBoundsClass == NoSymbol) tp // second condition for backwards compatibility with older scala-reflect.jar
    else tp.withAnnotation(AnnotationInfo marker UncheckedBoundsClass.tpe)
  }

  /** Members of the given class, other than those inherited
   *  from Any or AnyRef.
   */
  def nonTrivialMembers(clazz: Symbol): Scope = clazz.info.members filterNot isUniversalMember

  /** Members which can be imported into other scopes.
   */
  def importableMembers(pre: Type): Scope = pre.members filter isImportable

  def invalidateTreeTpeCaches(tree: Tree, updatedSyms: collection.Set[Symbol]) = if (!updatedSyms.isEmpty) {
    val invldtr = new InvalidateTypeCaches(updatedSyms)
    for (t <- tree if t.tpe != null)
      invldtr.invalidate(t.tpe)
  }

  def invalidateCaches(t: Type, updatedSyms: collection.Set[Symbol]): Unit =
    new InvalidateTypeCaches(updatedSyms).invalidate(t)

  class InvalidateTypeCaches(changedSymbols: collection.Set[Symbol]) extends TypeFolder {
    private var res = false
    private val seen = new java.util.IdentityHashMap[Type, Boolean]

    def invalidate(tps: Iterable[Type]): Unit = {
      res = false
      seen.clear()
      try tps.foreach(invalidateImpl)
      finally seen.clear()
    }

    def invalidate(tp: Type): Unit = invalidate(List(tp))

    protected def invalidateImpl(tp: Type): Boolean = Option(seen.get(tp)).getOrElse {
      val saved = res
      try {
        apply(tp)
        res
      } finally res = saved
    }

    def apply(tp: Type): Unit = tp match {
      case _ if seen.containsKey(tp) =>

      case tr: TypeRef =>
        val preInvalid = invalidateImpl(tr.pre)
        var argsInvalid = false
        tr.args.foreach(arg => argsInvalid = invalidateImpl(arg) || argsInvalid)
        if (preInvalid || argsInvalid || changedSymbols(tr.sym)) {
          tr.invalidateTypeRefCaches()
          res = true
        }
        seen.put(tp, res)

      case ct: CompoundType if ct.baseClasses.exists(changedSymbols) =>
        ct.invalidatedCompoundTypeCaches()
        res = true
        seen.put(tp, res)

      case st: SingleType =>
        val preInvalid = invalidateImpl(st.pre)
        if (preInvalid || changedSymbols(st.sym)) {
          st.invalidateSingleTypeCaches()
          res = true
        }
        val underInvalid = (st.underlying ne st) && invalidateImpl(st.underlying)
        res ||= underInvalid
        seen.put(tp, res)

      case _ =>
        tp.foldOver(this)
        seen.put(tp, res)
    }
  }

  val shorthands = Set(
    "scala.collection.immutable.List",
    "scala.collection.immutable.Nil",
    "scala.collection.immutable.Seq",
    "scala.collection.immutable.IndexedSeq",
    "scala.collection.mutable.StringBuilder",
    "scala.collection.Traversable",
    "scala.collection.Iterable",
    "scala.collection.Iterator")

  private[scala] val typeContainsTypeVar = { val collector = new FindTypeCollector(_.isInstanceOf[TypeVar]); (tp: Type) => collector.collect(tp).isDefined }
  private[scala] val typeIsSubTypeOfSerializable = (tp: Type) => tp <:< SerializableTpe

  @tailrec
  private[scala] final def typeIsNothing(tp: Type): Boolean =
    tp.dealias match {
      case PolyType(_, resultType) => typeIsNothing(resultType)
      case TypeRef(_, NothingClass, _) => true
      case _ => false
    }

  @tailrec
  private[scala] final def typeIsAnyOrJavaObject(tp: Type): Boolean =
    tp.dealias match {
      case PolyType(_, resultType) => typeIsAnyOrJavaObject(resultType)
      case TypeRef(_, AnyClass, _) => true
      case _: ObjectTpeJavaRef => true
      case _ => false
    }

  private[scala] final def typeIsAnyExactly(tp: Type): Boolean =
    tp.dealias match {
      case PolyType(_, resultType) => typeIsAnyExactly(resultType)
      case TypeRef(_, AnyClass, _) => true
      case _ => false
    }

  private[scala] val typeIsHigherKinded = (tp: Type) => tp.isHigherKinded

  /** The maximum depth of type `tp` */
  final def typeDepth(tp: Type): Depth = tp match {
    case TypeRef(pre, sym, args)          => typeDepth(pre) max maxDepth(args).incr
    case RefinedType(parents, decls)      => maxDepth(parents) max symTypeDepth(decls.toList).incr
    case TypeBounds(lo, hi)               => typeDepth(lo) max typeDepth(hi)
    case MethodType(paramtypes, result)   => typeDepth(result)
    case NullaryMethodType(result)        => typeDepth(result)
    case PolyType(tparams, result)        => typeDepth(result) max symTypeDepth(tparams).incr
    case ExistentialType(tparams, result) => typeDepth(result) max symTypeDepth(tparams).incr
    case _                                => Depth(1)
  }

  private[scala] def maxDepth(tps: List[Type]): Depth =
    Depth.maximumBy(tps)(typeDepth)

  @tailrec private def areTrivialTypes(tps: List[Type]): Boolean = tps match {
    case tp :: rest => tp.isTrivial && areTrivialTypes(rest)
    case _ => true
  }

// -------------- Classtags --------------------------------------------------------

  implicit val AnnotatedTypeTag: ClassTag[AnnotatedType] = ClassTag[AnnotatedType](classOf[AnnotatedType])
  implicit val BoundedWildcardTypeTag: ClassTag[BoundedWildcardType] = ClassTag[BoundedWildcardType](classOf[BoundedWildcardType])
  implicit val ClassInfoTypeTag: ClassTag[ClassInfoType] = ClassTag[ClassInfoType](classOf[ClassInfoType])
  implicit val CompoundTypeTag: ClassTag[CompoundType] = ClassTag[CompoundType](classOf[CompoundType])
  implicit val ConstantTypeTag: ClassTag[ConstantType] = ClassTag[ConstantType](classOf[ConstantType])
  implicit val ExistentialTypeTag: ClassTag[ExistentialType] = ClassTag[ExistentialType](classOf[ExistentialType])
  implicit val MethodTypeTag: ClassTag[MethodType] = ClassTag[MethodType](classOf[MethodType])
  implicit val NullaryMethodTypeTag: ClassTag[NullaryMethodType] = ClassTag[NullaryMethodType](classOf[NullaryMethodType])
  implicit val PolyTypeTag: ClassTag[PolyType] = ClassTag[PolyType](classOf[PolyType])
  implicit val RefinedTypeTag: ClassTag[RefinedType] = ClassTag[RefinedType](classOf[RefinedType])
  implicit val SingletonTypeTag: ClassTag[SingletonType] = ClassTag[SingletonType](classOf[SingletonType])
  implicit val SingleTypeTag: ClassTag[SingleType] = ClassTag[SingleType](classOf[SingleType])
  implicit val SuperTypeTag: ClassTag[SuperType] = ClassTag[SuperType](classOf[SuperType])
  implicit val ThisTypeTag: ClassTag[ThisType] = ClassTag[ThisType](classOf[ThisType])
  implicit val TypeBoundsTag: ClassTag[TypeBounds] = ClassTag[TypeBounds](classOf[TypeBounds])
  implicit val TypeRefTag: ClassTag[TypeRef] = ClassTag[TypeRef](classOf[TypeRef])
  implicit val TypeTagg: ClassTag[Type] = ClassTag[Type](classOf[Type])
}

object TypeConstants {
  final val DefaultLogThreshhold         = 50
  final val LogPendingBaseTypesThreshold = DefaultLogThreshhold
  final val LogVolatileThreshold         = DefaultLogThreshhold
}

trait TypesStats {
  self: BaseTypeSeqsStats with Statistics =>
  val uniqueTypesView     = newView      ("#unique types")(symbolTable.howManyUniqueTypes)
  val rawTypeCount        = newCounter   ("#raw type creations")
  val subtypeCount        = newCounter   ("#subtype ops")
  val sametypeCount       = newCounter   ("#sametype ops")
  val lubCount            = newCounter   ("#toplevel lubs/glbs")
  val nestedLubCount      = newCounter   ("#all lubs/glbs")
  val findMemberCount     = newCounter   ("#findMember ops")
  val findMembersCount    = newCounter   ("#findMembers ops")
  val noMemberCount       = newSubCounter("  of which not found", findMemberCount)
  val multMemberCount     = newSubCounter("  of which multiple overloaded", findMemberCount)
  val typerNanos          = newTimer     ("time spent typechecking", "typer")
  val lubNanos            = newStackableTimer("time spent in lubs", typerNanos)
  val subtypeNanos        = newStackableTimer("time spent in <:<", typerNanos)
  val findMemberNanos     = newStackableTimer("time spent in findmember", typerNanos)
  val findMembersNanos    = newStackableTimer("time spent in findmembers", typerNanos)
  val asSeenFromNanos     = newStackableTimer("time spent in asSeenFrom", typerNanos)
  val baseTypeSeqNanos    = newStackableTimer("time spent in baseTypeSeq", typerNanos)
  val baseClassesNanos    = newStackableTimer("time spent in baseClasses", typerNanos)
  val compoundBaseTypeSeqCount = newSubCounter("  of which for compound types", baseTypeSeqCount)
  val typerefBaseTypeSeqCount = newSubCounter("  of which for typerefs", baseTypeSeqCount)
  val singletonBaseTypeSeqCount = newSubCounter("  of which for singletons", baseTypeSeqCount)
  val typeOpsStack = newTimerStack()
}

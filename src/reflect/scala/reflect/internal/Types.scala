/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package reflect
package internal

import scala.collection.{ mutable, immutable, generic }
import scala.ref.WeakReference
import mutable.ListBuffer
import Flags._
import scala.util.control.ControlThrowable
import scala.annotation.tailrec
import util.Statistics
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
  case OverloadedType(pre, tparams, alts) =>
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
  import TypesStats._

  private var explainSwitch = false
  private final val emptySymbolSet = immutable.Set.empty[Symbol]

  private final val traceTypeVars = sys.props contains "scalac.debug.tvar"
  private final val breakCycles = settings.breakCycles.value
  /** In case anyone wants to turn on type parameter bounds being used
   *  to seed type constraints.
   */
  private final val propagateParameterBoundsToTypeVars = sys.props contains "scalac.debug.prop-constraints"
  private final val sharperSkolems = sys.props contains "scalac.experimental.sharper-skolems"

  protected val enableTypeVarExperimentals = settings.Xexperimental.value

  /** Caching the most recent map has a 75-90% hit rate. */
  private object substTypeMapCache {
    private[this] var cached: SubstTypeMap = new SubstTypeMap(Nil, Nil)

    def apply(from: List[Symbol], to: List[Type]): SubstTypeMap = {
      if ((cached.from ne from) || (cached.to ne to))
        cached = new SubstTypeMap(from, to)

      cached
    }
  }

  /** The current skolemization level, needed for the algorithms
   *  in isSameType, isSubType that do constraint solving under a prefix.
   */
  private var _skolemizationLevel = 0
  def skolemizationLevel = _skolemizationLevel
  def skolemizationLevel_=(value: Int) = _skolemizationLevel = value

  /** A map from lists to compound types that have the given list as parents.
   *  This is used to avoid duplication in the computation of base type sequences and baseClasses.
   *  It makes use of the fact that these two operations depend only on the parents,
   *  not on the refinement.
   */
  private val _intersectionWitness = perRunCaches.newWeakMap[List[Type], WeakReference[Type]]()
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
    override def boundSyms = underlying.boundSyms
    override def typeSymbol = underlying.typeSymbol
    override def typeSymbolDirect = underlying.typeSymbolDirect
    override def widen = underlying.widen
    override def typeOfThis = underlying.typeOfThis
    override def bounds = underlying.bounds
    override def parents = underlying.parents
    override def prefix = underlying.prefix
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
    protected def maybeRewrap(newtp: Type) = (
      if (newtp eq underlying) this
      // BoundedWildcardTypes reach here during erroneous compilation: neg/t6258
      // Higher-kinded exclusion is because [x]CC[x] compares =:= to CC: pos/t3800
      // Otherwise, if newtp =:= underlying, don't rewrap it.
      else if (!newtp.isWildcard && !newtp.isHigherKinded && (newtp =:= underlying)) this
      else rewrap(newtp)
    )
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
    override def load(sym: Symbol) { underlying.load(sym) }
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
        // http://groups.google.com/group/scala-internals/browse_thread/thread/6d3277ae21b6d581
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

    /** Is this type higher-kinded, i.e., is it a type constructor @M */
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
    def isFinalType = typeSymbol.hasOnlyBottomSubclasses && prefix.isStable

    /** Is this type completed (i.e. not a lazy type)? */
    def isComplete: Boolean = true

    /** If this is a lazy type, assign a new type to `sym`. */
    def complete(sym: Symbol) {}

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
    def bounds: TypeBounds = TypeBounds(this, this)

    /** For a class or intersection type, its parents.
     *  For a TypeBounds type, the parents of its hi bound.
     *  inherited by typerefs, singleton types, and refinement types,
     *  The empty list for all other types */
    def parents: List[Type] = List()

    /** For a class with nonEmpty parents, the first parent.
     *  Otherwise some specific fixed top type.
     */
    def firstParent = if (parents.nonEmpty) parents.head else ObjectTpe

    /** For a typeref or single-type, the prefix of the normalized type (@see normalize).
     *  NoType for all other types. */
    def prefix: Type = NoType

    /** A chain of all typeref or singletype prefixes of this type, longest first.
     *  (Only used from safeToString.)
     */
    def prefixChain: List[Type] = this match {
      case TypeRef(pre, _, _) => pre :: pre.prefixChain
      case SingleType(pre, _) => pre :: pre.prefixChain
      case _ => List()
    }

    /** This type, without its type arguments @M */
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

    /** For a (potentially wrapped) poly or existential type, its bound symbols,
     *  the empty list for all other types */
    def boundSyms: immutable.Set[Symbol] = emptySymbolSet

    /** Replace formal type parameter symbols with actual type arguments.
     *
     * Amounts to substitution except for higher-kinded types. (See overridden method in TypeRef) -- @M
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
     *  might lurk in the upper bounds of an abstract type. See SI-7051.
     */
    def dealiasWiden: Type = (
      if (this ne widen) widen.dealiasWiden
      else if (this ne dealias) dealias.dealiasWiden
      else this
    )

    /** All the types encountered in the course of dealiasing/widening,
     *  including each intermediate beta reduction step (whereas calling
     *  dealias applies as many as possible.)
     */
    def dealiasWidenChain: List[Type] = this :: (
      if (this ne widen) widen.dealiasWidenChain
      else if (this ne betaReduce) betaReduce.dealiasWidenChain
      else Nil
    )

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
    def implicitMembers: Scope = membersBasedOnFlags(BridgeFlags, IMPLICIT)

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
      val start = if (Statistics.canEnable) Statistics.pushTimer(typeOpsStack, asSeenFromNanos)  else null
      try {
        val trivial = (
             this.isTrivial
          || phase.erasedTypes && pre.typeSymbol != ArrayClass
          || skipPrefixOf(pre, clazz)
        )
        if (trivial) this
        else {
          val m     = newAsSeenFromMap(pre.normalize, clazz)
          val tp    = m(this)
          val tp1   = existentialAbstraction(m.capturedParams, tp)

          if (m.capturedSkolems.isEmpty) tp1
          else deriveType(m.capturedSkolems, _.cloneSymbol setFlag CAPTURED)(tp1)
        }
      } finally if (Statistics.canEnable) Statistics.popTimer(typeOpsStack, start)
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
      require(sym ne NoSymbol, this)
      sym.info.asSeenFrom(this, sym.owner)
    }

    /** The type of `sym`, seen as a member of this type. */
    def memberType(sym: Symbol): Type = sym match {
      case meth: MethodSymbol =>
        meth.typeAsMemberOf(this)
      case _ =>
        computeMemberType(sym)
    }

    def computeMemberType(sym: Symbol): Type = sym.tpeHK match { //@M don't prematurely instantiate higher-kinded types, they will be instantiated by transform, typedTypeApply, etc. when really necessary
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
      else new SubstSymMap(from, to) apply this

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
      def foreach[U](f: Type => U): Unit = collect(Type.this) foreach f
      def map[T](f: Type => T): List[T]  = collect(Type.this) map f
    }

    @inline final def orElse(alt: => Type): Type = if (this ne NoType) this else alt

    /** Returns optionally first type (in a preorder traversal) which satisfies predicate `p`,
     *  or None if none exists.
     */
    def find(p: Type => Boolean): Option[Type] = new FindTypeCollector(p).collect(this)

    /** Apply `f` to each part of this type */
    def foreach(f: Type => Unit) { new ForEachTypeTraverser(f).traverse(this) }

    /** Apply `pf` to each part of this type on which the function is defined */
    def collect[T](pf: PartialFunction[Type, T]): List[T] = new CollectTypeCollector(pf).collect(this)

    /** Apply `f` to each part of this type; children get mapped before their parents */
    def map(f: Type => Type): Type = new TypeMap {
      def apply(x: Type) = f(mapOver(x))
    } apply this

    /** Is there part of this type which satisfies predicate `p`? */
    def exists(p: Type => Boolean): Boolean = !find(p).isEmpty

    /** Does this type contain a reference to this symbol? */
    def contains(sym: Symbol): Boolean = new ContainsCollector(sym).collect(this)

    /** Is this type a subtype of that type? */
    def <:<(that: Type): Boolean = {
      if (Statistics.canEnable) stat_<:<(that)
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
        val that1 = existentialAbstraction(args map (_.typeSymbol), that)
        (that ne that1) && (this <:< that1) && {
          debuglog(s"$this.matchesPattern($that) depended on discarding args and testing <:< $that1")
          true
        }
      case _ =>
        false
    })

    def stat_<:<(that: Type): Boolean = {
      if (Statistics.canEnable) Statistics.incCounter(subtypeCount)
      val start = if (Statistics.canEnable) Statistics.pushTimer(typeOpsStack, subtypeNanos) else null
      val result =
        (this eq that) ||
        (if (explainSwitch) explain("<:", isSubType(_: Type, _: Type), this, that)
         else isSubType(this, that))
      if (Statistics.canEnable) Statistics.popTimer(typeOpsStack, start)
      result
    }

    /** Is this type a weak subtype of that type? True also for numeric types, i.e. Int weak_<:< Long.
     */
    def weak_<:<(that: Type): Boolean = {
      if (Statistics.canEnable) Statistics.incCounter(subtypeCount)
      val start = if (Statistics.canEnable) Statistics.pushTimer(typeOpsStack, subtypeNanos) else null
      val result =
        ((this eq that) ||
         (if (explainSwitch) explain("weak_<:", isWeakSubType, this, that)
          else isWeakSubType(this, that)))
      if (Statistics.canEnable) Statistics.popTimer(typeOpsStack, start)
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

    /** The shortest sorted upwards closed array of types that contains
     *  this type as first element.
     *
     *  A list or array of types ts is upwards closed if
     *
     *    for all t in ts:
     *      for all typerefs p.s[args] such that t <: p.s[args]
     *      there exists a typeref p'.s[args'] in ts such that
     *      t <: p'.s['args] <: p.s[args],
     *
     *      and
     *
     *      for all singleton types p.s such that t <: p.s
     *      there exists a singleton type p'.s in ts such that
     *      t <: p'.s <: p.s
     *
     *  Sorting is with respect to Symbol.isLess() on type symbols.
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
    def baseTypeIndex(sym: Symbol): Int = {
      val bts = baseTypeSeq
      var lo = 0
      var hi = bts.length - 1
      while (lo <= hi) {
        val mid = (lo + hi) / 2
        val btssym = bts.typeSymbol(mid)
        if (sym == btssym) return mid
        else if (sym isLess btssym) hi = mid - 1
        else if (btssym isLess sym) lo = mid + 1
        else abort("sym is neither `sym == btssym`, `sym isLess btssym` nor `btssym isLess sym`")
      }
      -1
    }

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
    def prefixString = trimPrefix(toString) + "#"

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
    def load(sym: Symbol) {}

    private def findDecl(name: Name, excludedFlags: Int): Symbol = {
      var alts: List[Symbol] = List()
      var sym: Symbol = NoSymbol
      var e: ScopeEntry = decls.lookupEntry(name)
      while (e ne null) {
        if (!e.sym.hasFlag(excludedFlags.toLong)) {
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
      else suspendingTypeVars(typeVarsInType(this))(findMembersInternal)
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
      def findMemberInternal = new FindMember(this, name, excludedFlags, requiredFlags, stableOnly).apply()

      if (this.isGround) findMemberInternal
      else suspendingTypeVars(typeVarsInType(this))(findMemberInternal)
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

    /** The kind of this type; used for debugging */
    def kind: String = "unknown type of class "+getClass()
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
    override def widen: Type = underlying.widen
    override def baseTypeSeq: BaseTypeSeq = {
      if (Statistics.canEnable) Statistics.incCounter(singletonBaseTypeSeqCount)
      underlying.baseTypeSeq prepend this
    }
    override def isHigherKinded = false // singleton type classifies objects, thus must be kind *
    override def safeToString: String = {
      // Avoiding printing Predef.type and scala.package.type as "type",
      // since in all other cases we omit those prefixes.
      val pre = underlying.typeSymbol.skipPackageObject
      if (pre.isOmittablePrefix) pre.fullName + ".type"
      else prefixString + "type"
    }
/*
    override def typeOfThis: Type = typeSymbol.typeOfThis
    override def bounds: TypeBounds = TypeBounds(this, this)
    override def prefix: Type = NoType
    override def typeArgs: List[Type] = List()
    override def typeParams: List[Symbol] = List()
*/
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
  case object WildcardType extends Type {
    override def isWildcard = true
    override def safeToString: String = "?"
    override def kind = "WildcardType"
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
  case class BoundedWildcardType(override val bounds: TypeBounds) extends Type with BoundedWildcardTypeApi {
    override def isWildcard = true
    override def safeToString: String = "?" + bounds
    override def kind = "BoundedWildcardType"
  }

  object BoundedWildcardType extends BoundedWildcardTypeExtractor

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
      // SI-6640 allow StubSymbols to reveal what's missing from the classpath before we trip the assertion.
      sym.failIfStub()
      abort(s"ThisType($sym) for sym which is not a class")
    }

    override def isTrivial: Boolean = sym.isPackageClass
    override def typeSymbol = sym
    override def underlying: Type = sym.typeOfThis
    override def isHigherKinded = sym.isRefinementClass && underlying.isHigherKinded
    override def prefixString =
      if (settings.debug) sym.nameString + ".this."
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
      else if (sym.isImplClass) sym.typeOfThis
      else sym.tpe_*
    )
  }

  /** A class for singleton types of the form `<prefix>.<sym.name>.type`.
   *  Cannot be created directly; one should always use `singleType` for creation.
   */
  abstract case class SingleType(pre: Type, sym: Symbol) extends SingletonType with SingleTypeApi {
    private var trivial: ThreeValue = UNKNOWN
    override def isTrivial: Boolean = {
      if (trivial == UNKNOWN) trivial = fromBoolean(pre.isTrivial)
      toBoolean(trivial)
    }
    override def isGround = sym.isPackageClass || pre.isGround

    private[reflect] var underlyingCache: Type = NoType
    private[reflect] var underlyingPeriod = NoPeriod
    override def underlying: Type = {
      val cache = underlyingCache
      if (underlyingPeriod == currentPeriod && cache != null) cache
      else {
        defineUnderlyingOfSingleType(this)
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
  }

  final class UniqueSingleType(pre: Type, sym: Symbol) extends SingleType(pre, sym)

  object SingleType extends SingleTypeExtractor {
    def apply(pre: Type, sym: Symbol): Type = {
      unique(new UniqueSingleType(pre, sym))
    }
  }

  protected def defineUnderlyingOfSingleType(tpe: SingleType) = {
    val period = tpe.underlyingPeriod
    if (period != currentPeriod) {
      tpe.underlyingPeriod = currentPeriod
      if (!isValid(period)) {
        // [Eugene to Paul] needs review
        tpe.underlyingCache = if (tpe.sym == NoSymbol) ThisType(rootMirror.RootClass) else tpe.pre.memberType(tpe.sym).resultType
        assert(tpe.underlyingCache ne tpe, tpe)
      }
    }
  }

  abstract case class SuperType(thistpe: Type, supertpe: Type) extends SingletonType with SuperTypeApi {
    private var trivial: ThreeValue = UNKNOWN
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
    def containsType(that: Type) = that match {
      case TypeBounds(_, _) => that <:< this
      case _                => lo <:< that && that <:< hi
    }
    private def emptyLowerBound = typeIsNothing(lo) || lo.isWildcard
    private def emptyUpperBound = typeIsAny(hi) || hi.isWildcard
    def isEmptyBounds = emptyLowerBound && emptyUpperBound

    override def safeToString = scalaNotation(_.toString)

    /** Bounds notation used in Scala syntax.
      * For example +This <: scala.collection.generic.Sorted[K,This].
      */
    private[internal] def scalaNotation(typeString: Type => String): String = {
      (if (emptyLowerBound) "" else " >: " + typeString(lo)) +
      (if (emptyUpperBound) "" else " <: " + typeString(hi))
    }
    /** Bounds notation used in http://adriaanm.github.com/files/higher.pdf.
      * For example *(scala.collection.generic.Sorted[K,This]).
      */
    private[internal] def starNotation(typeString: Type => String): String = {
      if (emptyLowerBound && emptyUpperBound) ""
      else if (emptyLowerBound) "(" + typeString(hi) + ")"
      else "(%s, %s)" format (typeString(lo), typeString(hi))
    }
    override def kind = "TypeBoundsType"
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

    protected def shouldForceScope = settings.debug || parents.isEmpty || !decls.isEmpty
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
        if (tpe.parents exists typeContainsTypeVar) {
          // rename type vars to fresh type params, take base type sequence of
          // resulting type, and rename back all the entries in that sequence
          var tvs = Set[TypeVar]()
          for (p <- tpe.parents)
            for (t <- p) t match {
              case tv: TypeVar => tvs += tv
              case _ =>
            }
          val varToParamMap: Map[Type, Symbol] =
            mapFrom[TypeVar, Type, Symbol](tvs.toList)(_.origin.typeSymbol.cloneSymbol)
          val paramToVarMap = varToParamMap map (_.swap)
          val varToParam = new TypeMap {
            def apply(tp: Type) = varToParamMap get tp match {
              case Some(sym) => sym.tpe_*
              case _ => mapOver(tp)
            }
          }
          val paramToVar = new TypeMap {
            def apply(tp: Type) = tp match {
              case TypeRef(_, tsym, _) if paramToVarMap.isDefinedAt(tsym) => paramToVarMap(tsym)
              case _ => mapOver(tp)
            }
          }
          val bts = copyRefinedType(tpe.asInstanceOf[RefinedType], tpe.parents map varToParam, varToParam mapOver tpe.decls).baseTypeSeq
          tpe.baseTypeSeqCache = bts lateMap paramToVar
        } else {
          if (Statistics.canEnable) Statistics.incCounter(compoundBaseTypeSeqCount)
          val start = if (Statistics.canEnable) Statistics.pushTimer(typeOpsStack, baseTypeSeqNanos) else null
          try {
            tpe.baseTypeSeqCache = undetBaseTypeSeq
            tpe.baseTypeSeqCache =
              if (tpe.typeSymbol.isRefinementClass)
                tpe.memo(compoundBaseTypeSeq(tpe))(_.baseTypeSeq updateHead tpe.typeSymbol.tpe_*)
              else
                compoundBaseTypeSeq(tpe)
          } finally {
            if (Statistics.canEnable) Statistics.popTimer(typeOpsStack, start)
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
    //Console.println("baseTypeSeq(" + typeSymbol + ") = " + baseTypeSeqCache.toList);//DEBUG
    if (tpe.baseTypeSeqCache eq undetBaseTypeSeq)
      throw new TypeError("illegal cyclic inheritance involving " + tpe.typeSymbol)
  }

  object baseClassesCycleMonitor {
    private var open: List[Symbol] = Nil
    @inline private def cycleLog(msg: => String) {
      if (settings.debug)
        Console.err.println(msg)
    }
    def size = open.size
    def push(clazz: Symbol) {
      cycleLog("+ " + ("  " * size) + clazz.fullNameString)
      open ::= clazz
    }
    def pop(clazz: Symbol) {
      assert(open.head eq clazz, (clazz, open))
      open = open.tail
    }
    def isOpen(clazz: Symbol) = open contains clazz
  }

  protected def defineBaseClassesOfCompoundType(tpe: CompoundType) {
    def define() = defineBaseClassesOfCompoundType(tpe, force = false)
    if (!breakCycles || isPastTyper) define()
    else tpe match {
      // non-empty parents helpfully excludes all package classes
      case tpe @ ClassInfoType(_ :: _, _, clazz) if !clazz.isAnonOrRefinementClass =>
        // Cycle: force update
        if (baseClassesCycleMonitor isOpen clazz)
          defineBaseClassesOfCompoundType(tpe, force = true)
        else {
          baseClassesCycleMonitor push clazz
          try define()
          finally baseClassesCycleMonitor pop clazz
        }
      case _ =>
        define()
    }
  }
  private def defineBaseClassesOfCompoundType(tpe: CompoundType, force: Boolean) {
    val period = tpe.baseClassesPeriod
    if (period == currentPeriod) {
      if (force && breakCycles) {
        def what = tpe.typeSymbol + " in " + tpe.typeSymbol.owner.fullNameString
        val bcs  = computeBaseClasses(tpe)
        tpe.baseClassesCache = bcs
        warning(s"Breaking cycle in base class computation of $what ($bcs)")
      }
    }
    else {
      tpe.baseClassesPeriod = currentPeriod
      if (!isValidForBaseClasses(period)) {
        val start = if (Statistics.canEnable) Statistics.pushTimer(typeOpsStack, baseClassesNanos) else null
        try {
          tpe.baseClassesCache = null
          tpe.baseClassesCache = tpe.memo(computeBaseClasses(tpe))(tpe.typeSymbol :: _.baseClasses.tail)
        }
        finally {
          if (Statistics.canEnable) Statistics.popTimer(typeOpsStack, start)
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
      parents.nonEmpty &&
      (parents forall typeIsHigherKinded) &&
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

    private var normalized: Type = _
    private def normalizeImpl = {
      // TODO see comments around def intersectionType and def merge
      def flatten(tps: List[Type]): List[Type] = tps flatMap { case RefinedType(parents, ds) if ds.isEmpty => flatten(parents) case tp => List(tp) }
      val flattened = flatten(parents).distinct
      if (decls.isEmpty && hasLength(flattened, 1)) {
        flattened.head
      } else if (flattened != parents) {
        refinedType(flattened, if (typeSymbol eq NoSymbol) NoSymbol else typeSymbol.owner, decls, NoPosition)
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
  }

  final class RefinedType0(parents: List[Type], decls: Scope, clazz: Symbol) extends RefinedType(parents, decls) {
    override def typeSymbol = clazz
  }

  object RefinedType extends RefinedTypeExtractor {
    def apply(parents: List[Type], decls: Scope, clazz: Symbol): RefinedType =
      new RefinedType0(parents, decls, clazz)
  }

  /** Overridden in reflection compiler */
  def validateClassInfo(tp: ClassInfoType) {}

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

    private type RefMap = Map[Symbol, immutable.Set[Symbol]]

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
    private var refs: Array[RefMap] = _

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
    private def addRef(which: Int, from: Symbol, to: Symbol) {
      refs(which) = refs(which) + (from -> (getRefs(which, from) + to))
    }

    /** Augment existing refs map with references <pre>from -> sym</pre>, for
     *  all elements <pre>sym</pre> of set `to`.
     *  @param  which <- {NonExpansive, Expansive}
     */
    private def addRefs(which: Int, from: Symbol, to: Set[Symbol]) {
      refs(which) = refs(which) + (from -> (getRefs(which, from) ++ to))
    }

    /** The ClassInfoType which belongs to the class containing given type parameter
     */
    private def classInfo(tparam: Symbol): ClassInfoType =
      tparam.owner.info.resultType match {
        case ci: ClassInfoType => ci
        case _ => classInfo(ObjectClass) // something's wrong; fall back to safe value
                                         // (this can happen only for erroneous programs).
      }

    private object enterRefs extends TypeMap {
      private var tparam: Symbol = _

      def apply(tp: Type): Type = {
        tp match {
          case tr @ TypeRef(_, sym, args) if args.nonEmpty =>
            val tparams = tr.initializedTypeParams
            if (settings.debug && !sameLength(tparams, args))
              devWarning(s"Mismatched zip in computeRefs(): ${sym.info.typeParams}, $args")

            foreach2(tparams, args) { (tparam1, arg) =>
              if (arg contains tparam) {
                addRef(NonExpansive, tparam, tparam1)
                if (arg.typeSymbol != tparam)
                  addRef(Expansive, tparam, tparam1)
              }
            }
          case _ =>
        }
        mapOver(tp)
      }
      def enter(tparam0: Symbol, parent: Type) {
        this.tparam = tparam0
        this(parent)
      }
    }

    /** Compute initial (one-step) references and set state to `Initializing`.
     */
    private def computeRefs() {
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
    override protected def shouldForceScope = settings.debug || decls.size > 1
    override protected def scopeString      = initDecls.mkString(" {\n  ", "\n  ", "\n}")
    override def safeToString               = if (shouldForceScope) formattedToString else super.safeToString
  }

  object ClassInfoType extends ClassInfoTypeExtractor

  class PackageClassInfoType(decls: Scope, clazz: Symbol)
  extends ClassInfoType(List(), decls, clazz)

  /** A class representing a constant type.
   */
  abstract case class ConstantType(value: Constant) extends SingletonType with ConstantTypeApi {
    override def underlying: Type = value.tpe
    assert(underlying.typeSymbol != UnitClass)
    override def isTrivial: Boolean = true
    override def deconst: Type = underlying.deconst
    override def safeToString: String =
      underlying.toString + "(" + value.escapedStringValue + ")"
    override def kind = "ConstantType"
  }

  final class UniqueConstantType(value: Constant) extends ConstantType(value)

  object ConstantType extends ConstantTypeExtractor {
    def apply(value: Constant) = unique(new UniqueConstantType(value))
  }

  /* Syncnote: The `volatile` var and `pendingVolatiles` mutable set need not be protected
   * with synchronized, because they are accessed only from isVolatile, which is called only from
   * Typer.
   */
  private var volatileRecursions: Int = 0
  private val pendingVolatiles = new mutable.HashSet[Symbol]

  class ArgsTypeRef(pre0: Type, sym0: Symbol, args0: List[Type]) extends TypeRef(pre0, sym0, args0) {
    require(args0 ne Nil, this)

    /** No unapplied type params size it has (should have) equally as many args. */
    override def isHigherKinded = false
    override def typeParams = Nil

    override def transform(tp: Type): Type = {
      // This situation arises when a typevar is encountered for which
      // too little information is known to determine its kind, and
      // it later turns out not to have kind *. See SI-4070.  Only
      // logging it for now.
      val tparams = sym.typeParams
      if (tparams.size != args.size)
        devWarning(s"$this.transform($tp), but tparams.isEmpty and args=$args")
      def asSeenFromInstantiated(tp: Type) =
        asSeenFromOwner(tp).instantiateTypeParams(tparams, args)
      // If we're called with a poly type, and we were to run the `asSeenFrom`, over the entire
      // type, we can end up with new symbols for the type parameters (clones from TypeMap).
      // The subsequent substitution of type arguments would fail. This problem showed up during
      // the fix for SI-8046, however the solution taken there wasn't quite right, and led to
      // SI-8170.
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
      tp match {
        case PolyType(`tparams`, result) => PolyType(tparams, asSeenFromInstantiated(result))
        case _                           => asSeenFromInstantiated(tp)
      }
    }

    // note: does not go through typeRef. There's no need to because
    // neither `pre` nor `sym` changes.  And there's a performance
    // advantage to call TypeRef directly.
    override def typeConstructor = TypeRef(pre, sym, Nil)
  }

  class ModuleTypeRef(pre0: Type, sym0: Symbol) extends NoArgsTypeRef(pre0, sym0) with ClassTypeRef {
    require(sym.isModuleClass, sym)
    private[this] var narrowedCache: Type = _
    override def narrow = {
      if (narrowedCache eq null)
        narrowedCache = singleType(pre, sym.sourceModule)

      narrowedCache
    }
    override protected def finishPrefix(rest: String) = objectPrefix + rest
    override def directObjectString = super.safeToString
    override def toLongString = toString
    override def safeToString = prefixString + "type"
    override def prefixString = if (sym.isOmittablePrefix) "" else prefix.prefixString + sym.nameString + "."
  }
  class PackageTypeRef(pre0: Type, sym0: Symbol) extends ModuleTypeRef(pre0, sym0) {
    require(sym.isPackageClass, sym)
    override protected def finishPrefix(rest: String) = packagePrefix + rest
  }
  class RefinementTypeRef(pre0: Type, sym0: Symbol) extends NoArgsTypeRef(pre0, sym0) with ClassTypeRef {
    require(sym.isRefinementClass, sym)

    // I think this is okay, but see #1241 (r12414), #2208, and typedTypeConstructor in Typers
    override protected def normalizeImpl: Type = sym.info.normalize
    override protected def finishPrefix(rest: String) = "" + thisInfo
  }

  class NoArgsTypeRef(pre0: Type, sym0: Symbol) extends TypeRef(pre0, sym0, Nil) {
    // A reference (in a Scala program) to a type that has type parameters, but where the reference
    // does not include type arguments. Note that it doesn't matter whether the symbol refers
    // to a java or scala symbol, but it does matter whether it occurs in java or scala code.
    // TypeRefs w/o type params that occur in java signatures/code are considered raw types, and are
    // represented as existential types.
    override def isHigherKinded = (typeParams ne Nil)
    override def typeParams     = if (isDefinitionsInitialized) sym.typeParams else sym.unsafeTypeParams
    private def isRaw           = !phase.erasedTypes && isRawIfWithoutArgs(sym)

    override def instantiateTypeParams(formals: List[Symbol], actuals: List[Type]): Type =
      if (isHigherKinded) {
        if (sameLength(formals intersect typeParams, typeParams))
          copyTypeRef(this, pre, sym, actuals)
        // partial application (needed in infer when bunching type arguments from classes and methods together)
        else
          copyTypeRef(this, pre, sym, dummyArgs).instantiateTypeParams(formals, actuals)
      }
      else
        super.instantiateTypeParams(formals, actuals)

    override def transform(tp: Type): Type = {
      val res = asSeenFromOwner(tp)
      if (isHigherKinded && !isRaw)
        res.instantiateTypeParams(typeParams, dummyArgs)
      else
        res
    }

    override def transformInfo(tp: Type): Type =
      appliedType(asSeenFromOwner(tp), dummyArgs)

    override def narrow =
      if (sym.isModuleClass) singleType(pre, sym.sourceModule)
      else super.narrow

    override def typeConstructor = this
    // eta-expand, subtyping relies on eta-expansion of higher-kinded types

    override protected def normalizeImpl: Type =
      if (isHigherKinded) etaExpand else super.normalizeImpl
  }

  trait ClassTypeRef extends TypeRef {
    // !!! There are scaladoc-created symbols arriving which violate this require.
    // require(sym.isClass, sym)

    override def baseType(clazz: Symbol): Type =
      if (sym == clazz) this
      else transform(sym.info.baseType(clazz))
  }

  trait NonClassTypeRef extends TypeRef {
    require(sym.isNonClassType, sym)

    /* Syncnote: These are pure caches for performance; no problem to evaluate these
     * several times. Hence, no need to protected with synchronized in a multi-threaded
     * usage scenario.
     */
    private var relativeInfoCache: Type = _
    private var relativeInfoPeriod: Period = NoPeriod

    private[Types] def relativeInfo = /*trace(s"relativeInfo(${safeToString}})")*/{
      if (relativeInfoPeriod != currentPeriod) {
        val memberInfo = pre.memberInfo(sym)
        relativeInfoCache = transformInfo(memberInfo)
        relativeInfoPeriod = currentPeriod
      }
      relativeInfoCache
    }

    override def baseType(clazz: Symbol): Type =
      if (sym == clazz) this else baseTypeOfNonClassTypeRef(this, clazz)
  }

  protected def baseTypeOfNonClassTypeRef(tpe: NonClassTypeRef, clazz: Symbol) = try {
    basetypeRecursions += 1
    if (basetypeRecursions < LogPendingBaseTypesThreshold)
      tpe.relativeInfo.baseType(clazz)
    else if (pendingBaseTypes contains tpe)
      if (clazz == AnyClass) clazz.tpe else NoType
    else
      try {
        pendingBaseTypes += tpe
        tpe.relativeInfo.baseType(clazz)
      } finally {
        pendingBaseTypes -= tpe
      }
  } finally {
    basetypeRecursions -= 1
  }

  trait AliasTypeRef extends NonClassTypeRef {
    require(sym.isAliasType, sym)

    override def dealias    = if (typeParamsMatchArgs) betaReduce.dealias else super.dealias
    override def narrow     = normalize.narrow
    override def thisInfo   = normalize
    override def prefix     = if (this ne normalize) normalize.prefix else pre
    override def termSymbol = if (this ne normalize) normalize.termSymbol else super.termSymbol
    override def typeSymbol = if (this ne normalize) normalize.typeSymbol else sym

    // beta-reduce, but don't do partial application -- cycles have been checked in typeRef
    override protected def normalizeImpl =
      if (typeParamsMatchArgs) betaReduce.normalize
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
    override def betaReduce = transform(sym.info.resultType)

    /** SI-3731, SI-8177: when prefix is changed to `newPre`, maintain consistency of prefix and sym
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

    /** Syncnote: Pure performance caches; no need to synchronize in multi-threaded environment
     */
    private var symInfoCache: Type = _
    private var thisInfoCache: Type = _

    override def thisInfo   = {
      val symInfo = sym.info
      if (thisInfoCache == null || (symInfo ne symInfoCache)) {
        symInfoCache = symInfo
        thisInfoCache = transformInfo(symInfo) match {
          // If a subtyping cycle is not detected here, we'll likely enter an infinite
          // loop before a sensible error can be issued.  SI-5093 is one example.
          case x: SubType if x.supertype eq this =>
            throw new RecoverableCyclicReference(sym)
          case tp => tp
        }
      }
      thisInfoCache
    }
    override def bounds   = thisInfo.bounds
    override protected[Types] def baseTypeSeqImpl: BaseTypeSeq = transform(bounds.hi).baseTypeSeq prepend this
    override def kind = "AbstractTypeRef"
  }

  /** A class for named types of the form
   *    `<prefix>.<sym.name>[args]`
   *  Cannot be created directly; one should always use `typeRef`
   *  for creation. (@M: Otherwise hashing breaks)
   *
   * @M: a higher-kinded type is represented as a TypeRef with sym.typeParams.nonEmpty, but args.isEmpty
   */
  abstract case class TypeRef(pre: Type, sym: Symbol, args: List[Type]) extends UniqueType with TypeRefApi {
    private var trivial: ThreeValue = UNKNOWN
    override def isTrivial: Boolean = {
      if (trivial == UNKNOWN)
        trivial = fromBoolean(!sym.isTypeParameter && pre.isTrivial && areTrivialTypes(args))
      toBoolean(trivial)
    }
    private[scala] def invalidateCaches(): Unit = {
      parentsPeriod = NoPeriod
      baseTypeSeqPeriod = NoPeriod
    }
    private[reflect] var parentsCache: List[Type]      = _
    private[reflect] var parentsPeriod                 = NoPeriod
    private[reflect] var baseTypeSeqCache: BaseTypeSeq = _
    private[reflect] var baseTypeSeqPeriod             = NoPeriod
    private var normalized: Type                       = _

    //OPT specialize hashCode
    override final def computeHashCode = {
      import scala.util.hashing.MurmurHash3._
      val hasArgs = args ne Nil
      var h = productSeed
      h = mix(h, pre.hashCode)
      h = mix(h, sym.hashCode)
      if (hasArgs)
        finalizeHash(mix(h, args.hashCode()), 3)
      else
        finalizeHash(h, 2)
    }

    // @M: propagate actual type params (args) to `tp`, by replacing
    // formal type parameters with actual ones. If tp is higher kinded,
    // the "actual" type arguments are types that simply reference the
    // corresponding type parameters (unbound type variables)
    def transform(tp: Type): Type

    // eta-expand, subtyping relies on eta-expansion of higher-kinded types
    protected def normalizeImpl: Type = if (isHigherKinded) etaExpand else super.normalize

    // TODO: test case that is compiled in a specific order and in different runs
    final override def normalize: Type = {
      // arises when argument-dependent types are approximated (see def depoly in implicits)
      if (pre eq WildcardType) WildcardType
      else if (phase.erasedTypes) normalizeImpl
      else {
        if (normalized eq null)
          normalized = normalizeImpl
        normalized
      }
    }

    override def isGround = (
         sym.isPackageClass
      || pre.isGround && args.forall(_.isGround)
    )

    final override def etaExpand: Type = {
      // must initialise symbol, see test/files/pos/ticket0137.scala
      val tpars = initializedTypeParams
      if (tpars.isEmpty) this
      else typeFunAnon(tpars, copyTypeRef(this, pre, sym, tpars map (_.tpeHK))) // todo: also beta-reduce?
    }

    // only need to rebind type aliases, as typeRef already handles abstract types
    // (they are allowed to be rebound more liberally)
    def coevolveSym(pre1: Type): Symbol = sym

    //@M! use appliedType on the polytype that represents the bounds (or if aliastype, the rhs)
    def transformInfo(tp: Type): Type = appliedType(asSeenFromOwner(tp), args)

    def thisInfo                  = sym.info
    def initializedTypeParams     = sym.info.typeParams
    def typeParamsMatchArgs       = sameLength(initializedTypeParams, args)
    def asSeenFromOwner(tp: Type) = tp.asSeenFrom(pre, sym.owner)

    override def baseClasses      = thisInfo.baseClasses
    override def baseTypeSeqDepth = baseTypeSeq.maxDepth
    override def prefix           = pre
    override def termSymbol       = super.termSymbol
    override def termSymbolDirect = super.termSymbol
    override def typeArgs         = args
    override def typeOfThis       = transform(sym.typeOfThis)
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

    override def decls: Scope = {
      sym.info match {
        case TypeRef(_, sym1, _) =>
          assert(sym1 != sym, this) // @MAT was != typeSymbol
        case _ =>
      }
      thisInfo.decls
    }
    protected[Types] def baseTypeSeqImpl: BaseTypeSeq =
      if (sym.info.baseTypeSeq exists (_.typeSymbolDirect.isAbstractType))
        // SI-8046 base type sequence might have more elements in a subclass, we can't map it element wise.
        transform(sym.info).baseTypeSeq
      else
        // Optimization: no abstract types, we can compute the BTS of this TypeRef as an element-wise map
        //               of the BTS of the referenced symbol.
        sym.info.baseTypeSeq map transform

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
         settings.debug
      || !shorthands(sym.fullName)
      || (sym.ownersIterator exists (s => !s.isClass))
    )
    private def preString  = if (needsPreString) pre.prefixString else ""
    private def argsString = if (args.isEmpty) "" else args.mkString("[", ",", "]")

    private def refinementDecls = fullyInitializeScope(decls) filter (sym => sym.isPossibleInRefinement && sym.isPublic)
    private def refinementString = (
      if (sym.isStructuralRefinement)
        refinementDecls map (_.defString) mkString("{", "; ", "}")
      else ""
    )
    protected def finishPrefix(rest: String) = (
      if (sym.isInitialized && sym.isAnonymousClass && !phase.erasedTypes)
        parentsString(thisInfo.parents) + refinementString
      else rest
    )
    private def noArgsString = finishPrefix(preString + sym.nameString)
    private def tupleTypeString: String = args match {
      case Nil        => noArgsString
      case arg :: Nil => s"($arg,)"
      case _          => args.mkString("(", ", ", ")")
    }
    private def customToString = sym match {
      case RepeatedParamClass | JavaRepeatedParamClass => args.head + "*"
      case ByNameParamClass   => "=> " + args.head
      case _                  =>
        if (isFunctionTypeDirect(this)) {
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
        }
        else if (isTupleTypeDirect(this))
          tupleTypeString
        else if (sym.isAliasType && prefixChain.exists(_.termSymbol.isSynthetic) && (this ne dealias))
          "" + dealias
        else
          ""
    }
    override def safeToString = {
      val custom = if (settings.debug) "" else customToString
      if (custom != "") custom
      else finishPrefix(preString + sym.nameString + argsString)
    }
    override def prefixString = "" + (
      if (settings.debug)
        super.prefixString
      else if (sym.isOmittablePrefix)
        ""
      else if (sym.isPackageClass || sym.isPackageObjectOrClass)
        sym.skipPackageObject.fullName + "."
      else if (isStable && nme.isSingletonName(sym.name))
        tpnme.dropSingletonName(sym.name) + "."
      else
        super.prefixString
    )
    // Suppressing case class copy method which risks subverting our single point of creation.
    private def copy = null
    override def kind = "TypeRef"
  }

  // No longer defined as anonymous classes in `object TypeRef` to avoid an unnecessary outer pointer.
  private final class AliasArgsTypeRef(pre: Type, sym: Symbol, args: List[Type]) extends ArgsTypeRef(pre, sym, args) with AliasTypeRef
  private final class AbstractArgsTypeRef(pre: Type, sym: Symbol, args: List[Type]) extends ArgsTypeRef(pre, sym, args) with AbstractTypeRef
  private final class ClassArgsTypeRef(pre: Type, sym: Symbol, args: List[Type]) extends ArgsTypeRef(pre, sym, args) with ClassTypeRef
  private final class AliasNoArgsTypeRef(pre: Type, sym: Symbol) extends NoArgsTypeRef(pre, sym) with AliasTypeRef
  private final class AbstractNoArgsTypeRef(pre: Type, sym: Symbol) extends NoArgsTypeRef(pre, sym) with AbstractTypeRef
  private final class ClassNoArgsTypeRef(pre: Type, sym: Symbol) extends NoArgsTypeRef(pre, sym) with ClassTypeRef

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

  protected def defineParentsOfTypeRef(tpe: TypeRef) = {
    val period = tpe.parentsPeriod
    if (period != currentPeriod) {
      tpe.parentsPeriod = currentPeriod
      if (!isValidForBaseClasses(period)) {
        tpe.parentsCache = tpe.thisInfo.parents map tpe.transform
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
        if (Statistics.canEnable) Statistics.incCounter(typerefBaseTypeSeqCount)
        val start = if (Statistics.canEnable) Statistics.pushTimer(typeOpsStack, baseTypeSeqNanos) else null
        try {
          tpe.baseTypeSeqCache = undetBaseTypeSeq
          tpe.baseTypeSeqCache = tpe.baseTypeSeqImpl
        } finally {
          if (Statistics.canEnable) Statistics.popTimer(typeOpsStack, start)
        }
      }
    }
    if (tpe.baseTypeSeqCache == undetBaseTypeSeq)
      throw new TypeError("illegal cyclic inheritance involving " + tpe.sym)
  }

  /** A class representing a method type with parameters.
   *  Note that a parameterless method is represented by a NullaryMethodType:
   *
   *    def m(): Int        MethodType(Nil, Int)
   *    def m: Int          NullaryMethodType(Int)
   */
  case class MethodType(override val params: List[Symbol],
                        override val resultType: Type) extends Type with MethodTypeApi {

    private var trivial: ThreeValue = UNKNOWN
    override def isTrivial: Boolean = {
      if (trivial == UNKNOWN) trivial = fromBoolean(isTrivialResult && areTrivialParams(params))
      toBoolean(trivial)
    }

    private def isTrivialResult =
      resultType.isTrivial && (resultType eq resultType.withoutAnnotations)

    private def areTrivialParams(ps: List[Symbol]): Boolean = ps match {
      case p :: rest =>
        p.tpe.isTrivial && !typesContain(paramTypes, p) && !(resultType contains p) &&
        areTrivialParams(rest)
      case _ =>
        true
    }

    def isImplicit = (params ne Nil) && params.head.isImplicit
    def isJava = false // can we do something like for implicits? I.e. do Java methods without parameters need to be recognized?

    //assert(paramTypes forall (pt => !pt.typeSymbol.isImplClass))//DEBUG
    override def paramSectionCount: Int = resultType.paramSectionCount + 1

    override def paramss: List[List[Symbol]] = params :: resultType.paramss

    override def paramTypes = mapList(params)(symTpe) // OPT use mapList rather than .map

    override def boundSyms = resultType.boundSyms ++ params

    override def resultType(actuals: List[Type]) =
      if (isTrivial || phase.erasedTypes) resultType
      else if (/*isDependentMethodType &&*/ sameLength(actuals, params)) {
        val idm = new InstantiateDependentMap(params, actuals)
        val res = idm(resultType)
        existentialAbstraction(idm.existentialsNeeded, res)
      }
      else existentialAbstraction(params, resultType)

    private var isdepmeth: ThreeValue = UNKNOWN
    override def isDependentMethodType: Boolean = {
      if (isdepmeth == UNKNOWN) isdepmeth = fromBoolean(IsDependentCollector.collect(resultType.dealias))
      toBoolean(isdepmeth)
    }

    // implicit args can only be depended on in result type:
    //TODO this may be generalised so that the only constraint is dependencies are acyclic
    def approximate: MethodType = MethodType(params, resultApprox)

    override def safeToString = paramString(this) + resultType

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
  }

  object MethodType extends MethodTypeExtractor

  class JavaMethodType(ps: List[Symbol], rt: Type) extends MethodType(ps, rt) {
    override def isJava = true
  }

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
    override def boundSyms = resultType.boundSyms
    override def safeToString: String = "=> "+ resultType
    override def kind = "NullaryMethodType"
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
    assert(typeParams.nonEmpty, this) // used to be a marker for nullary method type, illegal now (see @NullaryMethodType)

    override def paramSectionCount: Int = resultType.paramSectionCount
    override def paramss: List[List[Symbol]] = resultType.paramss
    override def params: List[Symbol] = resultType.params
    override def paramTypes: List[Type] = resultType.paramTypes
    override def parents: List[Type] = resultType.parents
    override def decls: Scope = resultType.decls
    override def termSymbol: Symbol = resultType.termSymbol
    override def typeSymbol: Symbol = resultType.typeSymbol
    override def boundSyms = immutable.Set[Symbol](typeParams ++ resultType.boundSyms: _*)
    override def prefix: Type = resultType.prefix
    override def baseTypeSeq: BaseTypeSeq = resultType.baseTypeSeq
    override def baseTypeSeqDepth: Depth = resultType.baseTypeSeqDepth
    override def baseClasses: List[Symbol] = resultType.baseClasses
    override def baseType(clazz: Symbol): Type = resultType.baseType(clazz)
    override def narrow: Type = resultType.narrow

    // SI-9475: PolyTypes with dependent method types are still dependent
    override def isDependentMethodType = resultType.isDependentMethodType

    /** @M: typeDefSig wraps a TypeBounds in a PolyType
     *  to represent a higher-kinded type parameter
     *  wrap lo&hi in polytypes to bind variables
     */
    override def bounds: TypeBounds =
      TypeBounds(typeFun(typeParams, resultType.bounds.lo),
                 typeFun(typeParams, resultType.bounds.hi))

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
  }

  object PolyType extends PolyTypeExtractor

  /** A creator for existential types which flattens nested existentials.
   */
  def newExistentialType(quantified: List[Symbol], underlying: Type): Type =
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
    override def bounds = TypeBounds(maybeRewrap(underlying.bounds.lo), maybeRewrap(underlying.bounds.hi))
    override def parents = underlying.parents map maybeRewrap
    override def boundSyms = quantified.toSet
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

    /** [SI-6169, SI-8197 -- companion to SI-1786]
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
     * NOTE: we're only modifying the skolems to avoid leaking the sharper bounds to `quantified` (SI-8283)
     *
     * TODO: figure out how to do this earlier without running into cycles, so this can subsume the fix for SI-1786
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
          && (existentialsInType(tparam.info) intersect quantified).isEmpty
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
        case TypeRef(pre, sym, args) =>
          def isQuantified(tpe: Type): Boolean = {
            (tpe exists (t => qset contains t.typeSymbol)) ||
            tpe.typeSymbol.isRefinementClass && (tpe.parents exists isQuantified)
          }
          val (wildcardArgs, otherArgs) = args partition (arg => qset contains arg.typeSymbol)
          wildcardArgs.distinct == wildcardArgs &&
          !(otherArgs exists (arg => isQuantified(arg))) &&
          !(wildcardArgs exists (arg => isQuantified(arg.typeSymbol.info.bounds))) &&
          !(qset contains sym) &&
          !isQuantified(pre)
        case _ => false
    }
    }

    override def safeToString: String = {
      def clauses = {
        val str = quantified map (_.existentialToString) mkString (" forSome { ", "; ", " }")
        if (settings.explaintypes) "(" + str + ")" else str
      }
      underlying match {
        case TypeRef(pre, sym, args) if !settings.debug && isRepresentableWithWildcards =>
          "" + TypeRef(pre, sym, Nil) + wildcardArgsString(quantified.toSet, args).mkString("[", ", ", "]")
        case MethodType(_, _) | NullaryMethodType(_) | PolyType(_, _) =>
          "(" + underlying + ")" + clauses
        case _ =>
          "" + underlying + clauses
      }
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
        solve(tvars, quantifiedFresh, quantifiedFresh map (_ => Invariant), upper = false, depth) &&
        isWithinBounds(NoPrefix, NoSymbol, quantifiedFresh, tvars map (_.inst))
      }
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
  }

  object HasTypeMember {
    def apply(name: TypeName, tp: Type): Type = {
      val bound = refinedType(List(WildcardType), NoSymbol)
      val bsym = bound.typeSymbol.newAliasType(name)
      bsym setInfo tp
      bound.decls enter bsym
      bound
    }
    def unapply(tp: Type): Option[(TypeName, Type)] = tp match {
      case RefinedType(List(WildcardType), Scope(sym)) => Some((sym.name.toTypeName, sym.info))
      case _ => None
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
      if (traceTypeVars) {
        val s = msg match {
          case ""   => ""
          case str  => "( " + str + " )"
        }
        Console.err.println("[%10s] %-25s%s".format(action, value, s))
      }
      value
    }

    /** Create a new TypeConstraint based on the given symbol.
     */
    private def deriveConstraint(tparam: Symbol): TypeConstraint = {
      /** Must force the type parameter's info at this point
       *  or things don't end well for higher-order type params.
       *  See SI-5359.
       */
      val bounds  = tparam.info.bounds
      /* We can seed the type constraint with the type parameter
       * bounds as long as the types are concrete.  This should lower
       * the complexity of the search even if it doesn't improve
       * any results.
       */
      if (propagateParameterBoundsToTypeVars) {
        val exclude = bounds.isEmptyBounds || (bounds exists typeIsNonClassType)

        if (exclude) new TypeConstraint
        else TypeVar.trace("constraint", "For " + tparam.fullLocationString)(new TypeConstraint(bounds))
      }
      else new TypeConstraint
    }
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
          if (untouchable) new AppliedTypeVar(origin, constr, params zip args) with UntouchableTypeVar
          else new AppliedTypeVar(origin, constr, params zip args)
        }
        else if (args.isEmpty) {
          if (untouchable) new HKTypeVar(origin, constr, params) with UntouchableTypeVar
          else new HKTypeVar(origin, constr, params)
        }
        else throw new Error("Invalid TypeVar construction: " + ((origin, constr, args, params)))
      )

      trace("create", "In " + tv.originLocation)(tv)
    }
    private def createTypeVar(tparam: Symbol, untouchable: Boolean): TypeVar =
      createTypeVar(tparam.tpeHK, deriveConstraint(tparam), Nil, tparam.typeParams, untouchable)
  }

  /** Precondition: params.nonEmpty.  (args.nonEmpty enforced structurally.)
   */
  class HKTypeVar(
    _origin: Type,
    _constr: TypeConstraint,
    override val params: List[Symbol]
  ) extends TypeVar(_origin, _constr) {

    require(params.nonEmpty, this)
    override def isHigherKinded          = true
  }

  /** Precondition: zipped params/args nonEmpty.  (Size equivalence enforced structurally.)
   */
  class AppliedTypeVar(
    _origin: Type,
    _constr: TypeConstraint,
    zippedArgs: List[(Symbol, Type)]
  ) extends TypeVar(_origin, _constr) {

    require(zippedArgs.nonEmpty, this)

    override def params: List[Symbol] = zippedArgs map (_._1)
    override def typeArgs: List[Type] = zippedArgs map (_._2)

    override def safeToString: String = super.safeToString + typeArgs.map(_.safeToString).mkString("[", ", ", "]")
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
    // and TypeRefs based on them get wrongly `uniqued` otherwise. See SI-7226.
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
        TypeVar.trace("applyArgs", "In " + originLocation + ", apply args " + newArgs.mkString(", ") + " to " + originName)(tv)
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

    // When comparing to types containing skolems, remember the highest level
    // of skolemization. If that highest level is higher than our initial
    // skolemizationLevel, we can't re-use those skolems as the solution of this
    // typevar, which means we'll need to repack our inst into a fresh existential.
    // were we compared to skolems at a higher skolemizationLevel?
    // EXPERIMENTAL: value will not be considered unless enableTypeVarExperimentals is true
    // see SI-5729 for why this is still experimental
    private var encounteredHigherLevel = false
    private def shouldRepackType = enableTypeVarExperimentals && encounteredHigherLevel

    // <region name="constraint mutators + undoLog">
    // invariant: before mutating constr, save old state in undoLog
    // (undoLog is used to reset constraints to avoid piling up unrelated ones)
    def setInst(tp: Type): this.type = {
      if (tp eq this) {
        log(s"TypeVar cycle: called setInst passing $this to itself.")
        return this
      }
      undoLog record this
      // if we were compared against later typeskolems, repack the existential,
      // because skolems are only compatible if they were created at the same level
      val res = if (shouldRepackType) repackExistential(tp) else tp
      constr.inst = TypeVar.trace("setInst", "In " + originLocation + ", " + originName + "=" + res)(res)
      this
    }

    def addLoBound(tp: Type, isNumericBound: Boolean = false) {
      assert(tp != this, tp) // implies there is a cycle somewhere (?)
      //println("addLoBound: "+(safeToString, debugString(tp))) //DEBUG
      if (!sharesConstraints(tp)) {
        undoLog record this
        constr.addLoBound(tp, isNumericBound)
      }
    }

    def addHiBound(tp: Type, isNumericBound: Boolean = false) {
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
    private var _suspended: Type = ConstantFalse
    private[Types] def suspended: Boolean = (_suspended: @unchecked) match {
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
      case other: TypeVar => constr == other.constr // SI-8237 avoid cycles. Details in pos/t8237.scala
      case _ => false
    }
    private[Types] def suspended_=(b: Boolean): Unit = _suspended = if (b) ConstantTrue else ConstantFalse
    // SI-7785 Link the suspended attribute of a TypeVar created in, say, a TypeMap (e.g. AsSeenFrom) to its originator
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
      if (isLowerBound)
        assert(tp != this)

      // side effect: adds the type to upper or lower bounds
      def addBound(tp: Type) {
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
        val sym = tp.typeSymbol
        if (sym == NothingClass || sym == AnyClass) { // kind-polymorphic
          // SI-7126 if we register some type alias `T=Any`, we can later end
          // with malformed types like `T[T]` during type inference in
          // `handlePolymorphicCall`. No such problem if we register `Any`.
          addBound(sym.tpe)
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
       *  type parameter we're trying to infer (the result will be sanity-checked later).
       */
      def unifyFull(tpe: Type): Boolean = {
        def unifySpecific(tp: Type) = {
          sameLength(typeArgs, tp.typeArgs) && {
            val lhs = if (isLowerBound) tp.typeArgs else typeArgs
            val rhs = if (isLowerBound) typeArgs else tp.typeArgs
            // This is a higher-kinded type var with same arity as tp.
            // If so (see SI-7517), side effect: adds the type constructor itself as a bound.
            isSubArgs(lhs, rhs, params, AnyDepth) && { addBound(tp.typeConstructor); true }
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
      else isRelatable(tp) && {
        unifySimple || unifyFull(tp) || (
          // only look harder if our gaze is oriented toward Any
          isLowerBound && (
            (tp.parents exists unifyFull) || (
              // @PP: Is it going to be faster to filter out the parents we just checked?
              // That's what's done here but I'm not sure it matters.
              tp.baseTypeSeq.toList.tail filterNot (tp.parents contains _) exists unifyFull
            )
          )
        )
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
        val newInst = wildcardToTypeVarMap(tp)
        (constr isWithinBounds newInst) && {
          setInst(newInst)
          true
        }
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

    private def isSkolemAboveLevel(tp: Type) = tp.typeSymbol match {
      case ts: TypeSkolem => ts.level > level
      case _              => false
    }
    // side-effects encounteredHigherLevel
    private def containsSkolemAboveLevel(tp: Type) =
      (tp exists isSkolemAboveLevel) && { encounteredHigherLevel = true ; true }

     /** Can this variable be related in a constraint to type `tp`?
      *  This is not the case if `tp` contains type skolems whose
      *  skolemization level is higher than the level of this variable.
      */
    def isRelatable(tp: Type) = (
         shouldRepackType               // short circuit if we already know we've seen higher levels
      || !containsSkolemAboveLevel(tp)  // side-effects tracking boolean
      || enableTypeVarExperimentals     // -Xexperimental: always say we're relatable, track consequences
    )

    override def normalize: Type = (
      if (instValid) inst
      // get here when checking higher-order subtyping of the typevar by itself
      // TODO: check whether this ever happens?
      else if (isHigherKinded) etaExpand
      else super.normalize
    )
    override def etaExpand: Type = (
      if (!isHigherKinded) this
      else logResult("Normalizing HK $this")(typeFun(params, applyArgs(params map (_.typeConstructor))))
    )
    override def typeSymbol = origin.typeSymbol

    private def tparamsOfSym(sym: Symbol) = sym.info match {
      case PolyType(tparams, _) if tparams.nonEmpty =>
        tparams map (_.defString) mkString("[", ",", "]")
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
    private def levelString = if (settings.explaintypes) level else ""
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
        TypeVar(origin, constr.cloneInternal, typeArgs, params) // @M TODO: clone args/params?
      )
    }
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

    override def safeToString = annotations.mkString(underlying + " @", " @", "")

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
  }
  /** As with NamedType, used only when calling isApplicable.
   *  Records that the application has a wildcard star (aka _*)
   *  at the end of it.
   */
  case class RepeatedType(tp: Type) extends Type {
    override def safeToString: String = tp + ": _*"
  }

  /** A temporary type representing the erasure of a user-defined value type.
   *  Created during phase erasure, eliminated again in posterasure.
   *
   *  SI-6385 Erasure's creation of bridges considers method signatures `exitingErasure`,
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
    override def complete(sym: Symbol)
    override def safeToString = "<?>"
    override def kind = "LazyType"
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
      // SI-7928 `isModuleNotMethod` is here to avoid crashing with spuriously "overloaded" module accessor and module symbols.
      //         These appear after refchecks eliminates ModuleDefs that implement an interface.
      //         Here, we exclude the module symbol, which allows us to bind to the accessor.
      // SI-8054 We must only do this after refchecks, otherwise we exclude the module symbol which does not yet have an accessor!
      val isModuleWithAccessor = phase.refChecked && sym.isModuleNotMethod
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

  def copyRefinedType(original: RefinedType, parents: List[Type], decls: Scope) =
    if ((parents eq original.parents) && (decls eq original.decls)) original
    else {
      val owner = original.typeSymbol.owner
      val result = refinedType(parents, owner)
      val syms1 = decls.toList
      for (sym <- syms1)
        result.decls.enter(sym.cloneSymbol(result.typeSymbol))
      val syms2 = result.decls.toList
      val resultThis = result.typeSymbol.thisType
      for (sym <- syms2)
        sym modifyInfo (_ substThisAndSym(original.typeSymbol, resultThis, syms1, syms2))

      result
    }

  /** The canonical creator for typerefs
   *  todo: see how we can clean this up a bit
   */
  def typeRef(pre: Type, sym: Symbol, args: List[Type]): Type = {
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

      TypeRef(pre, sym, args)
    case _ =>
      typeRef(pre, sym, args)
  }

  /** The canonical creator for implicit method types */
  def JavaMethodType(params: List[Symbol], resultType: Type): JavaMethodType =
    new JavaMethodType(params, resultType) // don't unique this!

  /** Create a new MethodType of the same class as tp, i.e. keep JavaMethodType */
  def copyMethodType(tp: Type, params: List[Symbol], restpe: Type): Type = tp match {
    case _: JavaMethodType => JavaMethodType(params, restpe)
    case _                 => MethodType(params, restpe)
  }

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
        val arg_s  = args filterNot isUseableAsTypeArg map (t => t + "/" + t.getClass) mkString ", "
        s"$tapp_s includes illegal type argument $arg_s"
      })
    }

    tycon match {
      case TypeRef(pre, sym @ (NothingClass|AnyClass), _) => copyTypeRef(tycon, pre, sym, Nil)   //@M drop type args to Any/Nothing
      case TypeRef(pre, sym, Nil)                         => copyTypeRef(tycon, pre, sym, args)
      case TypeRef(pre, sym, bogons)                      => devWarning(s"Dropping $bogons from $tycon in appliedType.") ; copyTypeRef(tycon, pre, sym, args)
      case PolyType(tparams, restpe)                      => restpe.instantiateTypeParams(tparams, args)
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
   *  until we start tracking explicit kinds equivalent to typeFun (except that the latter requires tparams nonEmpty).
   *
   *  PP to AM: I've co-opted this for where I know tparams may well be empty, and
   *  expecting to get back `tpe` in such cases.  Re being "forgiving" below,
   *  can we instead say this is the canonical creator for polyTypes which
   *  may or may not be poly? (It filched the standard "canonical creator" name.)
   */
  object GenPolyType {
    def apply(tparams: List[Symbol], tpe: Type): Type = {
      tpe match {
        case MethodType(_, _) =>
          assert(tparams forall (_.isInvariant), "Trying to create a method with variant type parameters: " + ((tparams, tpe)))
        case _                =>
      }
      if (tparams.nonEmpty) typeFun(tparams, tpe)
      else tpe // it's okay to be forgiving here
    }
    def unapply(tpe: Type): Option[(List[Symbol], Type)] = tpe match {
      case PolyType(tparams, restpe) => Some((tparams, restpe))
      case _                         => Some((Nil, tpe))
    }
  }
  def genPolyType(params: List[Symbol], tpe: Type): Type = GenPolyType(params, tpe)

  @deprecated("use genPolyType(...) instead", "2.10.0") // Used in reflection API
  def polyType(params: List[Symbol], tpe: Type): Type = GenPolyType(params, tpe)

  /** A creator for anonymous type functions, where the symbol for the type function still needs to be created.
   *
   * TODO:
   * type params of anonymous type functions, which currently can only arise from normalising type aliases, are owned by the type alias of which they are the eta-expansion
   * higher-order subtyping expects eta-expansion of type constructors that arise from a class; here, the type params are owned by that class, but is that the right thing to do?
   */
  def typeFunAnon(tps: List[Symbol], body: Type): Type = typeFun(tps, body)

  /** A creator for a type functions, assuming the type parameters tps already have the right owner. */
  def typeFun(tps: List[Symbol], body: Type): Type = PolyType(tps, body)

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
  def existentialAbstraction(tparams: List[Symbol], tpe0: Type): Type =
    if (tparams.isEmpty) tpe0
    else {
      val tpe      = normalizeAliases(tpe0)
      val tpe1     = new ExistentialExtrapolation(tparams) extrapolate tpe
      var tparams0 = tparams
      var tparams1 = tparams0 filter tpe1.contains

      while (tparams1 != tparams0) {
        tparams0 = tparams1
        tparams1 = tparams filter { p =>
          tparams1 exists { p1 => p1 == p || (p1.info contains p) }
        }
      }
      newExistentialType(tparams1, tpe1)
    }



// Hash consing --------------------------------------------------------------

  private val initialUniquesCapacity = 4096
  private var uniques: util.WeakHashSet[Type] = _
  private var uniqueRunId = NoRunId

  protected def unique[T <: Type](tp: T): T = {
    if (Statistics.canEnable) Statistics.incCounter(rawTypeCount)
    if (uniqueRunId != currentRunId) {
      uniques = util.WeakHashSet[Type](initialUniquesCapacity)
      // JZ: We used to register this as a perRunCache so it would be cleared eagerly at
      // the end of the compilation run. But, that facility didn't actually clear this map (SI-8129)!
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

  object        unwrapToClass extends ClassUnwrapper(existential = true) { }
  object  unwrapToStableClass extends ClassUnwrapper(existential = false) { }
  object   unwrapWrapperTypes extends  TypeUnwrapper(true, true, true, true) { }

  def elementExtract(container: Symbol, tp: Type): Type = {
    assert(!container.isAliasType, container)
    unwrapWrapperTypes(tp baseType container).dealiasWiden match {
      case TypeRef(_, `container`, arg :: Nil)  => arg
      case _                                    => NoType
    }
  }
  def elementExtractOption(container: Symbol, tp: Type): Option[Type] = {
    elementExtract(container, tp) match {
      case NoType => None
      case tp => Some(tp)
    }
  }
  def elementTest(container: Symbol, tp: Type)(f: Type => Boolean): Boolean = {
    elementExtract(container, tp) match {
      case NoType => false
      case tp => f(tp)
    }
  }
  def elementTransform(container: Symbol, tp: Type)(f: Type => Type): Type = {
    elementExtract(container, tp) match {
      case NoType => NoType
      case tp => f(tp)
    }
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

  def containsExistential(tpe: Type) = tpe exists typeIsExistentiallyBound
  def existentialsInType(tpe: Type) = tpe withFilter typeIsExistentiallyBound map (_.typeSymbol)

  private def isDummyOf(tpe: Type)(targ: Type) = {
    val sym = targ.typeSymbol
    sym.isTypeParameter && sym.owner == tpe.typeSymbol
  }
  def isDummyAppliedType(tp: Type) = tp.dealias match {
    case tr @ TypeRef(_, _, args) => args exists isDummyOf(tr)
    case _                        => false
  }

  def typeParamsToExistentials(clazz: Symbol, tparams: List[Symbol]): List[Symbol] = {
    val eparams = mapWithIndex(tparams)((tparam, i) =>
      clazz.newExistential(newTypeName("?"+i), clazz.pos) setInfo tparam.info.bounds)

    eparams map (_ substInfo (tparams, eparams))
  }
  def typeParamsToExistentials(clazz: Symbol): List[Symbol] =
    typeParamsToExistentials(clazz, clazz.typeParams)

  def isRawIfWithoutArgs(sym: Symbol) = sym.isClass && sym.typeParams.nonEmpty && sym.isJavaDefined
  /** Is type tp a ''raw type''? */
  //  note: it's important to write the two tests in this order,
  //  as only typeParams forces the classfile to be read. See #400
  def isRawType(tp: Type) = !phase.erasedTypes && (tp match {
    case TypeRef(_, sym, Nil) => isRawIfWithoutArgs(sym)
    case _                    => false
  })

  @deprecated("Use isRawType", "2.10.1") // presently used by sbt
  def isRaw(sym: Symbol, args: List[Type]) = (
       !phase.erasedTypes
    && args.isEmpty
    && isRawIfWithoutArgs(sym)
  )

  def singletonBounds(hi: Type) = TypeBounds.upper(intersectionType(List(hi, SingletonClass.tpe)))

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
        val memType = widened asSeenFrom (pre, tp.typeSymbol.owner)
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
    val td = typeDepth(ts)
    val bd = baseTypeSeqDepth(ts)
    lubDepthAdjust(td, td max bd)
  }

  /** The maximum allowable depth of lubs or glbs over given types,
   *  as a function over the maximum depth `td` of these types, and
   *  the maximum depth `bd` of all types in the base type sequences of these types.
   */
  private def lubDepthAdjust(td: Depth, bd: Depth): Depth = (
    if (settings.XfullLubs) bd
    else if (bd <= Depth(3)) bd
    else if (bd <= Depth(5)) td max bd.decr
    else if (bd <= Depth(7)) td max (bd decr 2)
    else td.decr max (bd decr 3)
  )

  private def symTypeDepth(syms: List[Symbol]): Depth  = typeDepth(syms map (_.info))
  private def typeDepth(tps: List[Type]): Depth        = maxDepth(tps)
  private def baseTypeSeqDepth(tps: List[Type]): Depth = maxbaseTypeSeqDepth(tps)

  /** Is intersection of given types populated? That is,
   *  for all types tp1, tp2 in intersection
   *    for all common base classes bc of tp1 and tp2
   *      let bt1, bt2 be the base types of tp1, tp2 relative to class bc
   *      Then:
   *        bt1 and bt2 have the same prefix, and
   *        any corresponding non-variant type arguments of bt1 and bt2 are the same
   */
  def isPopulated(tp1: Type, tp2: Type): Boolean = {
    def isConsistent(tp1: Type, tp2: Type): Boolean = (tp1, tp2) match {
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
    }

    def check(tp1: Type, tp2: Type) = (
      if (tp1.typeSymbol.isClass && tp1.typeSymbol.hasFlag(FINAL))
        tp1 <:< tp2 || isNumericValueClass(tp1.typeSymbol) && isNumericValueClass(tp2.typeSymbol)
      else tp1.baseClasses forall (bc =>
        tp2.baseTypeIndex(bc) < 0 || isConsistent(tp1.baseType(bc), tp2.baseType(bc)))
    )

    check(tp1, tp2) && check(tp2, tp1)
  }

  /** Does a pattern of type `patType` need an outer test when executed against
   *  selector type `selType` in context defined by `currentOwner`?
   */
  def needsOuterTest(patType: Type, selType: Type, currentOwner: Symbol) = {
    def createDummyClone(pre: Type): Type = {
      val dummy = currentOwner.enclClass.newValue(nme.ANYname).setInfo(pre.widen)
      singleType(ThisType(currentOwner.enclClass), dummy)
    }
    def maybeCreateDummyClone(pre: Type, sym: Symbol): Type = pre match {
      case SingleType(pre1, sym1) =>
        if (sym1.isModule && sym1.isStatic) {
          NoType
        } else if (sym1.isModule && sym.owner == sym1.moduleClass) {
          val pre2 = maybeCreateDummyClone(pre1, sym1)
          if (pre2 eq NoType) pre2
          else singleType(pre2, sym1)
        } else {
          createDummyClone(pre)
        }
      case ThisType(clazz) =>
        if (clazz.isModuleClass)
          maybeCreateDummyClone(clazz.typeOfThis, sym)
        else if (sym.owner == clazz && (sym.hasFlag(PRIVATE) || sym.privateWithin == clazz))
          NoType
        else
          createDummyClone(pre)
      case _ =>
        NoType
    }
    // See the test for SI-7214 for motivation for dealias. Later `treeCondStrategy#outerTest`
    // generates an outer test based on `patType.prefix` with automatically dealises.
    patType.dealias match {
      case TypeRef(pre, sym, args) =>
        val pre1 = maybeCreateDummyClone(pre, sym)
        (pre1 ne NoType) && isPopulated(copyTypeRef(patType, pre1, sym, args), selType)
      case _ =>
        false
    }
  }

  def normalizePlus(tp: Type) = (
    if (isRawType(tp)) rawToExistential(tp)
    else tp.normalize match {
      // Unify the two representations of module classes
      case st @ SingleType(_, sym) if sym.isModule => st.underlying.normalize
      case _                                       => tp.normalize
    }
  )

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
  def isSameTypes(tps1: List[Type], tps2: List[Type]): Boolean = (tps1 corresponds tps2)(_ =:= _)

  /** True if two lists have the same length.  Since calling length on linear sequences
   *  is O(n), it is an inadvisable way to test length equality.
   */
  final def sameLength(xs1: List[_], xs2: List[_]) = compareLengths(xs1, xs2) == 0
  @tailrec final def compareLengths(xs1: List[_], xs2: List[_]): Int =
    if (xs1.isEmpty) { if (xs2.isEmpty) 0 else -1 }
    else if (xs2.isEmpty) 1
    else compareLengths(xs1.tail, xs2.tail)

  /** Again avoiding calling length, but the lengthCompare interface is clunky.
   */
  final def hasLength(xs: List[_], len: Int) = xs.lengthCompare(len) == 0

  private var _basetypeRecursions: Int = 0
  def basetypeRecursions = _basetypeRecursions
  def basetypeRecursions_=(value: Int) = _basetypeRecursions = value

  private val _pendingBaseTypes = new mutable.HashSet[Type]
  def pendingBaseTypes = _pendingBaseTypes

  /** Does this type have a prefix that begins with a type variable,
   *  or is it a refinement type? For type prefixes that fulfil this condition,
   *  type selections with the same name of equal (as determined by `=:=`) prefixes are
   *  considered equal in regard to `=:=`.
   */
  def isEligibleForPrefixUnification(tp: Type): Boolean = tp match {
    case SingleType(pre, sym)  => !(sym hasFlag PACKAGE) && isEligibleForPrefixUnification(pre)
    case tv@TypeVar(_, constr) => !tv.instValid || isEligibleForPrefixUnification(constr.inst)
    case RefinedType(_, _)     => true
    case _                     => false
  }

  def isErrorOrWildcard(tp: Type) = (tp eq ErrorType) || (tp eq WildcardType)

  /** This appears to be equivalent to tp.isInstanceof[SingletonType],
   *  except it excludes ConstantTypes.
   */
  def isSingleType(tp: Type) = tp match {
    case ThisType(_) | SuperType(_, _) | SingleType(_, _) => true
    case _                                                => false
  }

  def isConstantType(tp: Type) = tp match {
    case ConstantType(_) => true
    case _               => false
  }

  def isExistentialType(tp: Type): Boolean = tp match {
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
    case WildcardType           => true
    case BoundedWildcardType(_) => true
    case ErrorType              => true
    case _: TypeVar             => true
    case _                      => false
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
  private def isValueElseNonValue(tp: Type): Boolean = tp match {
    case tp if isAlwaysValueType(tp)           => true
    case tp if isAlwaysNonValueType(tp)        => false
    case AnnotatedType(_, underlying)          => isValueElseNonValue(underlying)
    case SingleType(_, sym)                    => sym.isValue           // excludes packages and statics
    case TypeRef(_, _, _) if tp.isHigherKinded => false                 // excludes type constructors
    case ThisType(sym)                         => !sym.isPackageClass   // excludes packages
    case TypeRef(_, sym, _)                    => !sym.isPackageClass   // excludes packages
    case PolyType(_, _)                        => true                  // poly-methods excluded earlier
    case tp                                    => sys.error("isValueElseNonValue called with third-way type " + tp)
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
   *  abd statics), overloaded types. Varargs and by-name types T* and (=>T) are
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
    def isSubArg(t1: Type, t2: Type, variance: Variance) = (
         (variance.isCovariant || isSubType(t2, t1, depth))     // The order of these two checks can be material for performance (SI-8478)
      && (variance.isContravariant || isSubType(t1, t2, depth))
    )

    corresponds3(tps1, tps2, mapList(tparams)(_.variance))(isSubArg)
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

    (    (tp.typeSymbol isBottomSubClass sym.owner)
      || specializedBy(tp nonPrivateMember sym.name)
    )
  }

  /** Does member `symLo` of `tpLo` have a stronger type
   *  than member `symHi` of `tpHi`?
   */
  protected[internal] def specializesSym(preLo: Type, symLo: Symbol, preHi: Type, symHi: Symbol, depth: Depth): Boolean =
    (symHi.isAliasType || symHi.isTerm || symHi.isAbstractType) && {
      // only now that we know symHi is a viable candidate ^^^^^^^, do the expensive checks: ----V
      require((symLo ne NoSymbol) && (symHi ne NoSymbol), ((preLo, symLo, preHi, symHi, depth)))

      val tpHi = preHi.memberInfo(symHi).substThis(preHi.typeSymbol, preLo)

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
            // sameLength(params1, params2) was used directly as pre-screening optimization (now done by matchesQuantified -- is that ok, performancewise?)
            mt1.isImplicit == mt2.isImplicit &&
            matchingParams(params1, params2, mt1.isJava, mt2.isJava) &&
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
  protected[internal] def matchingParams(syms1: List[Symbol], syms2: List[Symbol], syms1isJava: Boolean, syms2isJava: Boolean): Boolean = syms1 match {
    case Nil =>
      syms2.isEmpty
    case sym1 :: rest1 =>
      syms2 match {
        case Nil =>
          false
        case sym2 :: rest2 =>
          val tp1 = sym1.tpe
          val tp2 = sym2.tpe
          (tp1 =:= tp2 ||
           syms1isJava && tp2.typeSymbol == ObjectClass && tp1.typeSymbol == AnyClass ||
           syms2isJava && tp1.typeSymbol == ObjectClass && tp2.typeSymbol == AnyClass) &&
          matchingParams(rest1, rest2, syms1isJava, syms2isJava)
      }
  }

  /** Do type arguments `targs` conform to formal parameters `tparams`?
   */
  def isWithinBounds(pre: Type, owner: Symbol, tparams: List[Symbol], targs: List[Type]): Boolean = {
    var bounds = instantiatedBounds(pre, owner, tparams, targs)
    if (targs exists typeHasAnnotations)
      bounds = adaptBoundsToAnnotations(bounds, tparams, targs)
    (bounds corresponds targs)(boundsContainType)
  }

  def instantiatedBounds(pre: Type, owner: Symbol, tparams: List[Symbol], targs: List[Type]): List[TypeBounds] =
    mapList(tparams)(_.info.asSeenFrom(pre, owner).instantiateTypeParams(tparams, targs).bounds)

  def elimAnonymousClass(t: Type) = t match {
    case TypeRef(pre, clazz, Nil) if clazz.isAnonymousClass =>
      clazz.classBound.asSeenFrom(pre, clazz.owner)
    case _ =>
      t
  }

  /** A list of the typevars in a type. */
  def typeVarsInType(tp: Type): List[TypeVar] = {
    var tvs: List[TypeVar] = Nil
    tp foreach {
      case t: TypeVar => tvs ::= t
      case _          =>
    }
    tvs.reverse
  }

  // If this type contains type variables, put them to sleep for a while.
  // Don't just wipe them out by replacing them by the corresponding type
  // parameter, as that messes up (e.g.) type variables in type refinements.
  // Without this, the matchesType call would lead to type variables on both
  // sides of a subtyping/equality judgement, which can lead to recursive types
  // being constructed. See pos/t0851 for a situation where this happens.
  @inline final def suspendingTypeVars[T](tvs: List[TypeVar])(op: => T): T = {
    val saved = tvs map (_.suspended)
    tvs foreach (_.suspended = true)

    try op
    finally foreach2(tvs, saved)(_.suspended = _)
  }

  /** Compute lub (if `variance == Covariant`) or glb (if `variance == Contravariant`) of given list
   *  of types `tps`. All types in `tps` are typerefs or singletypes
   *  with the same symbol.
   *  Return `x` if the computation succeeds with result `x`.
   *  Return `NoType` if the computation fails.
   */
  def mergePrefixAndArgs(tps: List[Type], variance: Variance, depth: Depth): Type = tps match {
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
          if (argss exists typeListIsEmpty) {
            NoType  // something is wrong: an array without a type arg.
          }
          else {
            val args = argss map (_.head)
            if (args.tail forall (_ =:= args.head)) typeRef(pre, sym, List(args.head))
            else if (args exists (arg => isPrimitiveValueClass(arg.typeSymbol))) ObjectTpe
            else typeRef(pre, sym, List(lub(args)))
          }
        }
        else transposeSafe(argss) match {
          case None =>
            // transpose freaked out because of irregular argss
            // catching just in case (shouldn't happen, but also doesn't cost us)
            // [JZ] It happens: see SI-5683.
            debuglog(s"transposed irregular matrix!? tps=$tps argss=$argss")
            NoType
          case Some(argsst) =>
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
                if (tparam.variance == variance) lub(as, depth.decr)
                else if (tparam.variance == variance.flip) glb(as, depth.decr)
                else {
                  val l = lub(as, depth.decr)
                  val g = glb(as, depth.decr)
                  if (l <:< g) l
                  else { // Martin: I removed this, because incomplete. Not sure there is a good way to fix it. For the moment we
                       // just err on the conservative side, i.e. with a bound that is too high.
                       // if(!(tparam.info.bounds contains tparam))   //@M can't deal with f-bounds, see #2251

                    val qvar = commonOwner(as) freshExistential "" setInfo TypeBounds(g, l)
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
    case ExistentialType(tparams, quantified) :: rest =>
      mergePrefixAndArgs(quantified :: rest, variance, depth) match {
        case NoType => NoType
        case tpe    => existentialAbstraction(tparams, tpe)
      }
    case _ =>
      abort(s"mergePrefixAndArgs($tps, $variance, $depth): unsupported tps")
  }

  def addMember(thistp: Type, tp: Type, sym: Symbol): Unit = addMember(thistp, tp, sym, AnyDepth)

  /** Make symbol `sym` a member of scope `tp.decls`
   *  where `thistp` is the narrowed owner type of the scope.
   */
  def addMember(thistp: Type, tp: Type, sym: Symbol, depth: Depth) {
    assert(sym != NoSymbol)
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
  }

  // TODO: RecoverableCyclicReference should be separated from TypeError,
  // but that would be a big change. Left for further refactoring.
  /** An exception for cyclic references from which we can recover */
  case class RecoverableCyclicReference(sym: Symbol)
    extends TypeError("illegal cyclic reference involving " + sym) {
    if (settings.debug) printStackTrace()
  }

  class NoCommonType(tps: List[Type]) extends Throwable(
    "lub/glb of incompatible types: " + tps.mkString("", " and ", "")) with ControlThrowable

  /** A throwable signalling a malformed type */
  class MalformedType(msg: String) extends TypeError(msg) {
    def this(pre: Type, tp: String) = this("malformed type: " + pre + "#" + tp)
  }

  /** The current indentation string for traces */
  private var _indent: String = ""
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
  def explainTypes(found: Type, required: Type) {
    if (settings.explaintypes) withTypesExplained(found <:< required)
  }

  /** If option `explaintypes` is set, print a subtype trace for `op(found, required)`. */
  def explainTypes(op: (Type, Type) => Any, found: Type, required: Type) {
    if (settings.explaintypes) withTypesExplained(op(found, required))
  }

  /** Execute `op` while printing a trace of the operations on types executed. */
  def withTypesExplained[A](op: => A): A = {
    val s = explainSwitch
    try { explainSwitch = true; op } finally { explainSwitch = s }
  }

  def isUnboundedGeneric(tp: Type) = tp match {
    case t @ TypeRef(_, sym, _) => sym.isAbstractType && !(t <:< AnyRefTpe)
    case _                      => false
  }
  def isBoundedGeneric(tp: Type) = tp match {
    case TypeRef(_, sym, _) if sym.isAbstractType => (tp <:< AnyRefTpe)
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

  def objToAny(tp: Type): Type =
    if (!phase.erasedTypes && tp.typeSymbol == ObjectClass) AnyTpe
    else tp

  val shorthands = Set(
    "scala.collection.immutable.List",
    "scala.collection.immutable.Nil",
    "scala.collection.Seq",
    "scala.collection.Traversable",
    "scala.collection.Iterable",
    "scala.collection.mutable.StringBuilder",
    "scala.collection.IndexedSeq",
    "scala.collection.Iterator")

// ----- Hoisted closures and convenience methods, for compile time reductions -------

  private[scala] val isTypeVar = (tp: Type) => tp.isInstanceOf[TypeVar]
  private[scala] val typeContainsTypeVar = (tp: Type) => tp exists isTypeVar
  private[scala] val typeIsNonClassType = (tp: Type) => tp.typeSymbolDirect.isNonClassType
  private[scala] val typeIsExistentiallyBound = (tp: Type) => tp.typeSymbol.isExistentiallyBound
  private[scala] val typeIsErroneous = (tp: Type) => tp.isErroneous
  private[scala] val symTypeIsError = (sym: Symbol) => sym.tpe.isError
  private[scala] val treeTpe = (t: Tree) => t.tpe
  private[scala] val symTpe = (sym: Symbol) => sym.tpe
  private[scala] val symInfo = (sym: Symbol) => sym.info
  private[scala] val typeHasAnnotations = (tp: Type) => tp.annotations ne Nil
  private[scala] val boundsContainType = (bounds: TypeBounds, tp: Type) => bounds containsType tp
  private[scala] val typeListIsEmpty = (ts: List[Type]) => ts.isEmpty
  private[scala] val typeIsSubTypeOfSerializable = (tp: Type) => tp <:< SerializableTpe
  private[scala] val typeIsNothing = (tp: Type) => tp.typeSymbolDirect eq NothingClass
  private[scala] val typeIsAny = (tp: Type) => tp.typeSymbolDirect eq AnyClass
  private[scala] val typeIsHigherKinded = (tp: Type) => tp.isHigherKinded

  /** The maximum depth of type `tp` */
  def typeDepth(tp: Type): Depth = tp match {
    case TypeRef(pre, sym, args)          => typeDepth(pre) max typeDepth(args).incr
    case RefinedType(parents, decls)      => typeDepth(parents) max symTypeDepth(decls.toList).incr
    case TypeBounds(lo, hi)               => typeDepth(lo) max typeDepth(hi)
    case MethodType(paramtypes, result)   => typeDepth(result)
    case NullaryMethodType(result)        => typeDepth(result)
    case PolyType(tparams, result)        => typeDepth(result) max symTypeDepth(tparams).incr
    case ExistentialType(tparams, result) => typeDepth(result) max symTypeDepth(tparams).incr
    case _                                => Depth(1)
  }

  //OPT replaced with tailrecursive function to save on #closures
  // was:
  //    var d = 0
  //    for (tp <- tps) d = d max by(tp) //!!!OPT!!!
  //    d
  private[scala] def maxDepth(tps: List[Type]): Depth = {
    @tailrec def loop(tps: List[Type], acc: Depth): Depth = tps match {
      case tp :: rest => loop(rest, acc max typeDepth(tp))
      case _          => acc
    }
    loop(tps, Depth.Zero)
  }
  private[scala] def maxbaseTypeSeqDepth(tps: List[Type]): Depth = {
    @tailrec def loop(tps: List[Type], acc: Depth): Depth = tps match {
      case tp :: rest => loop(rest, acc max tp.baseTypeSeqDepth)
      case _          => acc
    }
    loop(tps, Depth.Zero)
  }

  @tailrec private def typesContain(tps: List[Type], sym: Symbol): Boolean = tps match {
    case tp :: rest => (tp contains sym) || typesContain(rest, sym)
    case _ => false
  }

  @tailrec private def areTrivialTypes(tps: List[Type]): Boolean = tps match {
    case tp :: rest => tp.isTrivial && areTrivialTypes(rest)
    case _ => true
  }

// -------------- Classtags --------------------------------------------------------

  implicit val AnnotatedTypeTag = ClassTag[AnnotatedType](classOf[AnnotatedType])
  implicit val BoundedWildcardTypeTag = ClassTag[BoundedWildcardType](classOf[BoundedWildcardType])
  implicit val ClassInfoTypeTag = ClassTag[ClassInfoType](classOf[ClassInfoType])
  implicit val CompoundTypeTag = ClassTag[CompoundType](classOf[CompoundType])
  implicit val ConstantTypeTag = ClassTag[ConstantType](classOf[ConstantType])
  implicit val ExistentialTypeTag = ClassTag[ExistentialType](classOf[ExistentialType])
  implicit val MethodTypeTag = ClassTag[MethodType](classOf[MethodType])
  implicit val NullaryMethodTypeTag = ClassTag[NullaryMethodType](classOf[NullaryMethodType])
  implicit val PolyTypeTag = ClassTag[PolyType](classOf[PolyType])
  implicit val RefinedTypeTag = ClassTag[RefinedType](classOf[RefinedType])
  implicit val SingletonTypeTag = ClassTag[SingletonType](classOf[SingletonType])
  implicit val SingleTypeTag = ClassTag[SingleType](classOf[SingleType])
  implicit val SuperTypeTag = ClassTag[SuperType](classOf[SuperType])
  implicit val ThisTypeTag = ClassTag[ThisType](classOf[ThisType])
  implicit val TypeBoundsTag = ClassTag[TypeBounds](classOf[TypeBounds])
  implicit val TypeRefTag = ClassTag[TypeRef](classOf[TypeRef])
  implicit val TypeTagg = ClassTag[Type](classOf[Type])

// -------------- Statistics --------------------------------------------------------

  Statistics.newView("#unique types") { if (uniques == null) 0 else uniques.size }

}

object TypeConstants {
  final val DefaultLogThreshhold         = 50
  final val LogPendingBaseTypesThreshold = DefaultLogThreshhold
  final val LogVolatileThreshold         = DefaultLogThreshhold
}

object TypesStats {
  import BaseTypeSeqsStats._
  val rawTypeCount        = Statistics.newCounter   ("#raw type creations")
  val subtypeCount        = Statistics.newCounter   ("#subtype ops")
  val sametypeCount       = Statistics.newCounter   ("#sametype ops")
  val lubCount            = Statistics.newCounter   ("#toplevel lubs/glbs")
  val nestedLubCount      = Statistics.newCounter   ("#all lubs/glbs")
  val findMemberCount     = Statistics.newCounter   ("#findMember ops")
  val findMembersCount    = Statistics.newCounter   ("#findMembers ops")
  val noMemberCount       = Statistics.newSubCounter("  of which not found", findMemberCount)
  val multMemberCount     = Statistics.newSubCounter("  of which multiple overloaded", findMemberCount)
  val typerNanos          = Statistics.newTimer     ("time spent typechecking", "typer")
  val lubNanos            = Statistics.newStackableTimer("time spent in lubs", typerNanos)
  val subtypeNanos        = Statistics.newStackableTimer("time spent in <:<", typerNanos)
  val findMemberNanos     = Statistics.newStackableTimer("time spent in findmember", typerNanos)
  val findMembersNanos    = Statistics.newStackableTimer("time spent in findmembers", typerNanos)
  val asSeenFromNanos     = Statistics.newStackableTimer("time spent in asSeenFrom", typerNanos)
  val baseTypeSeqNanos    = Statistics.newStackableTimer("time spent in baseTypeSeq", typerNanos)
  val baseClassesNanos    = Statistics.newStackableTimer("time spent in baseClasses", typerNanos)
  val compoundBaseTypeSeqCount = Statistics.newSubCounter("  of which for compound types", baseTypeSeqCount)
  val typerefBaseTypeSeqCount = Statistics.newSubCounter("  of which for typerefs", baseTypeSeqCount)
  val singletonBaseTypeSeqCount = Statistics.newSubCounter("  of which for singletons", baseTypeSeqCount)
  val typeOpsStack = Statistics.newTimerStack()

  /* Commented out, because right now this does not inline, so creates a closure which will distort statistics
  @inline final def timedTypeOp[T](c: Statistics.StackableTimer)(op: => T): T = {
    val start = Statistics.pushTimer(typeOpsStack, c)
    try op
    finally
  }
  */
}

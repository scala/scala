/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package internal

import scala.collection.{ mutable, immutable, generic }
import generic.Clearable
import scala.ref.WeakReference
import mutable.ListBuffer
import Flags._
import scala.util.control.ControlThrowable
import scala.annotation.tailrec
import util.Statistics._

/* A standard type pattern match:
  case ErrorType =>
    // internal: error
  case WildcardType =>
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
  case AnnotatedType(annots, tp, selfsym) =>
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
  case DeBruijnIndex(level, index)
    // for dependent method types: a type referring to a method parameter.
  case ErasedValueType(tp)
    // only used during erasure of derived value classes.
*/

trait Types extends api.Types { self: SymbolTable =>
  import definitions._

  //statistics
  def uniqueTypeCount = if (uniques == null) 0 else uniques.size

  private var explainSwitch = false
  private final val emptySymbolSet = immutable.Set.empty[Symbol]

  private final val LogPendingSubTypesThreshold = 50
  private final val LogPendingBaseTypesThreshold = 50
  private final val LogVolatileThreshold = 50

  /** A don't care value for the depth parameter in lubs/glbs and related operations. */
  private final val AnyDepth = -3

  /** Decrement depth unless it is a don't care. */
  private final def decr(depth: Int) = if (depth == AnyDepth) AnyDepth else depth - 1

  private final val printLubs = sys.props contains "scalac.debug.lub"
  private final val traceTypeVars = sys.props contains "scalac.debug.tvar"
  /** In case anyone wants to turn off lub verification without reverting anything. */
  private final val verifyLubs = true
  /** In case anyone wants to turn off type parameter bounds being used
   *  to seed type constraints.
   */
  private final val propagateParameterBoundsToTypeVars = sys.props contains "scalac.debug.prop-constraints"

  protected val enableTypeVarExperimentals = settings.Xexperimental.value

  /** Empty immutable maps to avoid allocations. */
  private val emptySymMap   = immutable.Map[Symbol, Symbol]()
  private val emptySymCount = immutable.Map[Symbol, Int]()

  /** The current skolemization level, needed for the algorithms
   *  in isSameType, isSubType that do constraint solving under a prefix.
   */
  var skolemizationLevel = 0

  /** A log of type variable with their original constraints. Used in order
   *  to undo constraints in the case of isSubType/isSameType failure.
   */
  lazy val undoLog = newUndoLog

  protected def newUndoLog = new UndoLog

  class UndoLog extends Clearable {
    private type UndoPairs = List[(TypeVar, TypeConstraint)]
    private var log: UndoPairs = List()

    // register with the auto-clearing cache manager
    perRunCaches.recordCache(this)

    /** Undo all changes to constraints to type variables upto `limit`. */
    private def undoTo(limit: UndoPairs) {
      while ((log ne limit) && log.nonEmpty) {
        val (tv, constr) = log.head
        tv.constr = constr
        log = log.tail
      }
    }

    /** No sync necessary, because record should only
     *  be called from within a undo or undoUnless block,
     *  which is already synchronized.
     */
    private[reflect] def record(tv: TypeVar) = {
      log ::= ((tv, tv.constr.cloneInternal))
    }

    def clear() {
      if (settings.debug.value)
        self.log("Clearing " + log.size + " entries from the undoLog.")

      log = Nil
    }
    def size = log.size

    // `block` should not affect constraints on typevars
    def undo[T](block: => T): T = {
      val before = log

      try block
      finally undoTo(before)
    }

    // if `block` evaluates to false, it should not affect constraints on typevars
    def undoUnless(block: => Boolean): Boolean = {
      val before = log
      var result = false

      try result = block
      finally if (!result) undoTo(before)

      result
    }
  }

  /** A map from lists to compound types that have the given list as parents.
   *  This is used to avoid duplication in the computation of base type sequences and baseClasses.
   *  It makes use of the fact that these two operations depend only on the parents,
   *  not on the refinement.
   */
  val intersectionWitness = perRunCaches.newWeakMap[List[Type], WeakReference[Type]]()

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
    override def isNotNull = underlying.isNotNull
    override def isError = underlying.isError
    override def isErroneous = underlying.isErroneous
    override def isStable: Boolean = underlying.isStable
    override def isVolatile = underlying.isVolatile
    override def finalResultType = underlying.finalResultType
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
    protected def maybeRewrap(newtp: Type) = if (newtp eq underlying) this else rewrap(newtp)
    protected def rewrap(newtp: Type): Type

    // the following are all operations in class Type that are overridden in some subclass
    // Important to keep this up-to-date when new operations are added!
    override def widen = maybeRewrap(underlying.widen)
    override def narrow = underlying.narrow
    override def deconst = maybeRewrap(underlying.deconst)
    override def resultType = maybeRewrap(underlying.resultType)
    override def resultType(actuals: List[Type]) = maybeRewrap(underlying.resultType(actuals))
    override def finalResultType = maybeRewrap(underlying.finalResultType)
    override def paramSectionCount = 0
    override def paramss: List[List[Symbol]] = List()
    override def params: List[Symbol] = List()
    override def paramTypes: List[Type] = List()
    override def typeArgs = underlying.typeArgs
    override def notNull = maybeRewrap(underlying.notNull)
    override def instantiateTypeParams(formals: List[Symbol], actuals: List[Type]) = underlying.instantiateTypeParams(formals, actuals)
    override def skolemizeExistential(owner: Symbol, origin: AnyRef) = underlying.skolemizeExistential(owner, origin)
    override def normalize = maybeRewrap(underlying.normalize)
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
    super.tpe_=(NoType)
    override def tpe_=(t: Type) = if (t != NoType) {
      throw new UnsupportedOperationException("tpe_=("+t+") inapplicable for <empty>")
    }
  }

  abstract class AbsTypeImpl extends AbsType { this: Type =>
    def declaration(name: Name): Symbol = decl(name)
    def nonPrivateDeclaration(name: Name): Symbol = nonPrivateDecl(name)
    def declarations = decls
    def typeArguments = typeArgs
    def erasure = this match {
      case ConstantType(value) => widen.erasure // [Eugene to Martin] constant types are unaffected by erasure. weird.
      case _ =>
        var result = transformedType(this)
        result = result.normalize match { // necessary to deal with erasures of HK types, typeConstructor won't work
          case PolyType(undets, underlying) => existentialAbstraction(undets, underlying) // we don't want undets in the result
          case _ => result
        }
        // [Eugene] erasure screws up all ThisTypes for modules into PackageTypeRefs
        // we need to unscrew them, or certain typechecks will fail mysteriously
        // http://groups.google.com/group/scala-internals/browse_thread/thread/6d3277ae21b6d581
        result = result.map(tpe => tpe match {
          case tpe: PackageTypeRef => ThisType(tpe.sym)
          case _ => tpe
        })
        result
    }
    def substituteTypes(from: List[Symbol], to: List[Type]): Type = subst(from, to)

    // [Eugene] to be discussed and refactored
    def isConcrete = {
      def notConcreteSym(sym: Symbol) =
        sym.isAbstractType && !sym.isExistential

      def notConcreteTpe(tpe: Type): Boolean = tpe match {
        case ThisType(_) => false
        case SuperType(_, _) => false
        case SingleType(pre, sym) => notConcreteSym(sym)
        case ConstantType(_) => false
        case TypeRef(_, sym, args) => notConcreteSym(sym) || (args exists (arg => notConcreteTpe(arg)))
        case RefinedType(_, _) => false
        case ExistentialType(_, _) => false
        case AnnotatedType(_, tp, _) => notConcreteTpe(tp)
        case _ => true
      }

      !notConcreteTpe(this)
    }

    // [Eugene] is this comprehensive?
    // the only thingies that we want to splice are: 1) type parameters, 2) type members
    // the thingies that we don't want to splice are: 1) concrete types (obviously), 2) existential skolems
    // this check seems to cover them all, right?
    // todo. after we discuss this, move the check to subclasses
    def isSpliceable = {
      this.isInstanceOf[TypeRef] && typeSymbol.isAbstractType && !typeSymbol.isExistential
    }
  }

  /** The base class for all types */
  abstract class Type extends AbsTypeImpl with Annotatable[Type] {
    /** Types for which asSeenFrom always is the identity, no matter what
     *  prefix or owner.
     */
    def isTrivial: Boolean = false

    /** Is this type higher-kinded, i.e., is it a type constructor @M */
    def isHigherKinded: Boolean = false

    /** Does this type denote a stable reference (i.e. singleton type)? */
    def isStable: Boolean = false

    /** Is this type dangerous (i.e. it might contain conflicting
     *  type information when empty, so that it can be constructed
     *  so that type unsoundness results.) A dangerous type has an underlying
     *  type of the form T_1 with T_n { decls }, where one of the
     *  T_i (i > 1) is an abstract type.
     */
    def isVolatile: Boolean = false

    /** Is this type guaranteed not to have `null` as a value? */
    def isNotNull: Boolean = false

    /** Is this type a structural refinement type (it ''refines'' members that have not been inherited) */
    def isStructuralRefinement: Boolean = false

    /** Does this type depend immediately on an enclosing method parameter?
      * I.e., is it a singleton type whose termSymbol refers to an argument of the symbol's owner (which is a method)?
      */
    def isImmediatelyDependent: Boolean = false

    /** Does this depend on an enclosing method parameter? */
    def isDependent: Boolean = IsDependentCollector.collect(this)

    /** True for WildcardType or BoundedWildcardType. */
    def isWildcard = false

    /** Is this type produced as a repair for an error? */
    def isError: Boolean = typeSymbol.isError || termSymbol.isError

    /** Is this type produced as a repair for an error? */
    def isErroneous: Boolean = ErroneousCollector.collect(this)

    /** Does this type denote a reference type which can be null? */
    // def isNullable: Boolean = false

    /** Can this type only be subtyped by bottom types?
     *  This is assessed to be the case if the class is final,
     *  and all type parameters (if any) are invariant.
     */
    def isFinalType =
      typeSymbol.isFinal && (typeSymbol.typeParams forall (_.variance == 0))

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
        refinedType(List(this), cowner, EmptyScope, cowner.pos).narrow
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
    def firstParent = if (parents.nonEmpty) parents.head else ObjectClass.tpe

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

    /** If this is a TypeRef `clazz`[`T`], return the argument `T`
     *  otherwise return this type
     */
    def remove(clazz: Symbol): Type = this

    /** For a curried/nullary method or poly type its non-method result type,
     *  the type itself for all other types */
    def finalResultType: Type = this

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

    /** Mixin a NotNull trait unless type already has one
     *  ...if the option is given, since it is causing typing bugs.
     */
    def notNull: Type =
      if (!settings.Ynotnull.value || isNotNull || phase.erasedTypes) this
      else NotNullType(this)

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
     *  Example: (in the below, <List> is the type constructor of List)
     *    TypeRef(pre, <List>, List()) is replaced by
     *    PolyType(X, TypeRef(pre, <List>, List(X)))
     */
    def normalize = this // @MAT

    /** Expands type aliases. */
    def dealias = this

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
    def nonPrivateDecls: List[Symbol] = decls filter (x => !x.isPrivate) toList

    /** The non-private defined or declared members with name `name` in this type;
     *  an OverloadedSymbol if several exist, NoSymbol if none exist.
     *  Alternatives of overloaded symbol appear in the order they are declared.
     */
    def nonPrivateDecl(name: Name): Symbol = findDecl(name, PRIVATE)

    /** A list of all members of this type (defined or inherited)
     *  Members appear in linearization order of their owners.
     *  Members with the same owner appear in reverse order of their declarations.
     */
    def members: List[Symbol] = membersBasedOnFlags(0, 0)

    /** A list of all non-private members of this type (defined or inherited) */
    def nonPrivateMembers: List[Symbol] = membersBasedOnFlags(BridgeAndPrivateFlags, 0)

    /** A list of all non-private members of this type  (defined or inherited),
     *  admitting members with given flags `admit`
     */
    def nonPrivateMembersAdmitting(admit: Long): List[Symbol] = membersBasedOnFlags(BridgeAndPrivateFlags & ~admit, 0)

    /** A list of all implicit symbols of this type  (defined or inherited) */
    def implicitMembers: List[Symbol] = membersBasedOnFlags(BridgeFlags, IMPLICIT)

    /** A list of all deferred symbols of this type  (defined or inherited) */
    def deferredMembers: List[Symbol] = membersBasedOnFlags(BridgeFlags, DEFERRED)

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

    /** All members with the given flags, excluding bridges.
     */
    def membersWithFlags(requiredFlags: Long): List[Symbol] =
      membersBasedOnFlags(BridgeFlags, requiredFlags)

    /** All non-private members with the given flags, excluding bridges.
     */
    def nonPrivateMembersWithFlags(requiredFlags: Long): List[Symbol] =
      membersBasedOnFlags(BridgeAndPrivateFlags, requiredFlags)

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
    def membersBasedOnFlags(excludedFlags: Long, requiredFlags: Long): List[Symbol] =
      findMember(nme.ANYNAME, excludedFlags, requiredFlags, false).alternatives

    def memberBasedOnName(name: Name, excludedFlags: Long): Symbol =
      findMember(name, excludedFlags, 0, false)

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
      if (isTrivial || phase.erasedTypes && pre.typeSymbol != ArrayClass) this
      else {
//        scala.tools.nsc.util.trace.when(pre.isInstanceOf[ExistentialType])("X "+this+".asSeenfrom("+pre+","+clazz+" = ") {
        incCounter(asSeenFromCount)
        val start = startTimer(asSeenFromNanos)
        val m = new AsSeenFromMap(pre.normalize, clazz)
        val tp = m apply this
        val tp1 = existentialAbstraction(m.capturedParams, tp)
        val result: Type =
          if (m.capturedSkolems.isEmpty) tp1
          else deriveType(m.capturedSkolems, _.cloneSymbol setFlag CAPTURED)(tp1)

        stopTimer(asSeenFromNanos, start)
        result
      }
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
        tp.asSeenFrom(this, sym.owner)
    }

    /** Substitute types `to` for occurrences of references to
     *  symbols `from` in this type.
     */
    def subst(from: List[Symbol], to: List[Type]): Type =
      if (from.isEmpty) this
      else new SubstTypeMap(from, to) apply this

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

    /** Performs both substThis and substSym in one traversal.
     */
    def substThisAndSym(from: Symbol, to: Type, symsFrom: List[Symbol], symsTo: List[Symbol]): Type = {
      if (symsFrom eq symsTo) substThis(from, to)
      else new SubstThisAndSymMap(from, to, symsFrom, symsTo) apply this
    }

    /** Returns all parts of this type which satisfy predicate `p` */
    def filter(p: Type => Boolean): List[Type] = new FilterTypeCollector(p) collect this
    def withFilter(p: Type => Boolean) = new FilterMapForeach(p)

    class FilterMapForeach(p: Type => Boolean) extends FilterTypeCollector(p){
      def foreach[U](f: Type => U): Unit = collect(Type.this) foreach f
      def map[T](f: Type => T): List[T]  = collect(Type.this) map f
    }

    /** Returns optionally first type (in a preorder traversal) which satisfies predicate `p`,
     *  or None if none exists.
     */
    def find(p: Type => Boolean): Option[Type] = new FindTypeCollector(p).collect(this)

    /** Apply `f` to each part of this type */
    def foreach(f: Type => Unit) { new ForEachTypeTraverser(f).traverse(this) }

    /** Apply `pf' to each part of this type on which the function is defined */
    def collect[T](pf: PartialFunction[Type, T]): List[T] = new CollectTypeCollector(pf).collect(this)

    /** Apply `f` to each part of this type; children get mapped before their parents */
    def map(f: Type => Type): Type = new TypeMap {
      def apply(x: Type) = f(mapOver(x))
    } apply this

    /** Is there part of this type which satisfies predicate `p`? */
    def exists(p: Type => Boolean): Boolean = !find(p).isEmpty

    /** Does this type contain a reference to this symbol? */
    def contains(sym: Symbol): Boolean = new ContainsCollector(sym).collect(this)

    /** Does this type contain a reference to this type */
    def containsTp(tp: Type): Boolean = new ContainsTypeCollector(tp).collect(this)

    /** Is this type a subtype of that type? */
    def <:<(that: Type): Boolean = {
      if (util.Statistics.enabled) stat_<:<(that)
      else {
        (this eq that) ||
        (if (explainSwitch) explain("<:", isSubType, this, that)
         else isSubType(this, that, AnyDepth))
      }
    }

    /** Is this type a subtype of that type in a pattern context?
     *  Any type arguments on the right hand side are replaced with
     *  fresh existentials, except for Arrays.
     *
     *  See bug1434.scala for an example of code which would fail
     *  if only a <:< test were applied.
     */
    def matchesPattern(that: Type): Boolean = {
      (this <:< that) || ((this, that) match {
        case (TypeRef(_, ArrayClass, List(arg1)), TypeRef(_, ArrayClass, List(arg2))) if arg2.typeSymbol.typeParams.nonEmpty =>
          arg1 matchesPattern arg2
        case (_, TypeRef(_, _, args)) =>
          val newtp = existentialAbstraction(args map (_.typeSymbol), that)
          !(that =:= newtp) && (this <:< newtp)
        case _ =>
          false
      })
    }

    def stat_<:<(that: Type): Boolean = {
      incCounter(subtypeCount)
      val start = startTimer(subtypeNanos)
      val result =
        (this eq that) ||
        (if (explainSwitch) explain("<:", isSubType, this, that)
         else isSubType(this, that, AnyDepth))
      stopTimer(subtypeNanos, start)
      result
    }

    /** Is this type a weak subtype of that type? True also for numeric types, i.e. Int weak_<:< Long.
     */
    def weak_<:<(that: Type): Boolean = {
      incCounter(subtypeCount)
      val start = startTimer(subtypeNanos)
      val result =
        ((this eq that) ||
         (if (explainSwitch) explain("weak_<:", isWeakSubType, this, that)
          else isWeakSubType(this, that)))
      stopTimer(subtypeNanos, start)
      result
    }

    /** Is this type equivalent to that type? */
    def =:=(that: Type): Boolean = (
      (this eq that) ||
      (if (explainSwitch) explain("=", isSameType, this, that)
       else isSameType(this, that))
    );

    /** Does this type implement symbol `sym` with same or stronger type? */
    def specializes(sym: Symbol): Boolean =
      if (explainSwitch) explain("specializes", specializesSym, this, sym)
      else specializesSym(this, sym)

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
    def looselyMatches(that: Type): Boolean = matchesType(this, that, true)

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

    /** The maximum depth (@see maxDepth)
     *  of each type in the BaseTypeSeq of this type.
     */
    def baseTypeSeqDepth: Int = 1

    /** The list of all baseclasses of this type (including its own typeSymbol)
     *  in reverse linearization order, starting with the class itself and ending
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
        else abort()
      }
      -1
    }

    /** If this is a poly- or methodtype, a copy with cloned type / value parameters
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
     *  after `maxTostringRecursions` recursion levels. Uses `safeToString`
     *  to produce a string on each level.
     */
    override def toString: String = typeToString(this)

    /** Method to be implemented in subclasses.
     *  Converts this type to a string in calling toString for its parts.
     */
    def safeToString: String = super.toString

    /** The string representation of this type, with singletypes explained. */
    def toLongString = {
      val str = toString
      if (str == "type") widen.toString
      else if ((str endsWith ".type") && !typeSymbol.isModuleClass) str + " (with underlying type " + widen + ")"
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
        if (!e.sym.hasFlag(excludedFlags)) {
          if (sym == NoSymbol) sym = e.sym
          else {
            if (alts.isEmpty) alts = List(sym)
            alts = e.sym :: alts
          }
        }
        e = decls.lookupNextEntry(e)
      }
      if (alts.isEmpty) sym
      else (baseClasses.head.newOverloaded(this, alts))
    }

    /**
     *  Find member(s) in this type. If several members matching criteria are found, they are
     *  returned in an OverloadedSymbol
     *
     *  @param name           The member's name, where nme.ANYNAME means `unspecified`
     *  @param excludedFlags  Returned members do not have these flags
     *  @param requiredFlags  Returned members do have these flags
     *  @param stableOnly     If set, return only members that are types or stable values
     */
    //TODO: use narrow only for modules? (correct? efficiency gain?)
    def findMember(name: Name, excludedFlags: Long, requiredFlags: Long, stableOnly: Boolean): Symbol = {
      // if this type contains type variables, put them to sleep for a while -- don't just wipe them out by
      // replacing them by the corresponding type parameter, as that messes up (e.g.) type variables in type refinements
      // without this, the matchesType call would lead to type variables on both sides
      // of a subtyping/equality judgement, which can lead to recursive types being constructed.
      // See (t0851) for a situation where this happens.
      val suspension: List[TypeVar] = if (this.isGround) null else suspendTypeVarsInType(this)

      incCounter(findMemberCount)
      val start = startTimer(findMemberNanos)

      //Console.println("find member " + name.decode + " in " + this + ":" + this.baseClasses)//DEBUG
      var members: Scope = null
      var member: Symbol = NoSymbol
      var excluded = excludedFlags | DEFERRED
      var continue = true
      var self: Type = null
      var membertpe: Type = null
      while (continue) {
        continue = false
        val bcs0 = baseClasses
        var bcs = bcs0
        while (!bcs.isEmpty) {
          val decls = bcs.head.info.decls
          var entry =
            if (name == nme.ANYNAME) decls.elems else decls.lookupEntry(name)
          while (entry ne null) {
            val sym = entry.sym
            if (sym hasAllFlags requiredFlags) {
              val excl = sym.getFlag(excluded)
              if (excl == 0L &&
                  (// omit PRIVATE LOCALS unless selector class is contained in class owning the def.
                   (bcs eq bcs0) ||
                   !sym.isPrivateLocal ||
                   (bcs0.head.hasTransOwner(bcs.head)))) {
                if (name.isTypeName || stableOnly && sym.isStable) {
                  stopTimer(findMemberNanos, start)
                  if (suspension ne null) suspension foreach (_.suspended = false)
                  return sym
                } else if (member == NoSymbol) {
                  member = sym
                } else if (members eq null) {
                  if (member.name != sym.name ||
                      !(member == sym ||
                        member.owner != sym.owner &&
                        !sym.isPrivate && {
                          if (self eq null) self = this.narrow
                          if (membertpe eq null) membertpe = self.memberType(member)
                          (membertpe matches self.memberType(sym))
                        })) {
                    members = newScope
                    members enter member
                    members enter sym
                  }
                } else {
                  var prevEntry = members.lookupEntry(sym.name)
                  var symtpe: Type = null
                  while ((prevEntry ne null) &&
                         !(prevEntry.sym == sym ||
                           prevEntry.sym.owner != sym.owner &&
                           !sym.hasFlag(PRIVATE) && {
                             if (self eq null) self = this.narrow
                             if (symtpe eq null) symtpe = self.memberType(sym)
                             self.memberType(prevEntry.sym) matches symtpe
                           })) {
                    prevEntry = members lookupNextEntry prevEntry
                  }
                  if (prevEntry eq null) {
                    members enter sym
                  }
                }
              } else if (excl == DEFERRED.toLong) {
                continue = true
              }
            }
            entry = if (name == nme.ANYNAME) entry.next else decls lookupNextEntry entry
          } // while (entry ne null)
          // excluded = excluded | LOCAL
          bcs = if (name == nme.CONSTRUCTOR) Nil else bcs.tail
        } // while (!bcs.isEmpty)
        excluded = excludedFlags
      } // while (continue)
      stopTimer(findMemberNanos, start)
      if (suspension ne null) suspension foreach (_.suspended = false)
      if (members eq null) {
        if (member == NoSymbol) incCounter(noMemberCount)
        member
      } else {
        incCounter(multMemberCount)
        baseClasses.head.newOverloaded(this, members.toList)
      }
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

    /** Remove any annotations from this type and from any
     *  types embedded in this type. */
    def stripAnnotations = StripAnnotationsMap(this)

    /** Set the self symbol of an annotated type, or do nothing
     *  otherwise.  */
    def withSelfsym(sym: Symbol) = this

    /** The selfsym of an annotated type, or NoSymbol of anything else */
    def selfsym: Symbol = NoSymbol

    /** The kind of this type; used for debugging */
    def kind: String = "unknown type of class "+getClass()
  }

// Subclasses ------------------------------------------------------------

  trait UniqueType extends Product {
    final override val hashCode = scala.runtime.ScalaRunTime._hashCode(this)
  }

 /** A base class for types that defer some operations
   *  to their immediate supertype.
   */
  abstract class SubType extends Type {
    def supertype: Type
    override def parents: List[Type] = supertype.parents
    override def decls: Scope = supertype.decls
    override def baseType(clazz: Symbol): Type = supertype.baseType(clazz)
    override def baseTypeSeq: BaseTypeSeq = supertype.baseTypeSeq
    override def baseTypeSeqDepth: Int = supertype.baseTypeSeqDepth
    override def baseClasses: List[Symbol] = supertype.baseClasses
    override def isNotNull = supertype.isNotNull
  }

  case class NotNullType(override val underlying: Type) extends SubType with RewrappingTypeProxy {
    def supertype = underlying
    protected def rewrap(newtp: Type): Type = NotNullType(newtp)
    override def isNotNull: Boolean = true
    override def notNull = this
    override def deconst: Type = underlying //todo: needed?
    override def safeToString: String = underlying.toString + " with NotNull"
    override def kind = "NotNullType"
  }

  /** A base class for types that represent a single value
   *  (single-types and this-types).
   */
  abstract class SingletonType extends SubType with SimpleTypeProxy {
    def supertype = underlying
    override def isTrivial = false
    override def isStable = true
    override def isVolatile = underlying.isVolatile
    override def widen: Type = underlying.widen
    override def baseTypeSeq: BaseTypeSeq = {
      incCounter(singletonBaseTypeSeqCount)
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
    // override def isNullable: Boolean = true
    override def kind = "ErrorType"
  }

  /** An object representing an unknown type, used during type inference.
   *  If you see WildcardType outside of inference it is almost certainly a bug.
   */
  case object WildcardType extends Type {
    override def isWildcard = true
    override def safeToString: String = "?"
    // override def isNullable: Boolean = true
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
  case class BoundedWildcardType(override val bounds: TypeBounds) extends Type {
    override def isWildcard = true
    override def safeToString: String = "?" + bounds
    override def kind = "BoundedWildcardType"
  }

  object BoundedWildcardType extends BoundedWildcardTypeExtractor

  /** An object representing a non-existing type */
  case object NoType extends Type {
    override def isTrivial: Boolean = true
    override def safeToString: String = "<notype>"
    // override def isNullable: Boolean = true
    override def kind = "NoType"
  }

  /** An object representing a non-existing prefix */
  case object NoPrefix extends Type {
    override def isTrivial: Boolean = true
    override def isStable: Boolean = true
    override def prefixString = ""
    override def safeToString: String = "<noprefix>"
    // override def isNullable: Boolean = true
    override def kind = "NoPrefixType"
  }

  /** A class for this-types of the form <sym>.this.type
   */
  abstract case class ThisType(sym: Symbol) extends SingletonType {
    assert(sym.isClass)
    //assert(sym.isClass && !sym.isModuleClass || sym.isRoot, sym)
    override def isTrivial: Boolean = sym.isPackageClass
    override def isNotNull = true
    override def typeSymbol = sym
    override def underlying: Type = sym.typeOfThis
    override def isVolatile = false
    override def isHigherKinded = sym.isRefinementClass && underlying.isHigherKinded
    override def prefixString =
      if (settings.debug.value) sym.nameString + ".this."
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

  final class UniqueThisType(sym: Symbol) extends ThisType(sym) with UniqueType { }

  object ThisType extends ThisTypeExtractor {
    def apply(sym: Symbol): Type = {
      if (!phase.erasedTypes) unique(new UniqueThisType(sym))
      else if (sym.isImplClass) sym.typeOfThis
      else sym.tpe
    }
  }

  /** A class for singleton types of the form `<prefix>.<sym.name>.type`.
   *  Cannot be created directly; one should always use `singleType` for creation.
   */
  abstract case class SingleType(pre: Type, sym: Symbol) extends SingletonType {
    override val isTrivial: Boolean = pre.isTrivial
    override def isGround = sym.isPackageClass || pre.isGround

    // override def isNullable = underlying.isNullable
    override def isNotNull = underlying.isNotNull
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

    override def isVolatile : Boolean = underlying.isVolatile && !sym.isStable
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

  final class UniqueSingleType(pre: Type, sym: Symbol) extends SingleType(pre, sym) with UniqueType { }

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
        tpe.underlyingCache = if (tpe.sym == NoSymbol) ThisType(RootClass) else tpe.pre.memberType(tpe.sym).resultType;
        assert(tpe.underlyingCache ne tpe, tpe)
      }
    }
  }

  abstract case class SuperType(thistpe: Type, supertpe: Type) extends SingletonType {
    override val isTrivial: Boolean = thistpe.isTrivial && supertpe.isTrivial
    override def isNotNull = true;
    override def typeSymbol = thistpe.typeSymbol
    override def underlying = supertpe
    override def prefix: Type = supertpe.prefix
    override def prefixString = thistpe.prefixString.replaceAll("""\bthis\.$""", "super.")
    override def narrow: Type = thistpe.narrow
    override def kind = "SuperType"
  }

  final class UniqueSuperType(thistp: Type, supertp: Type) extends SuperType(thistp, supertp) with UniqueType { }

  object SuperType extends SuperTypeExtractor {
    def apply(thistp: Type, supertp: Type): Type = {
      if (phase.erasedTypes) supertp
      else unique(new UniqueSuperType(thistp, supertp))
    }
  }

  /** A class for the bounds of abstract types and type parameters
   */
  abstract case class TypeBounds(lo: Type, hi: Type) extends SubType {
    def supertype = hi
    override val isTrivial: Boolean = lo.isTrivial && hi.isTrivial
    override def bounds: TypeBounds = this
    def containsType(that: Type) = that match {
      case TypeBounds(_, _) => that <:< this
      case _                => lo <:< that && that <:< hi
    }
    private def lowerString = if (emptyLowerBound) "" else " >: " + lo
    private def upperString = if (emptyUpperBound) "" else " <: " + hi
    private def emptyLowerBound = lo.typeSymbolDirect eq NothingClass
    private def emptyUpperBound = hi.typeSymbolDirect eq AnyClass
    def isEmptyBounds = emptyLowerBound && emptyUpperBound

    // override def isNullable: Boolean = NullClass.tpe <:< lo;
    override def safeToString = lowerString + upperString
    override def kind = "TypeBoundsType"
  }

  final class UniqueTypeBounds(lo: Type, hi: Type) extends TypeBounds(lo, hi) with UniqueType { }

  object TypeBounds extends TypeBoundsExtractor {
    def empty: TypeBounds           = apply(NothingClass.tpe, AnyClass.tpe)
    def upper(hi: Type): TypeBounds = apply(NothingClass.tpe, hi)
    def lower(lo: Type): TypeBounds = apply(lo, AnyClass.tpe)
    def apply(lo: Type, hi: Type): TypeBounds = {
      unique(new UniqueTypeBounds(lo, hi)).asInstanceOf[TypeBounds]
    }
  }

  /** A common base class for intersection types and class types
   */
  abstract class CompoundType extends Type {

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

    override def baseTypeSeqDepth: Int = baseTypeSeq.maxDepth

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
    override def isNotNull: Boolean = parents exists (_.isNotNull)

    override def isStructuralRefinement: Boolean =
      typeSymbol.isAnonOrRefinementClass && decls.exists(_.isPossibleInRefinement)

    // override def isNullable: Boolean =
    // parents forall (p => p.isNullable && !p.typeSymbol.isAbstractType);

    override def safeToString: String = parentsString(parents) + (
      (if (settings.debug.value || parents.isEmpty || (decls.elems ne null))
        decls.mkString("{", "; ", "}") else "")
    )
  }

  protected def defineBaseTypeSeqOfCompoundType(tpe: CompoundType) = {
    val period = tpe.baseTypeSeqPeriod
    if (period != currentPeriod) {
      tpe.baseTypeSeqPeriod = currentPeriod
      if (!isValidForBaseClasses(period)) {
        if (tpe.parents.exists(_.exists(_.isInstanceOf[TypeVar]))) {
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
              case Some(sym) => sym.tpe
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
          incCounter(compoundBaseTypeSeqCount)
          tpe.baseTypeSeqCache = undetBaseTypeSeq
          tpe.baseTypeSeqCache = if (tpe.typeSymbol.isRefinementClass)
            tpe.memo(compoundBaseTypeSeq(tpe))(_.baseTypeSeq updateHead tpe.typeSymbol.tpe)
          else
            compoundBaseTypeSeq(tpe)
          // [Martin] suppressing memo-ization solves the problem with "same type after erasure" errors
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

  protected def defineBaseClassesOfCompoundType(tpe: CompoundType) = {
    def computeBaseClasses: List[Symbol] =
      if (tpe.parents.isEmpty) List(tpe.typeSymbol)
      else {
        //Console.println("computing base classes of " + typeSymbol + " at phase " + phase);//DEBUG
        // optimized, since this seems to be performance critical
        val superclazz = tpe.firstParent
        var mixins = tpe.parents.tail
        val sbcs = superclazz.baseClasses
        var bcs = sbcs
        def isNew(clazz: Symbol): Boolean =
          superclazz.baseTypeIndex(clazz) < 0 &&
          { var p = bcs;
            while ((p ne sbcs) && (p.head != clazz)) p = p.tail;
            p eq sbcs
          }
        while (!mixins.isEmpty) {
          def addMixinBaseClasses(mbcs: List[Symbol]): List[Symbol] =
            if (mbcs.isEmpty) bcs
            else if (isNew(mbcs.head)) mbcs.head :: addMixinBaseClasses(mbcs.tail)
            else addMixinBaseClasses(mbcs.tail)
          bcs = addMixinBaseClasses(mixins.head.baseClasses)
          mixins = mixins.tail
        }
        tpe.typeSymbol :: bcs
      }
    val period = tpe.baseClassesPeriod
    if (period != currentPeriod) {
      tpe.baseClassesPeriod = currentPeriod
      if (!isValidForBaseClasses(period)) {
        tpe.baseClassesCache = null
        tpe.baseClassesCache = tpe.memo(computeBaseClasses)(tpe.typeSymbol :: _.baseClasses.tail)
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
                         override val decls: Scope) extends CompoundType {

    override def isHigherKinded = (
      parents.nonEmpty &&
      (parents forall (_.isHigherKinded)) &&
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
      if (decls.isEmpty && flattened.tail.isEmpty) {
        flattened.head
      } else if (flattened != parents) {
        refinedType(flattened, if (typeSymbol eq NoSymbol) NoSymbol else typeSymbol.owner, decls, NoPosition)
      } else if (isHigherKinded) {
        // MO to AM: This is probably not correct
        // If they are several higher-kinded parents with different bounds we need
        // to take the intersection of their bounds
        typeFun(
          typeParams,
          RefinedType(
            parents map {
              case TypeRef(pre, sym, List()) => TypeRef(pre, sym, dummyArgs)
              case p => p
            },
            decls,
            typeSymbol))
      } else super.normalize
    }

    /** A refined type P1 with ... with Pn { decls } is volatile if
     *  one of the parent types Pi is an abstract type, and
     *  either i > 1, or decls or a following parent Pj, j > 1, contributes
     *  an abstract member.
     *  A type contributes an abstract member if it has an abstract member which
     *  is also a member of the whole refined type. A scope `decls` contributes
     *  an abstract member if it has an abstract definition which is also
     *  a member of the whole type.
     */
    override def isVolatile = {
      def isVisible(m: Symbol) =
        this.nonPrivateMember(m.name).alternatives contains m
      def contributesAbstractMembers(p: Type) =
        p.deferredMembers exists isVisible

      ((parents exists (_.isVolatile))
       ||
       (parents dropWhile (! _.typeSymbol.isAbstractType) match {
         case ps @ (_ :: ps1) =>
           (ps ne parents) ||
           (ps1 exists contributesAbstractMembers) ||
           (decls.iterator exists (m => m.isDeferred && isVisible(m)))
         case _ =>
           false
       }))
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
    override val typeSymbol: Symbol) extends CompoundType
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

    /** The initialization state of the class: UnInialized --> Initializing --> Initialized
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
            if (settings.debug.value && !sameLength(tparams, args))
              debugwarn("Mismatched zip in computeRefs(): " + sym.info.typeParams + ", " + args)

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
          var thatInfo = classInfo(target)
          if (thatInfo.state != Initialized)
            change = change | thatInfo.propagate()
          addRefs(NonExpansive, from, thatInfo.getRefs(NonExpansive, target))
          addRefs(Expansive, from, thatInfo.getRefs(Expansive, target))
        }
      for ((from, targets) <- refs(Expansive).iterator)
        for (target <- targets) {
          var thatInfo = classInfo(target)
          if (thatInfo.state != Initialized)
            change = change | thatInfo.propagate()
          addRefs(Expansive, from, thatInfo.getRefs(NonExpansive, target))
        }
      change = change || refs(0) != lastRefs(0) || refs(1) != lastRefs(1)
      if (change) state = Initializing
      //else Console.println("Propagate "+symbol+", final expansive = "+refs(Expansive)+", nonexpansive = "+refs(NonExpansive))//DEBUG
      change
    }

    // override def isNullable: Boolean =
    // symbol == AnyClass ||
    // symbol != NothingClass && (symbol isSubClass ObjectClass) && !(symbol isSubClass NonNullClass);

    // override def isNonNull: Boolean = symbol == NonNullClass || super.isNonNull;
    override def kind = "ClassInfoType"

    override def safeToString =
      if (settings.debug.value || decls.size > 1)
        formattedToString
      else
        super.safeToString

    /** A nicely formatted string with newlines and such.
     */
    def formattedToString: String =
      parents.mkString("\n        with ") +
      (if (settings.debug.value || parents.isEmpty || (decls.elems ne null))
        decls.mkString(" {\n  ", "\n  ", "\n}") else "")
  }

  object ClassInfoType extends ClassInfoTypeExtractor

  class PackageClassInfoType(decls: Scope, clazz: Symbol)
  extends ClassInfoType(List(), decls, clazz)

  /** A class representing a constant type.
   *
   *  @param value ...
   */
  abstract case class ConstantType(value: Constant) extends SingletonType {
    override def underlying: Type = value.tpe
    assert(underlying.typeSymbol != UnitClass)
    override def isTrivial: Boolean = true
    override def isNotNull = value.value != null
    override def deconst: Type = underlying
    override def safeToString: String =
      underlying.toString + "(" + value.escapedStringValue + ")"
    // override def isNullable: Boolean = value.value eq null
    // override def isNonNull: Boolean = value.value ne null
    override def kind = "ConstantType"
  }

  final class UniqueConstantType(value: Constant) extends ConstantType(value) with UniqueType {
    /** Save the type of `value`. For Java enums, it depends on finding the linked class,
     *  which might not be found after `flatten`. */
    private lazy val _tpe: Type = value.tpe
    override def underlying: Type = _tpe
  }

  object ConstantType extends ConstantTypeExtractor {
    def apply(value: Constant): ConstantType = {
      val tpe = new UniqueConstantType(value)
      if (value.tag == ClazzTag) {
        // if we carry a classOf, we might be in trouble
        // http://groups.google.com/group/scala-internals/browse_thread/thread/45185b341aeb6a30
        // I don't have time for a thorough fix, so I put a hacky workaround here
        val alreadyThere = uniques findEntry tpe
        if ((alreadyThere ne null) && (alreadyThere ne tpe) && (alreadyThere.toString != tpe.toString)) {
          // we need to remove a stale type that has the same hashcode as we do
          // HashSet doesn't support removal, and this makes our task non-trivial
          // also we cannot simply recreate it, because that'd skew hashcodes (that change over time, omg!)
          // the only solution I can see is getting into the underlying array and sneakily manipulating it
          val ftable = uniques.getClass.getDeclaredFields().find(f => f.getName endsWith "table").get
          ftable.setAccessible(true)
          val table = ftable.get(uniques).asInstanceOf[Array[AnyRef]]
          def overwrite(hc: Int, x: Type) {
            def index(x: Int): Int = math.abs(x % table.length)
            var h = index(hc)
            var entry = table(h)
            while (entry ne null) {
              if (x == entry)
                table(h) = x
              h = index(h + 1)
              entry = table(h)
            }
          }
          overwrite(tpe.##, tpe)
        }
      }
      unique(tpe).asInstanceOf[ConstantType]
    }
  }

  /* Syncnote: The `volatile` var and `pendingVolatiles` mutable set need not be protected
   * with synchronized, because they are accessed only from isVolatile, which is called only from
   * Typer.
   */
  private var volatileRecursions: Int = 0
  private val pendingVolatiles = new mutable.HashSet[Symbol]

  class ArgsTypeRef(pre0: Type, sym0: Symbol, args0: List[Type]) extends TypeRef(pre0, sym0, args0) with UniqueType {
    require(args0.nonEmpty, this)

    /** No unapplied type params size it has (should have) equally as many args. */
    override def isHigherKinded = false
    override def typeParams = Nil

    override def transform(tp: Type): Type = {
      // This situation arises when a typevar is encountered for which
      // too little information is known to determine its kind, and
      // it later turns out not to have kind *. See SI-4070.  Only
      // logging it for now.
      if (sym.typeParams.size != args.size)
        log("!!! %s.transform(%s), but tparams.isEmpty and args=".format(this, tp, args))

      asSeenFromOwner(tp).instantiateTypeParams(sym.typeParams, args)
    }

    // note: does not go through typeRef. There's no need to because
    // neither `pre` nor `sym` changes.  And there's a performance
    // advantage to call TypeRef directly.
    override def typeConstructor = TypeRef(pre, sym, Nil)
  }

  class ModuleTypeRef(pre0: Type, sym0: Symbol) extends NoArgsTypeRef(pre0, sym0) with ClassTypeRef {
    require(sym.isModuleClass, sym)
    private[this] var narrowedCache: Type = _
    override def isStable = true
    override def narrow = {
      if (narrowedCache eq null)
        narrowedCache = singleType(pre, sym.sourceModule)

      narrowedCache
    }
    final override def isNotNull = true
    override protected def finishPrefix(rest: String) = objectPrefix + rest
    override def directObjectString = super.safeToString
    override def toLongString = toString
    override def safeToString = narrow.toString
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

  class NoArgsTypeRef(pre0: Type, sym0: Symbol) extends TypeRef(pre0, sym0, Nil) with UniqueType {
    // A reference (in a Scala program) to a type that has type parameters, but where the reference
    // does not include type arguments. Note that it doesn't matter whether the symbol refers
    // to a java or scala symbol, but it does matter whether it occurs in java or scala code.
    // TypeRefs w/o type params that occur in java signatures/code are considered raw types, and are
    // represented as existential types.
    override def isHigherKinded = typeParams.nonEmpty
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
     * several times. Hence, no need to protected with synchronzied in a mutli-threaded
     * usage scenario.
     */
    private var relativeInfoCache: Type = _
    private var memberInfoCache: Type = _

    private[Types] def relativeInfo = {
      val memberInfo = pre.memberInfo(sym)
      if (relativeInfoCache == null || (memberInfo ne memberInfoCache)) {
        memberInfoCache = memberInfo
        relativeInfoCache = transformInfo(memberInfo)
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
    override def isStable   = normalize.isStable
    override def isVolatile = normalize.isVolatile
    override def narrow     = normalize.narrow
    override def thisInfo   = normalize
    override def prefix     = if (this ne normalize) normalize.prefix else pre
    override def termSymbol = if (this ne normalize) normalize.termSymbol else super.termSymbol
    override def typeSymbol = if (this ne normalize) normalize.typeSymbol else sym

    // beta-reduce, but don't do partial application -- cycles have been checked in typeRef
    override protected def normalizeImpl =
      if (typeParamsMatchArgs) betaReduce.normalize
      else if (isHigherKinded) super.normalizeImpl
      else ErrorType

    // isHKSubType0 introduces synthetic type params so that
    // betaReduce can first apply sym.info to typeArgs before calling
    // asSeenFrom.  asSeenFrom then skips synthetic type params, which
    // are used to reduce HO subtyping to first-order subtyping, but
    // which can't be instantiated from the given prefix and class.
    //
    // this crashes pos/depmet_implicit_tpbetareduce.scala
    // appliedType(sym.info, typeArgs).asSeenFrom(pre, sym.owner)
    def betaReduce = transform(sym.info.resultType)

    // #3731: return sym1 for which holds: pre bound sym.name to sym and
    // pre1 now binds sym.name to sym1, conceptually exactly the same
    // symbol as sym.  The selection of sym on pre must be updated to the
    // selection of sym1 on pre1, since sym's info was probably updated
    // by the TypeMap to yield a new symbol, sym1 with transformed info.
    // @returns sym1
    override def coevolveSym(pre1: Type): Symbol =
      if (pre eq pre1) sym else (pre, pre1) match {
        // don't look at parents -- it would be an error to override alias types anyway
        case (RefinedType(_, _), RefinedType(_, decls1)) => decls1 lookup sym.name
        // TODO: is there another way a typeref's symbol can refer to a symbol defined in its pre?
        case _                                           => sym
      }
    override def kind = "AliasTypeRef"
  }

  trait AbstractTypeRef extends NonClassTypeRef {
    require(sym.isAbstractType, sym)

    /** Syncnote: Pure performance caches; no need to synchronize in multi-threaded environment
     */
    private var symInfoCache: Type = _
    private var thisInfoCache: Type = _

    override def isVolatile = {
      // need to be careful not to fall into an infinite recursion here
      // because volatile checking is done before all cycles are detected.
      // the case to avoid is an abstract type directly or
      // indirectly upper-bounded by itself. See #2918
      try {
        volatileRecursions += 1
        if (volatileRecursions < LogVolatileThreshold)
          bounds.hi.isVolatile
        else if (pendingVolatiles(sym))
          true // we can return true here, because a cycle will be detected
               // here afterwards and an error will result anyway.
        else
          try {
            pendingVolatiles += sym
            bounds.hi.isVolatile
          } finally {
            pendingVolatiles -= sym
          }
      } finally {
        volatileRecursions -= 1
      }
    }

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
    override def isStable = bounds.hi.typeSymbol isSubClass SingletonClass
    override def bounds   = thisInfo.bounds
    // def transformInfo(tp: Type): Type = appliedType(tp.asSeenFrom(pre, sym.owner), typeArgsOrDummies)
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
  abstract case class TypeRef(pre: Type, sym: Symbol, args: List[Type]) extends Type {
    private[reflect] var parentsCache: List[Type]      = _
    private[reflect] var parentsPeriod                 = NoPeriod
    private[reflect] var baseTypeSeqCache: BaseTypeSeq = _
    private[reflect] var baseTypeSeqPeriod             = NoPeriod
    private var normalized: Type                       = _

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

    def etaExpand: Type = {
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
    override def isStable         = (sym eq NothingClass) || (sym eq SingletonClass)
    override def prefix           = pre
    override def termSymbol       = super.termSymbol
    override def termSymbolDirect = super.termSymbol
    override def typeArgs         = args
    override def typeOfThis       = transform(sym.typeOfThis)
    override def typeSymbol       = sym
    override def typeSymbolDirect = sym

    override lazy val isTrivial: Boolean =
      !sym.isTypeParameter && pre.isTrivial && args.forall(_.isTrivial)

    override def isNotNull =
      sym.isModuleClass || sym == NothingClass || (sym isNonBottomSubClass NotNullClass) || super.isNotNull

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

    protected[Types] def baseTypeSeqImpl: BaseTypeSeq = sym.info.baseTypeSeq map transform

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
         settings.debug.value
      || !shorthands(sym.fullName)
      || sym.ownerChain.exists(s => !s.isClass)
    )
    private def preString  = if (needsPreString) pre.prefixString else ""
    private def argsString = if (args.isEmpty) "" else args.mkString("[", ",", "]")

    def refinementString = (
      if (sym.isStructuralRefinement) (
        decls filter (sym => sym.isPossibleInRefinement && sym.isPublic)
          map (_.defString)
          mkString("{", "; ", "}")
      )
      else ""
    )

    protected def finishPrefix(rest: String) = (
      if (sym.isInitialized && sym.isAnonymousClass && !phase.erasedTypes)
        parentsString(thisInfo.parents) + refinementString
      else rest
    )
    private def customToString = sym match {
      case RepeatedParamClass => args.head + "*"
      case ByNameParamClass   => "=> " + args.head
      case _                  =>
        def targs = normalize.typeArgs

        if (isFunctionType(this)) {
          // Aesthetics: printing Function1 as T => R rather than (T) => R
          // ...but only if it's not a tuple, so ((T1, T2)) => R is distinguishable
          // from (T1, T2) => R.
          targs match {
            case in :: out :: Nil if !isTupleType(in) =>
              // A => B => C should be (A => B) => C or A => (B => C)
              val in_s  = if (isFunctionType(in)) "(" + in + ")" else "" + in
              val out_s = if (isFunctionType(out)) "(" + out + ")" else "" + out
              in_s + " => " + out_s
            case xs =>
              xs.init.mkString("(", ", ", ")") + " => " + xs.last
          }
        }
        else if (isTupleType(this))
          targs.mkString("(", ", ", if (hasLength(targs, 1)) ",)" else ")")
        else if (sym.isAliasType && prefixChain.exists(_.termSymbol.isSynthetic) && (this ne this.normalize))
          "" + normalize
        else
          ""
    }
    override def safeToString = {
      val custom = if (settings.debug.value) "" else customToString
      if (custom != "") custom
      else finishPrefix(preString + sym.nameString + argsString)
    }
    override def prefixString = "" + (
      if (settings.debug.value)
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
    override def kind = "TypeRef"
  }

  object TypeRef extends TypeRefExtractor {
    def apply(pre: Type, sym: Symbol, args: List[Type]): Type = unique({
      if (args.nonEmpty) {
        if (sym.isAliasType)              new ArgsTypeRef(pre, sym, args) with AliasTypeRef
        else if (sym.isAbstractType)      new ArgsTypeRef(pre, sym, args) with AbstractTypeRef
        else                              new ArgsTypeRef(pre, sym, args) with ClassTypeRef
      }
      else {
        if (sym.isAliasType)              new NoArgsTypeRef(pre, sym) with AliasTypeRef
        else if (sym.isAbstractType)      new NoArgsTypeRef(pre, sym) with AbstractTypeRef
        else if (sym.isRefinementClass)   new RefinementTypeRef(pre, sym)
        else if (sym.isPackageClass)      new PackageTypeRef(pre, sym)
        else if (sym.isModuleClass)       new ModuleTypeRef(pre, sym)
        else                              new NoArgsTypeRef(pre, sym) with ClassTypeRef
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
        tpe.parentsCache = List(AnyClass.tpe)
      }
    }
  }

  protected def defineBaseTypeSeqOfTypeRef(tpe: TypeRef) = {
    val period = tpe.baseTypeSeqPeriod
    if (period != currentPeriod) {
      tpe.baseTypeSeqPeriod = currentPeriod
      if (!isValidForBaseClasses(period)) {
        incCounter(typerefBaseTypeSeqCount)
        tpe.baseTypeSeqCache = undetBaseTypeSeq
        tpe.baseTypeSeqCache = tpe.baseTypeSeqImpl
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
                        override val resultType: Type) extends Type {
    override def isTrivial: Boolean = isTrivial0 && (resultType eq resultType.withoutAnnotations)
    private lazy val isTrivial0 =
      resultType.isTrivial && params.forall{p => p.tpe.isTrivial &&  (
        !(params.exists(_.tpe.contains(p)) || resultType.contains(p)))
      }

    def isImplicit = params.nonEmpty && params.head.isImplicit
    def isJava = false // can we do something like for implicits? I.e. do Java methods without parameters need to be recognized?

    //assert(paramTypes forall (pt => !pt.typeSymbol.isImplClass))//DEBUG
    override def paramSectionCount: Int = resultType.paramSectionCount + 1

    override def paramss: List[List[Symbol]] = params :: resultType.paramss

    override def paramTypes = params map (_.tpe)

    override def boundSyms = resultType.boundSyms ++ params

    override def resultType(actuals: List[Type]) =
      if (isTrivial || phase.erasedTypes) resultType
      else if (sameLength(actuals, params)) {
        val idm = new InstantiateDependentMap(params, actuals)
        val res = idm(resultType)
        existentialAbstraction(idm.existentialsNeeded, res)
      }
      else existentialAbstraction(params, resultType)

    // implicit args can only be depended on in result type:
    //TODO this may be generalised so that the only constraint is dependencies are acyclic
    def approximate: MethodType = MethodType(params, resultApprox)

    override def finalResultType: Type = resultType.finalResultType

    override def safeToString = paramString(this) + resultType

    override def cloneInfo(owner: Symbol) = {
      val vparams = cloneSymbolsAtOwner(params, owner)
      copyMethodType(this, vparams, resultType.substSym(params, vparams).cloneInfo(owner))
    }

    override def atOwner(owner: Symbol) =
      if ((params exists (_.owner != owner)) || (resultType.atOwner(owner) ne resultType))
        cloneInfo(owner)
      else
        this

    override def kind = "MethodType"
  }

  object MethodType extends MethodTypeExtractor

  class JavaMethodType(ps: List[Symbol], rt: Type) extends MethodType(ps, rt) {
    override def isJava = true
  }

  case class NullaryMethodType(override val resultType: Type) extends Type {
    override def isTrivial = resultType.isTrivial && (resultType eq resultType.withoutAnnotations)
    override def prefix: Type = resultType.prefix
    override def narrow: Type = resultType.narrow
    override def finalResultType: Type = resultType.finalResultType
    override def termSymbol: Symbol = resultType.termSymbol
    override def typeSymbol: Symbol = resultType.typeSymbol
    override def parents: List[Type] = resultType.parents
    override def decls: Scope = resultType.decls
    override def baseTypeSeq: BaseTypeSeq = resultType.baseTypeSeq
    override def baseTypeSeqDepth: Int = resultType.baseTypeSeqDepth
    override def baseClasses: List[Symbol] = resultType.baseClasses
    override def baseType(clazz: Symbol): Type = resultType.baseType(clazz)
    override def boundSyms = resultType.boundSyms
    override def isVolatile = resultType.isVolatile
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
       extends Type {
    //assert(!(typeParams contains NoSymbol), this)
    assert(typeParams nonEmpty, this) // used to be a marker for nullary method type, illegal now (see @NullaryMethodType)

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
    override def baseTypeSeqDepth: Int = resultType.baseTypeSeqDepth
    override def baseClasses: List[Symbol] = resultType.baseClasses
    override def baseType(clazz: Symbol): Type = resultType.baseType(clazz)
    override def narrow: Type = resultType.narrow
    override def isVolatile = resultType.isVolatile
    override def finalResultType: Type = resultType.finalResultType

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
      if ((typeParams exists (_.owner != owner)) || (resultType.atOwner(owner) ne resultType))
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
                             override val underlying: Type) extends RewrappingTypeProxy
  {
    override protected def rewrap(newtp: Type) = existentialAbstraction(quantified, newtp)

    override def isTrivial = false
    override def isStable: Boolean = false
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

    override def skolemizeExistential(owner: Symbol, origin: AnyRef) =
      deriveType(quantified, tparam => (owner orElse tparam.owner).newExistentialSkolem(tparam, origin))(underlying)

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
     *   - the prefix is not quanitified
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
        if (settings.explaintypes.value) "(" + str + ")" else str
      }
      underlying match {
        case TypeRef(pre, sym, args) if !settings.debug.value && isRepresentableWithWildcards =>
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
      if (quantified exists (_.owner != owner)) cloneInfo(owner) else this

    override def kind = "ExistentialType"

    def withTypeVars(op: Type => Boolean): Boolean = withTypeVars(op, AnyDepth)

    def withTypeVars(op: Type => Boolean, depth: Int): Boolean = {
      val quantifiedFresh = cloneSymbols(quantified)
      val tvars = quantifiedFresh map (tparam => TypeVar(tparam))
      val underlying1 = underlying.instantiateTypeParams(quantified, tvars) // fuse subst quantified -> quantifiedFresh -> tvars
      op(underlying1) && {
        solve(tvars, quantifiedFresh, quantifiedFresh map (x => 0), false, depth) &&
        isWithinBounds(NoPrefix, NoSymbol, quantifiedFresh, tvars map (_.constr.inst))
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

  def overloadedType(pre: Type, alternatives: List[Symbol]): Type =
    if (alternatives.tail.isEmpty) pre memberType alternatives.head
    else OverloadedType(pre, alternatives)

  /** A class remembering a type instantiation for some a set of overloaded
   *  polymorphic symbols.
   *  Not used after phase `typer`.
   */
  case class AntiPolyType(pre: Type, targs: List[Type]) extends Type {
    override def safeToString =
      pre.toString + targs.mkString("(with type arguments ", ", ", ")");
    override def memberType(sym: Symbol) = appliedType(pre.memberType(sym), targs)
//     override def memberType(sym: Symbol) = pre.memberType(sym) match {
//       case PolyType(tparams, restp) =>
//         restp.subst(tparams, targs)
// /* I don't think this is needed, as existential types close only over value types
//       case ExistentialType(tparams, qtpe) =>
//         existentialAbstraction(tparams, qtpe.memberType(sym))
// */
//       case ErrorType =>
//         ErrorType
//     }
    override def kind = "AntiPolyType"
  }

  //private var tidCount = 0  //DEBUG

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

  // Not used yet.
  object HasTypeParams {
    def unapply(tp: Type): Option[(List[Symbol], Type)] = tp match {
      case AnnotatedType(_, tp, _)        => unapply(tp)
      case ExistentialType(tparams, qtpe) => Some((tparams, qtpe))
      case PolyType(tparams, restpe)      => Some((tparams, restpe))
      case _                              => None
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
      /** We can seed the type constraint with the type parameter
       *  bounds as long as the types are concrete.  This should lower
       *  the complexity of the search even if it doesn't improve
       *  any results.
       */
      if (propagateParameterBoundsToTypeVars) {
        val exclude = bounds.isEmptyBounds || bounds.exists(_.typeSymbolDirect.isNonClassType)

        if (exclude) new TypeConstraint
        else TypeVar.trace("constraint", "For " + tparam.fullLocationString)(new TypeConstraint(bounds))
      }
      else new TypeConstraint
    }
    def unapply(tv: TypeVar): Some[(Type, TypeConstraint)]   = Some((tv.origin, tv.constr))
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
          else new TypeVar(origin, constr)
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

  /** Repack existential types, otherwise they sometimes get unpacked in the
   *  wrong location (type inference comes up with an unexpected skolem)
   */
  def repackExistential(tp: Type): Type = (
    if (tp == NoType) tp
    else existentialAbstraction(existentialsInType(tp), tp)
  )
  def containsExistential(tpe: Type) =
    tpe exists (_.typeSymbol.isExistentiallyBound)

  def existentialsInType(tpe: Type) = (
    for (tp <- tpe ; if tp.typeSymbol.isExistentiallyBound) yield
      tp.typeSymbol
  )

  /** Precondition: params.nonEmpty.  (args.nonEmpty enforced structurally.)
   */
  class HKTypeVar(
    _origin: Type,
    _constr: TypeConstraint,
    override val params: List[Symbol]
  ) extends TypeVar(_origin, _constr) {

    require(params.nonEmpty, this)
    override def isHigherKinded          = true
    override protected def typeVarString = params.map(_.name).mkString("[", ", ", "]=>" + originName)
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

    override protected def typeVarString = (
      zippedArgs map { case (p, a) => p.name + "=" + a } mkString (origin + "[", ", ", "]")
    )
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
  class TypeVar(
    val origin: Type,
    val constr0: TypeConstraint
  ) extends Type {
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
    var constr = constr0
    def instValid = constr.instValid
    override def isGround = instValid && constr.inst.isGround

    /** The variable's skolemization level */
    val level = skolemizationLevel

    /** Two occurrences of a higher-kinded typevar, e.g. `?CC[Int]` and `?CC[String]`, correspond to
     *  ''two instances'' of `TypeVar` that share the ''same'' `TypeConstraint`.
     *
     *  `constr` for `?CC` only tracks type constructors anyway,
     *   so when `?CC[Int] <:< List[Int]` and `?CC[String] <:< Iterable[String]`
     *  `?CC's` hibounds contains List and Iterable.
     */
    def applyArgs(newArgs: List[Type]): TypeVar = (
      if (newArgs.isEmpty && typeArgs.isEmpty)
        this
      else if (newArgs.size == params.size) {
        val tv = TypeVar(origin, constr, newArgs, params)
        TypeVar.trace("applyArgs", "In " + originLocation + ", apply args " + newArgs.mkString(", ") + " to " + originName)(tv)
      }
      else
        throw new Error("Invalid type application in TypeVar: " + params + ", " + newArgs)
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
    // typevar, which means we'll need to repack our constr.inst into a fresh
    // existential.
    // were we compared to skolems at a higher skolemizationLevel?
    // EXPERIMENTAL: value will not be considered unless enableTypeVarExperimentals is true
    // see SI-5729 for why this is still experimental
    private var encounteredHigherLevel = false
    private def shouldRepackType = enableTypeVarExperimentals && encounteredHigherLevel

    // <region name="constraint mutators + undoLog">
    // invariant: before mutating constr, save old state in undoLog
    // (undoLog is used to reset constraints to avoid piling up unrelated ones)
    def setInst(tp: Type) {
//      assert(!(tp containsTp this), this)
      undoLog record this
      // if we were compared against later typeskolems, repack the existential,
      // because skolems are only compatible if they were created at the same level
      val res = if (shouldRepackType) repackExistential(tp) else tp
      constr.inst = TypeVar.trace("setInst", "In " + originLocation + ", " + originName + "=" + res)(res)
    }

    def addLoBound(tp: Type, isNumericBound: Boolean = false) {
      assert(tp != this, tp) // implies there is a cycle somewhere (?)
      //println("addLoBound: "+(safeToString, debugString(tp))) //DEBUG
      undoLog record this
      constr.addLoBound(tp, isNumericBound)
    }

    def addHiBound(tp: Type, isNumericBound: Boolean = false) {
      // assert(tp != this)
      //println("addHiBound: "+(safeToString, debugString(tp))) //DEBUG
      undoLog record this
      constr.addHiBound(tp, isNumericBound)
    }
    // </region>

    // ignore subtyping&equality checks while true -- see findMember
    private[Types] var suspended = false

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

      /** Simple case: type arguments can be ignored, because either this typevar has
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
      def unifySimple = (
        (params.isEmpty || tp.typeSymbol == NothingClass || tp.typeSymbol == AnyClass) && {
          addBound(tp)
          true
        }
      )

      /** Full case: involving a check of the form
       *  {{{
       *    TC1[T1,..., TN] <: TC2[T'1,...,T'N]
       *  }}}
       *  Checks subtyping of higher-order type vars, and uses variances as defined in the
       *  type parameter we're trying to infer (the result will be sanity-checked later).
       */
      def unifyFull(tpe: Type) = {
        // The alias/widen variations are often no-ops.
        val tpes = (
          if (isLowerBound) List(tpe, tpe.widen, tpe.dealias, tpe.widen.dealias).distinct
          else List(tpe)
        )
        tpes exists { tp =>
          val lhs = if (isLowerBound) tp.typeArgs else typeArgs
          val rhs = if (isLowerBound) typeArgs else tp.typeArgs

          sameLength(lhs, rhs) && {
            // this is a higher-kinded type var with same arity as tp.
            // side effect: adds the type constructor itself as a bound
            addBound(tp.typeConstructor)
            isSubArgs(lhs, rhs, params)
          }
        }
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
      // In a side-effect free universe, checking tp and tp.parents beofre checking tp.baseTypeSeq
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
      else if (constr.instValid)  // type var is already set
        checkSubtype(tp, constr.inst)
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
//      println("regTypeEq: "+(safeToString, debugString(tp), tp.getClass, if (typeVarLHS) "in LHS" else "in RHS", if (suspended) "ZZ" else if (constr.instValid) "IV" else "")) //@MDEBUG
//      println("constr: "+ constr)
      def checkIsSameType(tp: Type) =
        if(typeVarLHS) constr.inst =:= tp
        else           tp          =:= constr.inst

      if (suspended) tp =:= origin
      else if (constr.instValid) checkIsSameType(tp)
      else isRelatable(tp) && {
        val newInst = wildcardToTypeVarMap(tp)
        (constr isWithinBounds newInst) && { setInst(tp); true }
      }
    }

    /**
     * `?A.T =:= tp` is rewritten as the constraint `?A <: {type T = tp}`
     *
     * TODO: make these constraints count (incorporate them into implicit search in `applyImplicitArgs`)
     * (`T` corresponds to @param sym)
     */
    def registerTypeSelection(sym: Symbol, tp: Type): Boolean = {
      registerBound(HasTypeMember(sym.name.toTypeName, tp), false)
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
      if (constr.instValid) constr.inst
      // get here when checking higher-order subtyping of the typevar by itself
      // TODO: check whether this ever happens?
      else if (isHigherKinded) typeFun(params, applyArgs(params map (_.typeConstructor)))
      else super.normalize
    )
    override def typeSymbol = origin.typeSymbol
    override def isStable = origin.isStable
    override def isVolatile = origin.isVolatile

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
    private def levelString = if (settings.explaintypes.value) level else ""
    protected def typeVarString = originName
    override def safeToString = (
      if ((constr eq null) || (constr.inst eq null)) "TVar<" + originName + "=null>"
      else if (constr.inst ne NoType) "=?" + constr.inst
      else (if(untouchable) "!?" else "?") + levelString + originName
    )
    override def kind = "TypeVar"

    def cloneInternal = {
      // cloning a suspended type variable when it's suspended will cause the clone
      // to never be resumed with the current implementation
      assert(!suspended, this)
      TypeVar.trace("clone", originLocation)(
        TypeVar(origin, constr cloneInternal, typeArgs, params) // @M TODO: clone args/params?
      )
    }
  }

  /** A type carrying some annotations. Created by the typechecker
   *  when eliminating ''Annotated'' trees (see typedAnnotated).
   *
   *  @param annotations the list of annotations on the type
   *  @param underlying the type without the annotation
   *  @param selfsym a "self" symbol with type `underlying`;
   *    only available if -Yself-in-annots is turned on. Can be `NoSymbol`
   *    if it is not used.
   */
  case class AnnotatedType(override val annotations: List[AnnotationInfo],
                           override val underlying: Type,
                           override val selfsym: Symbol)
  extends RewrappingTypeProxy {

    assert(!annotations.isEmpty, "" + underlying)

    override protected def rewrap(tp: Type) = copy(underlying = tp)

    override def isTrivial: Boolean = isTrivial0
    private lazy val isTrivial0 = underlying.isTrivial && annotations.forall(_.isTrivial)

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

    /** Set the self symbol */
    override def withSelfsym(sym: Symbol) = copy(selfsym = sym)

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
      else AnnotatedType(annotations1, underlying1, selfsym)
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
  def annotatedType(annots: List[AnnotationInfo], underlying: Type, selfsym: Symbol = NoSymbol): Type =
    if (annots.isEmpty) underlying
    else AnnotatedType(annots, underlying, selfsym)

  object AnnotatedType extends AnnotatedTypeExtractor { }

  /** A class representing types with a name. When an application uses
   *  named arguments, the named argument types for calling isApplicable
   *  are represented as NamedType.
   */
  case class NamedType(name: Name, tp: Type) extends Type {
    override def safeToString: String = name.toString +": "+ tp
  }

  /** A De Bruijn index referring to a previous type argument. Only used
   *  as a serialization format.
   */
  case class DeBruijnIndex(level: Int, idx: Int, args: List[Type]) extends Type {
    override def safeToString: String = "De Bruijn index("+level+","+idx+")"
  }

  /** A binder defining data associated with De Bruijn indices. Only used
   *  as a serialization format.
   */
  case class DeBruijnBinder(pnames: List[Name], ptypes: List[Type], restpe: Type) extends Type {
    override def safeToString = {
      val kind = if (pnames.head.isTypeName) "poly" else "method"
      "De Bruijn "+kind+"("+(pnames mkString ",")+";"+(ptypes mkString ",")+";"+restpe+")"
    }
  }

  abstract case class ErasedValueType(sym: Symbol) extends Type {
    override def safeToString = sym.name+"$unboxed"
  }

  final class UniqueErasedValueType(sym: Symbol) extends ErasedValueType(sym) with UniqueType

  object ErasedValueType {
    def apply(sym: Symbol): Type = {
      assert(sym ne NoSymbol, "ErasedValueType cannot be NoSymbol")
      unique(new UniqueErasedValueType(sym))
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

  abstract class LazyPolyType(override val typeParams: List[Symbol]) extends LazyType {
    override def safeToString =
      (if (typeParams.isEmpty) "" else typeParamsString(this)) + super.safeToString
  }

  // def mkLazyType(tparams: Symbol*)(f: Symbol => Unit): LazyType = (
  //   if (tparams.isEmpty) new LazyType { override def complete(sym: Symbol) = f(sym) }
  //   else new LazyPolyType(tparams.toList) { override def complete(sym: Symbol) = f(sym) }
  // )

// Creators ---------------------------------------------------------------

  /** Rebind symbol `sym` to an overriding member in type `pre`. */
  private def rebind(pre: Type, sym: Symbol): Symbol = {
    if (!sym.isOverridableMember || sym.owner == pre.typeSymbol) sym
    else pre.nonPrivateMember(sym.name).suchThat(sym => sym.isType || sym.isStable) orElse sym
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
      ThisType(RootClass)
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
      if (parents.isEmpty) ObjectClass.tpe else parents.head
    else {
      val clazz = owner.newRefinementClass(pos) // TODO: why were we passing in NoPosition instead of pos?
      val result = RefinedType(parents, decls, clazz)
      clazz.setInfo(result)
      result
    }
  }

  /** The canonical creator for a refined type with an initially empty scope.
   *
   *  @param parents ...
   *  @param owner   ...
   *  @return        ...
   */
  def refinedType(parents: List[Type], owner: Symbol): Type =
    refinedType(parents, owner, newScope, owner.pos)

  def copyRefinedType(original: RefinedType, parents: List[Type], decls: Scope) =
    if ((parents eq original.parents) && (decls eq original.decls)) original
    else {
      val owner = if (original.typeSymbol == NoSymbol) NoSymbol else original.typeSymbol.owner
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
      case _: CompoundType if sym1.isClass =>
        // sharpen prefix so that it is maximal and still contains the class.
        pre.parents.reverse dropWhile (_.member(sym1.name) != sym1) match {
          case Nil         => pre
          case parent :: _ => parent
        }
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
  def appliedType(tycon: Type, args: List[Type]): Type =
    if (args.isEmpty) tycon //@M! `if (args.isEmpty) tycon' is crucial (otherwise we create new types in phases after typer and then they don't get adapted (??))
    else tycon match {
      case TypeRef(pre, sym @ (NothingClass|AnyClass), _) => copyTypeRef(tycon, pre, sym, Nil)   //@M drop type args to Any/Nothing
      case TypeRef(pre, sym, _)                           => copyTypeRef(tycon, pre, sym, args)
      case PolyType(tparams, restpe)                      => restpe.instantiateTypeParams(tparams, args)
      case ExistentialType(tparams, restpe)               => newExistentialType(tparams, appliedType(restpe, args))
      case st: SingletonType                              => appliedType(st.widen, args) // @M TODO: what to do? see bug1
      case RefinedType(parents, decls)                    => RefinedType(parents map (appliedType(_, args)), decls) // MO to AM: please check
      case TypeBounds(lo, hi)                             => TypeBounds(appliedType(lo, args), appliedType(hi, args))
      case tv@TypeVar(_, _)                               => tv.applyArgs(args)
      case AnnotatedType(annots, underlying, self)        => AnnotatedType(annots, appliedType(underlying, args), self)
      case ErrorType                                      => tycon
      case WildcardType                                   => tycon // needed for neg/t0226
      case _                                              => abort(debugString(tycon))
    }

  /** Very convenient. */
  def appliedType(tyconSym: Symbol, args: Type*): Type =
    appliedType(tyconSym.typeConstructor, args.toList)

  /** A creator for existential types where the type arguments,
   *  rather than being applied directly, are interpreted as the
   *  upper bounds of unknown types.  For instance if the type argument
   *  list given is List(AnyRefClass), the resulting type would be
   *  e.g. Set[_ <: AnyRef] rather than Set[AnyRef] .
   */
  def appliedTypeAsUpperBounds(tycon: Type, args: List[Type]): Type = {
    tycon match {
      case TypeRef(pre, sym, _) if sameLength(sym.typeParams, args) =>
        val eparams  = typeParamsToExistentials(sym)
        val bounds   = args map (TypeBounds upper _)
        foreach2(eparams, bounds)(_ setInfo _)

        newExistentialType(eparams, typeRef(pre, sym, eparams map (_.tpe)))
      case _ =>
        appliedType(tycon, args)
    }
  }

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
    def apply(tparams: List[Symbol], tpe: Type): Type = (
      if (tparams nonEmpty) typeFun(tparams, tpe)
      else tpe // it's okay to be forgiving here
    )
    def unapply(tpe: Type): Option[(List[Symbol], Type)] = tpe match {
      case PolyType(tparams, restpe) => Some((tparams, restpe))
      case _                         => Some((Nil, tpe))
    }
  }
  def genPolyType(params: List[Symbol], tpe: Type): Type = GenPolyType(params, tpe)

  @deprecated("use genPolyType(...) instead", "2.10.0")
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
   *  where `tpe1` is the result of extrapolating `tpe` wrt to `tparams`.
   *  Extrapolating means that type variables in `tparams` occurring
   *  in covariant positions are replaced by upper bounds, (minus any
   *  SingletonClass markers), type variables in `tparams` occurring in
   *  contravariant positions are replaced by upper bounds, provided the
   *  resulting type is legal wrt to stability, and does not contain any type
   *  variable in `tparams`.
   *
   *  The abstraction drops all type parameters that are not directly or
   *  indirectly referenced by type `tpe1`. If there are no remaining type
   *  parameters, simply returns result type `tpe`.
   */
  def existentialAbstraction(tparams: List[Symbol], tpe0: Type): Type =
    if (tparams.isEmpty) tpe0
    else {
      val tpe      = deAlias(tpe0)
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

  /** Remove any occurrences of type aliases from this type */
  object deAlias extends TypeMap {
    def apply(tp: Type): Type = mapOver {
      tp match {
        case TypeRef(pre, sym, args) if sym.isAliasType => tp.normalize
        case _ => tp
      }
    }
  }

  /** Remove any occurrence of type <singleton> from this type and its parents */
  object dropSingletonType extends TypeMap {
    def apply(tp: Type): Type = {
      tp match {
        case TypeRef(_, SingletonClass, _) =>
          AnyClass.tpe
        case tp1 @ RefinedType(parents, decls) =>
          var parents1 = parents filter (_.typeSymbol != SingletonClass)
          if (parents1.isEmpty) parents1 = List(AnyClass.tpe)
          if (parents1.tail.isEmpty && decls.isEmpty) mapOver(parents1.head)
          else mapOver(copyRefinedType(tp1, parents1, decls))
        case tp1 =>
          mapOver(tp1)
      }
    }
  }

  /** Substitutes the empty scope for any non-empty decls in the type. */
  object dropAllRefinements extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case rt @ RefinedType(parents, decls) if !decls.isEmpty =>
        mapOver(copyRefinedType(rt, parents, EmptyScope))
      case ClassInfoType(parents, decls, clazz) if !decls.isEmpty =>
        mapOver(ClassInfoType(parents, EmptyScope, clazz))
      case _ =>
        mapOver(tp)
    }
  }

  /** Type with all top-level occurrences of abstract types replaced by their bounds */
  def abstractTypesToBounds(tp: Type): Type = tp match { // @M don't normalize here (compiler loops on pos/bug1090.scala )
    case TypeRef(_, sym, _) if sym.isAbstractType =>
      abstractTypesToBounds(tp.bounds.hi)
    case TypeRef(_, sym, _) if sym.isAliasType =>
      abstractTypesToBounds(tp.normalize)
    case rtp @ RefinedType(parents, decls) =>
      copyRefinedType(rtp, parents mapConserve abstractTypesToBounds, decls)
    case AnnotatedType(_, underlying, _) =>
      abstractTypesToBounds(underlying)
    case _ =>
      tp
  }

  // Set to true for A* => Seq[A]
  //   (And it will only rewrite A* in method result types.)
  //   This is the pre-existing behavior.
  // Or false for Seq[A] => Seq[A]
  //   (It will rewrite A* everywhere but method parameters.)
  //   This is the specified behavior.
  protected def etaExpandKeepsStar = false

  object dropRepeatedParamType extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case MethodType(params, restpe) =>
        MethodType(params, apply(restpe))
      case PolyType(tparams, restpe) =>
        PolyType(tparams, apply(restpe))
      case TypeRef(_, RepeatedParamClass, arg :: Nil) =>
        seqType(arg)
      case _ =>
        if (etaExpandKeepsStar) tp else mapOver(tp)
    }
  }

  object toDeBruijn extends TypeMap {
    private var paramStack: List[List[Symbol]] = Nil
    def mkDebruijnBinder(params: List[Symbol], restpe: Type) = {
      paramStack = params :: paramStack
      try {
        DeBruijnBinder(params map (_.name), params map (p => this(p.info)), this(restpe))
      } finally paramStack = paramStack.tail
    }
    def apply(tp: Type): Type = tp match {
      case PolyType(tparams, restpe) =>
        mkDebruijnBinder(tparams, restpe)
      case MethodType(params, restpe) =>
        mkDebruijnBinder(params, restpe)
      case TypeRef(NoPrefix, sym, args) =>
        val level = paramStack indexWhere (_ contains sym)
        if (level < 0) mapOver(tp)
        else DeBruijnIndex(level, paramStack(level) indexOf sym, args mapConserve this)
      case _ =>
        mapOver(tp)
    }
  }

  def fromDeBruijn(owner: Symbol) = new TypeMap {
    private var paramStack: List[List[Symbol]] = Nil
    def apply(tp: Type): Type = tp match {
      case DeBruijnBinder(pnames, ptypes, restpe) =>
        val isType = pnames.head.isTypeName
        val newParams = for (name <- pnames) yield
          if (isType) owner.newTypeParameter(name.toTypeName)
          else owner.newValueParameter(name.toTermName)
        paramStack = newParams :: paramStack
        try {
          foreach2(newParams, ptypes)((p, t) => p setInfo this(t))
          val restpe1 = this(restpe)
          if (isType) PolyType(newParams, restpe1)
          else MethodType(newParams, restpe1)
        } finally paramStack = paramStack.tail
      case DeBruijnIndex(level, idx, args) =>
        TypeRef(NoPrefix, paramStack(level)(idx), args map this)
      case _ =>
        mapOver(tp)
    }
  }

// Hash consing --------------------------------------------------------------

  private val initialUniquesCapacity = 4096
  private var uniques: util.HashSet[Type] = _
  private var uniqueRunId = NoRunId

  protected def unique[T <: Type](tp: T): T = {
    incCounter(rawTypeCount)
    if (uniqueRunId != currentRunId) {
      uniques = util.HashSet[Type]("uniques", initialUniquesCapacity)
      uniqueRunId = currentRunId
    }
    (uniques findEntryOrUpdate tp).asInstanceOf[T]
  }

// Helper Classes ---------------------------------------------------------

  /** @PP: Unable to see why these apparently constant types should need vals
   *  in every TypeConstraint, I lifted them out.
   */
  private lazy val numericLoBound = IntClass.tpe
  private lazy val numericHiBound = intersectionType(List(ByteClass.tpe, CharClass.tpe), ScalaPackageClass)

  /** A class expressing upper and lower bounds constraints of type variables,
   * as well as their instantiations.
   */
  class TypeConstraint(lo0: List[Type], hi0: List[Type], numlo0: Type, numhi0: Type, avoidWidening0: Boolean = false) {
    def this(lo0: List[Type], hi0: List[Type]) = this(lo0, hi0, NoType, NoType)
    def this(bounds: TypeBounds) = this(List(bounds.lo), List(bounds.hi))
    def this() = this(List(), List())

    /*  Syncnote: Type constraints are assumed to be used from only one
     *  thread. They are not exposed in api.Types and are used only locally
     *  in operations that are exposed from types. Hence, no syncing of any
     *  variables should be ncessesary.
     */

    /** Guard these lists against AnyClass and NothingClass appearing,
     *  else loBounds.isEmpty will have different results for an empty
     *  constraint and one with Nothing as a lower bound.  [Actually
     *  guarding addLoBound/addHiBound somehow broke raw types so it
     *  only guards against being created with them.]
     */
    private var lobounds = lo0 filterNot (_.typeSymbolDirect eq NothingClass)
    private var hibounds = hi0 filterNot (_.typeSymbolDirect eq AnyClass)
    private var numlo = numlo0
    private var numhi = numhi0
    private var avoidWidening = avoidWidening0

    def loBounds: List[Type] = if (numlo == NoType) lobounds else numlo :: lobounds
    def hiBounds: List[Type] = if (numhi == NoType) hibounds else numhi :: hibounds
    def avoidWiden: Boolean = avoidWidening

    def addLoBound(tp: Type, isNumericBound: Boolean = false) {
      if (isNumericBound && isNumericValueType(tp)) {
        if (numlo == NoType || isNumericSubType(numlo, tp))
          numlo = tp
        else if (!isNumericSubType(tp, numlo))
          numlo = numericLoBound
      }
      else lobounds ::= tp
    }

    def checkWidening(tp: Type) {
      if(tp.isStable) avoidWidening = true
      else tp match {
        case HasTypeMember(_, _) => avoidWidening = true
        case _ =>
      }
    }

    def addHiBound(tp: Type, isNumericBound: Boolean = false) {
      checkWidening(tp)
      if (isNumericBound && isNumericValueType(tp)) {
        if (numhi == NoType || isNumericSubType(tp, numhi))
          numhi = tp
        else if (!isNumericSubType(numhi, tp))
          numhi = numericHiBound
      }
      else hibounds ::= tp
    }

    def isWithinBounds(tp: Type): Boolean =
      lobounds.forall(_ <:< tp) &&
      hibounds.forall(tp <:< _) &&
      (numlo == NoType || (numlo weak_<:< tp)) &&
      (numhi == NoType || (tp weak_<:< numhi))

    var inst: Type = NoType // @M reduce visibility?

    def instValid = (inst ne null) && (inst ne NoType)

    def cloneInternal = {
      val tc = new TypeConstraint(lobounds, hibounds, numlo, numhi, avoidWidening)
      tc.inst = inst
      tc
    }

    override def toString = {
      val boundsStr = {
        val lo    = loBounds filterNot (_.typeSymbolDirect eq NothingClass)
        val hi    = hiBounds filterNot (_.typeSymbolDirect eq AnyClass)
        val lostr = if (lo.isEmpty) Nil else List(lo.mkString(" >: (", ", ", ")"))
        val histr = if (hi.isEmpty) Nil else List(hi.mkString(" <: (", ", ", ")"))

        lostr ++ histr mkString ("[", " | ", "]")
      }
      if (inst eq NoType) boundsStr
      else boundsStr + " _= " + inst.safeToString
    }
  }

  class TypeUnwrapper(poly: Boolean, existential: Boolean, annotated: Boolean, nullary: Boolean) extends (Type => Type) {
    def apply(tp: Type): Type = tp match {
      case AnnotatedType(_, underlying, _) if annotated   => apply(underlying)
      case ExistentialType(_, underlying) if existential  => apply(underlying)
      case PolyType(_, underlying) if poly                => apply(underlying)
      case NullaryMethodType(underlying) if nullary       => apply(underlying)
      case tp                                             => tp
    }
  }
  class ClassUnwrapper(existential: Boolean) extends TypeUnwrapper(poly = true, existential, annotated = true, nullary = false) {
    override def apply(tp: Type) = super.apply(tp.normalize)
  }

  object        unwrapToClass extends ClassUnwrapper(existential = true) { }
  object  unwrapToStableClass extends ClassUnwrapper(existential = false) { }
  object   unwrapWrapperTypes extends  TypeUnwrapper(true, true, true, true) { }

  trait AnnotationFilter extends TypeMap {
    def keepAnnotation(annot: AnnotationInfo): Boolean

    override def mapOver(annot: AnnotationInfo) =
      if (keepAnnotation(annot)) super.mapOver(annot)
      else UnmappableAnnotation
  }

  trait KeepOnlyTypeConstraints extends AnnotationFilter {
    // filter keeps only type constraint annotations
    def keepAnnotation(annot: AnnotationInfo) = annot matches TypeConstraintClass
  }

  trait VariantTypeMap extends TypeMap {
    private[this] var _variance = 1

    override def variance = _variance
    def variance_=(x: Int) = _variance = x

    override protected def noChangeToSymbols(origSyms: List[Symbol]) = {
      origSyms forall { sym =>
        val v = variance
        if (sym.isAliasType) variance = 0
        val result = this(sym.info)
        variance = v
        result eq sym.info
      }
    }

    override protected def mapOverArgs(args: List[Type], tparams: List[Symbol]): List[Type] =
      map2Conserve(args, tparams) { (arg, tparam) =>
        val v = variance
        if (tparam.isContravariant) variance = -variance
        else if (!tparam.isCovariant) variance = 0
        val arg1 = this(arg)
        variance = v
        arg1
      }

    /** Map this function over given type */
    override def mapOver(tp: Type): Type = tp match {
      case MethodType(params, result) =>
        variance = -variance
        val params1 = mapOver(params)
        variance = -variance
        val result1 = this(result)
        if ((params1 eq params) && (result1 eq result)) tp
        else copyMethodType(tp, params1, result1.substSym(params, params1))
      case PolyType(tparams, result) =>
        variance = -variance
        val tparams1 = mapOver(tparams)
        variance = -variance
        var result1 = this(result)
        if ((tparams1 eq tparams) && (result1 eq result)) tp
        else PolyType(tparams1, result1.substSym(tparams, tparams1))
      case TypeBounds(lo, hi) =>
        variance = -variance
        val lo1 = this(lo)
        variance = -variance
        val hi1 = this(hi)
        if ((lo1 eq lo) && (hi1 eq hi)) tp
        else TypeBounds(lo1, hi1)
      case tr @ TypeRef(pre, sym, args) =>
        val pre1 = this(pre)
        val args1 =
          if (args.isEmpty)
            args
          else if (variance == 0) // fast & safe path: don't need to look at typeparams
            args mapConserve this
          else {
            val tparams = sym.typeParams
            if (tparams.isEmpty) args
            else mapOverArgs(args, tparams)
          }
        if ((pre1 eq pre) && (args1 eq args)) tp
        else copyTypeRef(tp, pre1, tr.coevolveSym(pre1), args1)
      case _ =>
        super.mapOver(tp)
    }
  }

  // todo. move these into scala.reflect.api

  /** A prototype for mapping a function over all possible types
   */
  abstract class TypeMap extends (Type => Type) {
    def apply(tp: Type): Type

    /** Mix in VariantTypeMap if you want variances to be significant.
     */
    def variance = 0

    /** Map this function over given type */
    def mapOver(tp: Type): Type = tp match {
      case tr @ TypeRef(pre, sym, args) =>
        val pre1 = this(pre)
        val args1 = args mapConserve this
        if ((pre1 eq pre) && (args1 eq args)) tp
        else copyTypeRef(tp, pre1, tr.coevolveSym(pre1), args1)
      case ThisType(_) => tp
      case SingleType(pre, sym) =>
        if (sym.isPackageClass) tp // short path
        else {
          val pre1 = this(pre)
          if (pre1 eq pre) tp
          else singleType(pre1, sym)
        }
      case MethodType(params, result) =>
        val params1 = mapOver(params)
        val result1 = this(result)
        if ((params1 eq params) && (result1 eq result)) tp
        else copyMethodType(tp, params1, result1.substSym(params, params1))
      case PolyType(tparams, result) =>
        val tparams1 = mapOver(tparams)
        var result1 = this(result)
        if ((tparams1 eq tparams) && (result1 eq result)) tp
        else PolyType(tparams1, result1.substSym(tparams, tparams1))
      case NullaryMethodType(result) =>
        val result1 = this(result)
        if (result1 eq result) tp
        else NullaryMethodType(result1)
      case ConstantType(_) => tp
      case SuperType(thistp, supertp) =>
        val thistp1 = this(thistp)
        val supertp1 = this(supertp)
        if ((thistp1 eq thistp) && (supertp1 eq supertp)) tp
        else SuperType(thistp1, supertp1)
      case TypeBounds(lo, hi) =>
        val lo1 = this(lo)
        val hi1 = this(hi)
        if ((lo1 eq lo) && (hi1 eq hi)) tp
        else TypeBounds(lo1, hi1)
      case BoundedWildcardType(bounds) =>
        val bounds1 = this(bounds)
        if (bounds1 eq bounds) tp
        else BoundedWildcardType(bounds1.asInstanceOf[TypeBounds])
      case rtp @ RefinedType(parents, decls) =>
        val parents1 = parents mapConserve this
        val decls1 = mapOver(decls)
        //if ((parents1 eq parents) && (decls1 eq decls)) tp
        //else refinementOfClass(tp.typeSymbol, parents1, decls1)
        copyRefinedType(rtp, parents1, decls1)
      case ExistentialType(tparams, result) =>
        val tparams1 = mapOver(tparams)
        var result1 = this(result)
        if ((tparams1 eq tparams) && (result1 eq result)) tp
        else newExistentialType(tparams1, result1.substSym(tparams, tparams1))
      case OverloadedType(pre, alts) =>
        val pre1 = if (pre.isInstanceOf[ClassInfoType]) pre else this(pre)
        if (pre1 eq pre) tp
        else OverloadedType(pre1, alts)
      case AntiPolyType(pre, args) =>
        val pre1 = this(pre)
        val args1 = args mapConserve (this)
        if ((pre1 eq pre) && (args1 eq args)) tp
        else AntiPolyType(pre1, args1)
      case tv@TypeVar(_, constr) =>
        if (constr.instValid) this(constr.inst)
        else tv.applyArgs(mapOverArgs(tv.typeArgs, tv.params))  //@M !args.isEmpty implies !typeParams.isEmpty
      case NotNullType(tp) =>
        val tp1 = this(tp)
        if (tp1 eq tp) tp
        else NotNullType(tp1)
      case AnnotatedType(annots, atp, selfsym) =>
        val annots1 = mapOverAnnotations(annots)
        val atp1 = this(atp)
        if ((annots1 eq annots) && (atp1 eq atp)) tp
        else if (annots1.isEmpty) atp1
        else AnnotatedType(annots1, atp1, selfsym)
      case DeBruijnIndex(shift, idx, args) =>
        val args1 = args mapConserve this
        if (args1 eq args) tp
        else DeBruijnIndex(shift, idx, args1)
/*
      case ErrorType => tp
      case WildcardType => tp
      case NoType => tp
      case NoPrefix => tp
      case ErasedSingleType(sym) => tp
*/
      case _ =>
        tp
        // throw new Error("mapOver inapplicable for " + tp);
    }

    protected def mapOverArgs(args: List[Type], tparams: List[Symbol]): List[Type] =
      args mapConserve this

    /** Called by mapOver to determine whether the original symbols can
     *  be returned, or whether they must be cloned.  Overridden in VariantTypeMap.
     */
    protected def noChangeToSymbols(origSyms: List[Symbol]) =
      origSyms forall (sym => sym.info eq this(sym.info))

    /** Map this function over given scope */
    def mapOver(scope: Scope): Scope = {
      val elems = scope.toList
      val elems1 = mapOver(elems)
      if (elems1 eq elems) scope
      else newScopeWith(elems1: _*)
    }

    /** Map this function over given list of symbols */
    def mapOver(origSyms: List[Symbol]): List[Symbol] = {
      // fast path in case nothing changes due to map
      if (noChangeToSymbols(origSyms)) origSyms
      // map is not the identity --> do cloning properly
      else cloneSymbolsAndModify(origSyms, TypeMap.this)
    }

    def mapOver(annot: AnnotationInfo): AnnotationInfo = {
      val AnnotationInfo(atp, args, assocs) = annot
      val atp1  = mapOver(atp)
      val args1 = mapOverAnnotArgs(args)
      // there is no need to rewrite assocs, as they are constants

      if ((args eq args1) && (atp eq atp1)) annot
      else if (args1.isEmpty && args.nonEmpty) UnmappableAnnotation  // some annotation arg was unmappable
      else AnnotationInfo(atp1, args1, assocs) setPos annot.pos
    }

    def mapOverAnnotations(annots: List[AnnotationInfo]): List[AnnotationInfo] = {
      val annots1 = annots mapConserve mapOver
      if (annots1 eq annots) annots
      else annots1 filterNot (_ eq UnmappableAnnotation)
    }

    /** Map over a set of annotation arguments.  If any
     *  of the arguments cannot be mapped, then return Nil.  */
    def mapOverAnnotArgs(args: List[Tree]): List[Tree] = {
      val args1 = args mapConserve mapOver
      if (args1 contains UnmappableTree) Nil
      else args1
    }

    def mapOver(tree: Tree): Tree =
      mapOver(tree, () => return UnmappableTree)

    /** Map a tree that is part of an annotation argument.
     *  If the tree cannot be mapped, then invoke giveup().
     *  The default is to transform the tree with
     *  TypeMapTransformer.
     */
    def mapOver(tree: Tree, giveup: ()=>Nothing): Tree =
      (new TypeMapTransformer).transform(tree)

    /** This transformer leaves the tree alone except to remap
     *  its types. */
    class TypeMapTransformer extends Transformer {
      override def transform(tree: Tree) = {
        val tree1 = super.transform(tree)
        val tpe1 = TypeMap.this(tree1.tpe)
        if ((tree eq tree1) && (tree.tpe eq tpe1))
          tree
        else
          tree1.shallowDuplicate.setType(tpe1)
      }
    }
  }

  abstract class TypeTraverser extends TypeMap {
    def traverse(tp: Type): Unit
    def apply(tp: Type): Type = { traverse(tp); tp }
  }

  abstract class TypeTraverserWithResult[T] extends TypeTraverser {
    def result: T
    def clear(): Unit
  }

  abstract class TypeCollector[T](initial: T) extends TypeTraverser {
    var result: T = _
    def collect(tp: Type) = {
      result = initial
      traverse(tp)
      result
    }
  }

  /** A collector that tests for existential types appearing at given variance in a type
   *  @PP: Commenting out due to not being used anywhere.
   */
  // class ContainsVariantExistentialCollector(v: Int) extends TypeCollector(false) with VariantTypeMap {
  //   variance = v
  //
  //   def traverse(tp: Type) = tp match {
  //     case ExistentialType(_, _) if (variance == v) => result = true
  //     case _ => mapOver(tp)
  //   }
  // }
  //
  // val containsCovariantExistentialCollector = new ContainsVariantExistentialCollector(1)
  // val containsContravariantExistentialCollector = new ContainsVariantExistentialCollector(-1)

  def typeParamsToExistentials(clazz: Symbol, tparams: List[Symbol]): List[Symbol] = {
    val eparams = mapWithIndex(tparams)((tparam, i) =>
      clazz.newExistential(newTypeName("?"+i), clazz.pos) setInfo tparam.info.bounds)

    eparams map (_ substInfo (tparams, eparams))
  }
  def typeParamsToExistentials(clazz: Symbol): List[Symbol] =
    typeParamsToExistentials(clazz, clazz.typeParams)

  //  note: it's important to write the two tests in this order,
  //  as only typeParams forces the classfile to be read. See #400
  private def isRawIfWithoutArgs(sym: Symbol) =
    sym.isClass && sym.typeParams.nonEmpty && sym.isJavaDefined

  def isRaw(sym: Symbol, args: List[Type]) =
    !phase.erasedTypes && isRawIfWithoutArgs(sym) && args.isEmpty

  /** Is type tp a ''raw type''? */
  def isRawType(tp: Type) = tp match {
    case TypeRef(_, sym, args) => isRaw(sym, args)
    case _ => false
  }

  /** The raw to existential map converts a ''raw type'' to an existential type.
   *  It is necessary because we might have read a raw type of a
   *  parameterized Java class from a class file. At the time we read the type
   *  the corresponding class file might still not be read, so we do not
   *  know what the type parameters of the type are. Therefore
   *  the conversion of raw types to existential types might not have taken place
   *  in ClassFileparser.sigToType (where it is usually done).
   */
  def rawToExistential = new TypeMap {
    private var expanded = immutable.Set[Symbol]()
    def apply(tp: Type): Type = tp match {
      case TypeRef(pre, sym, List()) if isRawIfWithoutArgs(sym) =>
        if (expanded contains sym) AnyRefClass.tpe
        else try {
          expanded += sym
          val eparams = mapOver(typeParamsToExistentials(sym))
          existentialAbstraction(eparams, typeRef(apply(pre), sym, eparams map (_.tpe)))
        } finally {
          expanded -= sym
        }
      case _ =>
        mapOver(tp)
    }
  }

  /** Used by existentialAbstraction.
   */
  class ExistentialExtrapolation(tparams: List[Symbol]) extends VariantTypeMap {
    private val occurCount = mutable.HashMap[Symbol, Int]()
    private def countOccs(tp: Type) = {
      tp foreach {
        case TypeRef(_, sym, _) =>
          if (tparams contains sym)
            occurCount(sym) += 1
        case _ => ()
      }
    }
    def extrapolate(tpe: Type): Type = {
      tparams foreach (t => occurCount(t) = 0)
      countOccs(tpe)
      for (tparam <- tparams)
        countOccs(tparam.info)

      apply(tpe)
    }

    def apply(tp: Type): Type = {
      val tp1 = mapOver(tp)
      if (variance == 0) tp1
      else tp1 match {
        case TypeRef(pre, sym, args) if tparams contains sym =>
          val repl = if (variance == 1) dropSingletonType(tp1.bounds.hi) else tp1.bounds.lo
          //println("eliminate "+sym+"/"+repl+"/"+occurCount(sym)+"/"+(tparams exists (repl.contains)))//DEBUG
          if (!repl.typeSymbol.isBottomClass && occurCount(sym) == 1 && !(tparams exists (repl.contains)))
            repl
          else tp1
        case _ =>
          tp1
      }
    }
    override def mapOver(tp: Type): Type = tp match {
      case SingleType(pre, sym) =>
        if (sym.isPackageClass) tp // short path
        else {
          val pre1 = this(pre)
          if ((pre1 eq pre) || !pre1.isStable) tp
          else singleType(pre1, sym)
        }
      case _ => super.mapOver(tp)
    }

    // Do not discard the types of existential ident's. The
    // symbol of the Ident itself cannot be listed in the
    // existential's parameters, so the resulting existential
    // type would be ill-formed.
    override def mapOver(tree: Tree) = tree match {
      case Ident(_) if tree.tpe.isStable => tree
      case _                             => super.mapOver(tree)
    }
  }

  def singletonBounds(hi: Type) = TypeBounds.upper(intersectionType(List(hi, SingletonClass.tpe)))

  /** A map to compute the asSeenFrom method  */
  class AsSeenFromMap(pre: Type, clazz: Symbol) extends TypeMap with KeepOnlyTypeConstraints {
    var capturedSkolems: List[Symbol] = List()
    var capturedParams: List[Symbol] = List()
    var capturedPre = emptySymMap

    override def mapOver(tree: Tree, giveup: ()=>Nothing): Tree = {
      object annotationArgRewriter extends TypeMapTransformer {
        /** Rewrite `This` trees in annotation argument trees */
        def rewriteThis(tree: Tree): Tree =
          tree match {
            case This(_)
            if (tree.symbol isNonBottomSubClass clazz) &&
               (pre.widen.typeSymbol isNonBottomSubClass tree.symbol) =>
              if (pre.isStable) { // XXX why is this in this method? pull it out and guard the call `annotationArgRewriter.transform(tree)`?
                val termSym = (
                  pre.typeSymbol.owner.newValue(pre.typeSymbol.name.toTermName, pre.typeSymbol.pos) // what symbol should really be used?
                    setInfo pre
                )
                gen.mkAttributedQualifier(pre, termSym)
              } else
                giveup()

            case tree => tree
          }

        override def transform(tree: Tree): Tree = {
          val tree1 = rewriteThis(super.transform(tree))
          tree1
        }
      }

      annotationArgRewriter.transform(tree)
    }

    def stabilize(pre: Type, clazz: Symbol): Type =
      capturedPre.getOrElse(clazz, {
          val qvar = clazz freshExistential ".type" setInfo singletonBounds(pre)
          capturedPre += (clazz -> qvar)
          capturedParams = qvar :: capturedParams
          qvar
      }).tpe

    /** Return `pre.baseType(clazz)`, or if that's `NoType` and `clazz` is a refinement, `pre` itself.
     *  See bug397.scala for an example where the second alternative is needed.
     *  The problem is that when forming the base type sequence of an abstract type,
     *  any refinements in the base type list might be regenerated, and thus acquire
     *  new class symbols. However, since refinements always have non-interesting prefixes
     *  it looks OK to me to just take the prefix directly. */
    def base(pre: Type, clazz: Symbol) = {
      val b = pre.baseType(clazz)
      if (b == NoType && clazz.isRefinementClass) pre
      else b
    }

    def apply(tp: Type): Type =
      if ((pre eq NoType) || (pre eq NoPrefix) || !clazz.isClass) tp
      else tp match {
        case ThisType(sym) =>
          def toPrefix(pre: Type, clazz: Symbol): Type =
            if ((pre eq NoType) || (pre eq NoPrefix) || !clazz.isClass) tp
            else if ((sym isNonBottomSubClass clazz) &&
                     (pre.widen.typeSymbol isNonBottomSubClass sym)) {
              val pre1 = pre match {
                case SuperType(thistp, _) => thistp
                case _ => pre
              }
              if (!(pre1.isStable ||
                    pre1.typeSymbol.isPackageClass ||
                    pre1.typeSymbol.isModuleClass && pre1.typeSymbol.isStatic)) {
                stabilize(pre1, sym)
              } else {
                pre1
              }
            } else {
              toPrefix(base(pre, clazz).prefix, clazz.owner)
            }
          toPrefix(pre, clazz)
        case SingleType(pre, sym) =>
          if (sym.isPackageClass) tp // short path
          else {
            val pre1 = this(pre)
            if (pre1 eq pre) tp
            else if (pre1.isStable) singleType(pre1, sym)
            else pre1.memberType(sym).resultType //todo: this should be rolled into existential abstraction
          }
        // AM: Martin, is this description accurate?
        // walk the owner chain of `clazz` (the original argument to asSeenFrom) until we find the type param's owner (while rewriting pre as we crawl up the owner chain)
        // once we're at the owner, extract the information that pre encodes about the type param,
        // by minimally subsuming pre to the type instance of the class that owns the type param,
        // the type we're looking for is the type instance's type argument at the position corresponding to the type parameter
        // optimisation: skip this type parameter if it's not owned by a class, as those params are not influenced by the prefix through which they are seen
        // (concretely: type params of anonymous type functions, which currently can only arise from normalising type aliases, are owned by the type alias of which they are the eta-expansion)
        // (skolems also aren't affected: they are ruled out by the isTypeParameter check)
        case TypeRef(prefix, sym, args) if (sym.isTypeParameter && sym.owner.isClass) =>
          def toInstance(pre: Type, clazz: Symbol): Type =
            if ((pre eq NoType) || (pre eq NoPrefix) || !clazz.isClass) mapOver(tp)
            //@M! see test pos/tcpoly_return_overriding.scala why mapOver is necessary
            else {
              def throwError = abort("" + tp + sym.locationString + " cannot be instantiated from " + pre.widen)

              val symclazz = sym.owner
              if (symclazz == clazz && !pre.isInstanceOf[TypeVar] && (pre.widen.typeSymbol isNonBottomSubClass symclazz)) {
                // have to deconst because it may be a Class[T].
                pre.baseType(symclazz).deconst match {
                  case TypeRef(_, basesym, baseargs) =>

                   def instParam(ps: List[Symbol], as: List[Type]): Type =
                      if (ps.isEmpty) {
                        if (forInteractive) {
                          val saved = settings.uniqid.value
                          try {
                            settings.uniqid.value = true
                            println("*** stale type parameter: " + tp + sym.locationString + " cannot be instantiated from " + pre.widen)
                            println("*** confused with params: " + sym + " in " + sym.owner + " not in " + ps + " of " + basesym)
                            println("*** stacktrace = ")
                            new Error().printStackTrace()
                          } finally settings.uniqid.value = saved
                          instParamRelaxed(basesym.typeParams, baseargs)
                        } else throwError
                      } else if (sym eq ps.head)
                        // @M! don't just replace the whole thing, might be followed by type application
                        appliedType(as.head, args mapConserve (this)) // @M: was as.head
                      else instParam(ps.tail, as.tail)

                    /** Relaxed version of instParams which matches on names not symbols.
                     *  This is a last fallback in interactive mode because races in calls
                     *  from the IDE to the compiler may in rare cases lead to symbols referring
                     *  to type parameters that are no longer current.
                     */
                    def instParamRelaxed(ps: List[Symbol], as: List[Type]): Type =
                      if (ps.isEmpty) throwError
                      else if (sym.name == ps.head.name)
                        // @M! don't just replace the whole thing, might be followed by type application
                        appliedType(as.head, args mapConserve (this)) // @M: was as.head
                      else instParamRelaxed(ps.tail, as.tail)

                    //Console.println("instantiating " + sym + " from " + basesym + " with " + basesym.typeParams + " and " + baseargs+", pre = "+pre+", symclazz = "+symclazz);//DEBUG
                    if (sameLength(basesym.typeParams, baseargs))
                      instParam(basesym.typeParams, baseargs)
                    else
                      if (symclazz.tpe.parents.exists(_.isErroneous))
                        ErrorType // don't be to overzealous with throwing exceptions, see #2641
                      else
                        throw new Error(
                          "something is wrong (wrong class file?): "+basesym+
                          " with type parameters "+
                          basesym.typeParams.map(_.name).mkString("[",",","]")+
                          " gets applied to arguments "+baseargs.mkString("[",",","]")+", phase = "+phase)
                  case ExistentialType(tparams, qtpe) =>
                    capturedSkolems = capturedSkolems union tparams
                    toInstance(qtpe, clazz)
                  case t =>
                    throwError
                }
              } else toInstance(base(pre, clazz).prefix, clazz.owner)
            }
          toInstance(pre, clazz)
        case _ =>
          mapOver(tp)
      }
  }

  /** A base class to compute all substitutions */
  abstract class SubstMap[T](from: List[Symbol], to: List[T]) extends TypeMap {
    assert(sameLength(from, to), "Unsound substitution from "+ from +" to "+ to)

    /** Are `sym` and `sym1` the same? Can be tuned by subclasses. */
    protected def matches(sym: Symbol, sym1: Symbol): Boolean = sym eq sym1

    /** Map target to type, can be tuned by subclasses */
    protected def toType(fromtp: Type, tp: T): Type

    protected def renameBoundSyms(tp: Type): Type = tp match {
      case MethodType(ps, restp) =>
        createFromClonedSymbols(ps, restp)((ps1, tp1) => copyMethodType(tp, ps1, renameBoundSyms(tp1)))
      case PolyType(bs, restp) =>
        createFromClonedSymbols(bs, restp)((ps1, tp1) => PolyType(ps1, renameBoundSyms(tp1)))
      case ExistentialType(bs, restp) =>
        createFromClonedSymbols(bs, restp)(newExistentialType)
      case _ =>
        tp
    }

    def apply(tp0: Type): Type = if (from.isEmpty) tp0 else {
      @tailrec def subst(tp: Type, sym: Symbol, from: List[Symbol], to: List[T]): Type =
        if (from.isEmpty) tp
        // else if (to.isEmpty) error("Unexpected substitution on '%s': from = %s but to == Nil".format(tp, from))
        else if (matches(from.head, sym)) toType(tp, to.head)
        else subst(tp, sym, from.tail, to.tail)

      val boundSyms = tp0.boundSyms
      val tp1 = if (boundSyms exists from.contains) renameBoundSyms(tp0) else tp0
      val tp = mapOver(tp1)

      tp match {
        // @M
        // 1) arguments must also be substituted (even when the "head" of the
        // applied type has already been substituted)
        // example: (subst RBound[RT] from [type RT,type RBound] to
        // [type RT&,type RBound&]) = RBound&[RT&]
        // 2) avoid loops (which occur because alpha-conversion is
        // not performed properly imo)
        // e.g. if in class Iterable[a] there is a new Iterable[(a,b)],
        // we must replace the a in Iterable[a] by (a,b)
        // (must not recurse --> loops)
        // 3) replacing m by List in m[Int] should yield List[Int], not just List
        case TypeRef(NoPrefix, sym, args) =>
          appliedType(subst(tp, sym, from, to), args) // if args.isEmpty, appliedType is the identity
        case SingleType(NoPrefix, sym) =>
          subst(tp, sym, from, to)
        case _ =>
          tp
      }
    }
  }

  /** A map to implement the `substSym` method. */
  class SubstSymMap(from: List[Symbol], to: List[Symbol]) extends SubstMap(from, to) {
    protected def toType(fromtp: Type, sym: Symbol) = fromtp match {
      case TypeRef(pre, _, args) => copyTypeRef(fromtp, pre, sym, args)
      case SingleType(pre, _) => singleType(pre, sym)
    }
    override def apply(tp: Type): Type = if (from.isEmpty) tp else {
      @tailrec def subst(sym: Symbol, from: List[Symbol], to: List[Symbol]): Symbol =
        if (from.isEmpty) sym
        // else if (to.isEmpty) error("Unexpected substitution on '%s': from = %s but to == Nil".format(sym, from))
        else if (matches(from.head, sym)) to.head
        else subst(sym, from.tail, to.tail)
      tp match {
        case TypeRef(pre, sym, args) if pre ne NoPrefix =>
          val newSym = subst(sym, from, to)
          // assert(newSym.typeParams.length == sym.typeParams.length, "typars mismatch in SubstSymMap: "+(sym, sym.typeParams, newSym, newSym.typeParams))
          mapOver(copyTypeRef(tp, pre, newSym, args)) // mapOver takes care of subst'ing in args
        case SingleType(pre, sym) if pre ne NoPrefix =>
          mapOver(singleType(pre, subst(sym, from, to)))
        case _ =>
          super.apply(tp)
      }
    }

    override def mapOver(tree: Tree, giveup: ()=>Nothing): Tree = {
      object trans extends TypeMapTransformer {

        def termMapsTo(sym: Symbol) = from indexOf sym match {
          case -1   => None
          case idx  => Some(to(idx))
        }

        override def transform(tree: Tree) =
          tree match {
            case tree@Ident(_) =>
              termMapsTo(tree.symbol) match {
                case Some(tosym) =>
                  if (tosym.info.bounds.hi.typeSymbol isSubClass SingletonClass) {
                    Ident(tosym.existentialToString)
                      .setSymbol(tosym)
                      .setPos(tosym.pos)
                      .setType(dropSingletonType(tosym.info.bounds.hi))
                  } else {
                    giveup()
                  }
                case none => super.transform(tree)
              }
            case tree => super.transform(tree)
          }
      }
      trans.transform(tree)
    }
  }

  /** A map to implement the `subst` method. */
  class SubstTypeMap(from: List[Symbol], to: List[Type])
  extends SubstMap(from, to) {
    protected def toType(fromtp: Type, tp: Type) = tp

    override def mapOver(tree: Tree, giveup: () => Nothing): Tree = {
      object trans extends TypeMapTransformer {
        override def transform(tree: Tree) = tree match {
          case Ident(name) =>
            from indexOf tree.symbol match {
              case -1   => super.transform(tree)
              case idx  =>
                val totpe = to(idx)
                if (totpe.isStable) tree.duplicate setType totpe
                else giveup()
            }
          case _ =>
            super.transform(tree)
        }
      }
      trans.transform(tree)
    }
  }

  /** A map to implement the `substThis` method. */
  class SubstThisMap(from: Symbol, to: Type) extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case ThisType(sym) if (sym == from) => to
      case _ => mapOver(tp)
    }
  }
  class SubstThisAndSymMap(fromThis: Symbol, toThis: Type, fromSyms: List[Symbol], toSyms: List[Symbol])
  extends SubstSymMap(fromSyms, toSyms) {
    override def apply(tp: Type): Type = tp match {
      case ThisType(sym) if sym == fromThis => apply(toThis)
      case _                                => super.apply(tp)
    }
  }

  class SubstWildcardMap(from: List[Symbol]) extends TypeMap {
    def apply(tp: Type): Type = try {
      tp match {
        case TypeRef(_, sym, _) if from contains sym =>
          BoundedWildcardType(sym.info.bounds)
        case _ =>
          mapOver(tp)
      }
    } catch {
      case ex: MalformedType =>
        WildcardType
    }
  }

// dependent method types
  object IsDependentCollector extends TypeCollector(false) {
    def traverse(tp: Type) {
      if(tp isImmediatelyDependent) result = true
      else if (!result) mapOver(tp)
    }
  }

  object ApproximateDependentMap extends TypeMap {
    def apply(tp: Type): Type =
      if(tp isImmediatelyDependent) WildcardType
      else mapOver(tp)
  }

  class InstantiateDependentMap(params: List[Symbol], actuals0: List[Type]) extends TypeMap with KeepOnlyTypeConstraints {
    private val actuals      = actuals0.toIndexedSeq
    private val existentials = new Array[Symbol](actuals.size)
    def existentialsNeeded: List[Symbol] = existentials.filter(_ ne null).toList

    private object StableArg {
      def unapply(param: Symbol) = Arg unapply param map actuals filter (tp =>
        tp.isStable && (tp.typeSymbol != NothingClass)
      )
    }
    private object Arg {
      def unapply(param: Symbol) = Some(params indexOf param) filter (_ >= 0)
    }

    def apply(tp: Type): Type = mapOver(tp) match {
      // unsound to replace args by unstable actual #3873
      case SingleType(NoPrefix, StableArg(arg)) => arg
      // (soundly) expand type alias selections on implicit arguments,
      // see depmet_implicit_oopsla* test cases -- typically, `param.isImplicit`
      case tp1 @ TypeRef(SingleType(NoPrefix, Arg(pid)), sym, targs) =>
        val arg = actuals(pid)
        val res = typeRef(arg, sym, targs)
        if (res.typeSymbolDirect.isAliasType) res.dealias else tp1
      // don't return the original `tp`, which may be different from `tp1`,
      // due to dropping annotations
      case tp1 => tp1
    }

    /* Return the type symbol for referencing a parameter inside the existential quantifier.
     * (Only needed if the actual is unstable.)
     */
    private def existentialFor(pid: Int) = {
      if (existentials(pid) eq null) {
        val param = params(pid)
        existentials(pid) = (
          param.owner.newExistential(newTypeName(param.name + ".type"), param.pos, param.flags)
            setInfo singletonBounds(actuals(pid))
        )
      }
      existentials(pid)
    }

    //AM propagate more info to annotations -- this seems a bit ad-hoc... (based on code by spoon)
    override def mapOver(arg: Tree, giveup: ()=>Nothing): Tree = {
      // TODO: this should be simplified; in the stable case, one can
      // probably just use an Ident to the tree.symbol.
      //
      // @PP: That leads to failure here, where stuff no longer has type
      // 'String @Annot("stuff")' but 'String @Annot(x)'.
      //
      //   def m(x: String): String @Annot(x) = x
      //   val stuff = m("stuff")
      //
      // (TODO cont.) Why an existential in the non-stable case?
      //
      // @PP: In the following:
      //
      //   def m = { val x = "three" ; val y: String @Annot(x) = x; y }
      //
      // m is typed as 'String @Annot(x) forSome { val x: String }'.
      //
      // Both examples are from run/constrained-types.scala.
      object treeTrans extends Transformer {
        override def transform(tree: Tree): Tree = tree.symbol match {
          case StableArg(actual) =>
            gen.mkAttributedQualifier(actual, tree.symbol)
          case Arg(pid) =>
            val sym = existentialFor(pid)
            Ident(sym) copyAttrs tree setType typeRef(NoPrefix, sym, Nil)
          case _ =>
            super.transform(tree)
        }
      }
      treeTrans transform arg
    }
  }

  object StripAnnotationsMap extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case AnnotatedType(_, atp, _) =>
        mapOver(atp)
      case tp =>
        mapOver(tp)
    }
  }

  /** A map to convert every occurrence of a wildcard type to a fresh
   *  type variable */
  object wildcardToTypeVarMap extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case WildcardType =>
        TypeVar(tp, new TypeConstraint)
      case BoundedWildcardType(bounds) =>
        TypeVar(tp, new TypeConstraint(bounds))
      case _ =>
        mapOver(tp)
    }
  }

  /** A map to convert every occurrence of a type variable to a wildcard type. */
  object typeVarToOriginMap extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case TypeVar(origin, _) => origin
      case _ => mapOver(tp)
    }
  }

  /** A map to implement the `contains` method. */
  class ContainsCollector(sym: Symbol) extends TypeCollector(false) {
    def traverse(tp: Type) {
      if (!result) {
        tp.normalize match {
          case TypeRef(_, sym1, _) if (sym == sym1) => result = true
          case SingleType(_, sym1) if (sym == sym1) => result = true
          case _ => mapOver(tp)
        }
      }
    }

    override def mapOver(arg: Tree) = {
      for (t <- arg) {
        traverse(t.tpe)
        if (t.symbol == sym)
          result = true
      }
      arg
    }
  }

  /** A map to implement the `contains` method. */
  class ContainsTypeCollector(t: Type) extends TypeCollector(false) {
    def traverse(tp: Type) {
      if (!result) {
        if (tp eq t) result = true
        else mapOver(tp)
      }
    }
    override def mapOver(arg: Tree) = {
      for (t <- arg)
        traverse(t.tpe)

      arg
    }
  }

  /** A map to implement the `filter` method. */
  class FilterTypeCollector(p: Type => Boolean) extends TypeCollector[List[Type]](Nil) {
    def withFilter(q: Type => Boolean) = new FilterTypeCollector(tp => p(tp) && q(tp))

    override def collect(tp: Type) = super.collect(tp).reverse

    def traverse(tp: Type) {
      if (p(tp)) result ::= tp
      mapOver(tp)
    }
  }

  /** A map to implement the `collect` method. */
  class CollectTypeCollector[T](pf: PartialFunction[Type, T]) extends TypeCollector[List[T]](Nil) {
    override def collect(tp: Type) = super.collect(tp).reverse

    def traverse(tp: Type) {
      if (pf.isDefinedAt(tp)) result ::= pf(tp)
      mapOver(tp)
    }
  }

  class ForEachTypeTraverser(f: Type => Unit) extends TypeTraverser {
    def traverse(tp: Type) {
      f(tp)
      mapOver(tp)
    }
  }

  /** A map to implement the `filter` method. */
  class FindTypeCollector(p: Type => Boolean) extends TypeCollector[Option[Type]](None) {
    def traverse(tp: Type) {
      if (result.isEmpty) {
        if (p(tp)) result = Some(tp)
        mapOver(tp)
      }
    }
  }

  /** A map to implement the `contains` method. */
  object ErroneousCollector extends TypeCollector(false) {
    def traverse(tp: Type) {
      if (!result) {
        result = tp.isError
        mapOver(tp)
      }
    }
  }

  /** The most deeply nested owner that contains all the symbols
   *  of thistype or prefixless typerefs/singletype occurrences in given type.
   */
  private def commonOwner(t: Type): Symbol = commonOwner(t :: Nil)

  /** The most deeply nested owner that contains all the symbols
   *  of thistype or prefixless typerefs/singletype occurrences in given list
   *  of types.
   */
  private def commonOwner(tps: List[Type]): Symbol = {
    if (tps.isEmpty) NoSymbol
    else {
      commonOwnerMap.clear()
      tps foreach (commonOwnerMap traverse _)
      if (commonOwnerMap.result ne null) commonOwnerMap.result else NoSymbol
    }
  }

  protected def commonOwnerMap: CommonOwnerMap = commonOwnerMapObj

  protected class CommonOwnerMap extends TypeTraverserWithResult[Symbol] {
    var result: Symbol = _

    def clear() { result = null }

    private def register(sym: Symbol) {
      // First considered type is the trivial result.
      if ((result eq null) || (sym eq NoSymbol))
        result = sym
      else
        while ((result ne NoSymbol) && (result ne sym) && !(sym isNestedIn result))
          result = result.owner
    }
    def traverse(tp: Type) = tp.normalize match {
      case ThisType(sym)                => register(sym)
      case TypeRef(NoPrefix, sym, args) => register(sym.owner) ; args foreach traverse
      case SingleType(NoPrefix, sym)    => register(sym.owner)
      case _                            => mapOver(tp)
    }
  }

  private lazy val commonOwnerMapObj = new CommonOwnerMap

  class MissingAliasControl extends ControlThrowable
  val missingAliasException = new MissingAliasControl
  class MissingTypeControl extends ControlThrowable

  object adaptToNewRunMap extends TypeMap {

    private def adaptToNewRun(pre: Type, sym: Symbol): Symbol = {
      if (phase.flatClasses || sym.isRootSymbol || (pre eq NoPrefix) || (pre eq NoType) || sym.isPackageClass)
        sym
      else if (sym.isModuleClass) {
        val sourceModule1 = adaptToNewRun(pre, sym.sourceModule)

        sourceModule1.moduleClass orElse sourceModule1.initialize.moduleClass orElse {
          val msg = "Cannot adapt module class; sym = %s, sourceModule = %s, sourceModule.moduleClass = %s => sourceModule1 = %s, sourceModule1.moduleClass = %s"
          debuglog(msg.format(sym, sym.sourceModule, sym.sourceModule.moduleClass, sourceModule1, sourceModule1.moduleClass))
          sym
        }
      }
      else {
        var rebind0 = pre.findMember(sym.name, BRIDGE, 0, true) orElse {
          if (sym.isAliasType) throw missingAliasException
          debugwarn(pre+"."+sym+" does no longer exist, phase = "+phase)
          throw new MissingTypeControl // For build manager and presentation compiler purposes
        }
        /** The two symbols have the same fully qualified name */
        def corresponds(sym1: Symbol, sym2: Symbol): Boolean =
          sym1.name == sym2.name && (sym1.isPackageClass || corresponds(sym1.owner, sym2.owner))
        if (!corresponds(sym.owner, rebind0.owner)) {
          debuglog("ADAPT1 pre = "+pre+", sym = "+sym.fullLocationString+", rebind = "+rebind0.fullLocationString)
          val bcs = pre.baseClasses.dropWhile(bc => !corresponds(bc, sym.owner));
          if (bcs.isEmpty)
            assert(pre.typeSymbol.isRefinementClass, pre) // if pre is a refinementclass it might be a structural type => OK to leave it in.
          else
            rebind0 = pre.baseType(bcs.head).member(sym.name)
          debuglog(
            "ADAPT2 pre = " + pre +
            ", bcs.head = " + bcs.head +
            ", sym = " + sym.fullLocationString +
            ", rebind = " + rebind0.fullLocationString
          )
        }
        rebind0.suchThat(sym => sym.isType || sym.isStable) orElse {
          debuglog("" + phase + " " +phase.flatClasses+sym.owner+sym.name+" "+sym.isType)
          throw new MalformedType(pre, sym.nameString)
        }
      }
    }
    def apply(tp: Type): Type = tp match {
      case ThisType(sym) =>
        try {
          val sym1 = adaptToNewRun(sym.owner.thisType, sym)
          if (sym1 == sym) tp else ThisType(sym1)
        } catch {
          case ex: MissingTypeControl =>
            tp
        }
      case SingleType(pre, sym) =>
        if (sym.isPackage) tp
        else {
          val pre1 = this(pre)
          try {
            val sym1 = adaptToNewRun(pre1, sym)
            if ((pre1 eq pre) && (sym1 eq sym)) tp
            else singleType(pre1, sym1)
          } catch {
            case _: MissingTypeControl =>
              tp
          }
        }
      case TypeRef(pre, sym, args) =>
        if (sym.isPackageClass) tp
        else {
          val pre1 = this(pre)
          val args1 = args mapConserve (this)
          try {
            val sym1 = adaptToNewRun(pre1, sym)
            if ((pre1 eq pre) && (sym1 eq sym) && (args1 eq args)/* && sym.isExternal*/) {
              tp
            } else if (sym1 == NoSymbol) {
              debugwarn("adapt fail: "+pre+" "+pre1+" "+sym)
              tp
            } else {
              copyTypeRef(tp, pre1, sym1, args1)
            }
          } catch {
            case ex: MissingAliasControl =>
              apply(tp.dealias)
            case _: MissingTypeControl =>
              tp
          }
        }
      case MethodType(params, restp) =>
        val restp1 = this(restp)
        if (restp1 eq restp) tp
        else copyMethodType(tp, params, restp1)
      case NullaryMethodType(restp) =>
        val restp1 = this(restp)
        if (restp1 eq restp) tp
        else NullaryMethodType(restp1)
      case PolyType(tparams, restp) =>
        val restp1 = this(restp)
        if (restp1 eq restp) tp
        else PolyType(tparams, restp1)

      // Lukas: we need to check (together) whether we should also include parameter types
      // of PolyType and MethodType in adaptToNewRun

      case ClassInfoType(parents, decls, clazz) =>
        if (clazz.isPackageClass) tp
        else {
          val parents1 = parents mapConserve (this)
          if (parents1 eq parents) tp
          else ClassInfoType(parents1, decls, clazz)
        }
      case RefinedType(parents, decls) =>
        val parents1 = parents mapConserve (this)
        if (parents1 eq parents) tp
        else refinedType(parents1, tp.typeSymbol.owner, decls, tp.typeSymbol.owner.pos)
      case SuperType(_, _) => mapOver(tp)
      case TypeBounds(_, _) => mapOver(tp)
      case TypeVar(_, _) => mapOver(tp)
      case AnnotatedType(_,_,_) => mapOver(tp)
      case NotNullType(_) => mapOver(tp)
      case ExistentialType(_, _) => mapOver(tp)
      case _ => tp
    }
  }

  class SubTypePair(val tp1: Type, val tp2: Type) {
    override def hashCode = tp1.hashCode * 41 + tp2.hashCode
    override def equals(other: Any) = other match {
      case stp: SubTypePair =>
        // suspend TypeVars in types compared by =:=,
        // since we don't want to mutate them simply to check whether a subtype test is pending
        // in addition to making subtyping "more correct" for type vars,
        // it should avoid the stackoverflow that's been plaguing us (https://groups.google.com/d/topic/scala-internals/2gHzNjtB4xA/discussion)
        // this method is only called when subtyping hits a recursion threshold (subsametypeRecursions >= LogPendingSubTypesThreshold)
        @inline def suspend(tp: Type) =
          if (tp.isGround) null else suspendTypeVarsInType(tp)
        @inline def revive(suspension: List[TypeVar]) =
          if (suspension ne null) suspension foreach (_.suspended = false)

        val suspensions = Array(tp1, stp.tp1, tp2, stp.tp2) map suspend

        val sameTypes = (tp1 =:= stp.tp1) && (tp2 =:= stp.tp2)

        suspensions foreach revive

        sameTypes
      case _ =>
        false
    }
    override def toString = tp1+" <:<? "+tp2
  }

// Helper Methods  -------------------------------------------------------------

  final val LubGlbMargin = 0

  /** The maximum allowable depth of lubs or glbs over types `ts`.
    * This is the maximum depth of all types in the base type sequences
    * of each of the types `ts`, plus LubGlbMargin.
    */
  def lubDepth(ts: List[Type]) = {
    var d = 0
    for (tp <- ts) d = math.max(d, tp.baseTypeSeqDepth)
    d + LubGlbMargin
  }

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
        assert(sym1 == sym2)
        pre1 =:= pre2 &&
        forall3(args1, args2, sym1.typeParams) { (arg1, arg2, tparam) =>
            //if (tparam.variance == 0 && !(arg1 =:= arg2)) Console.println("inconsistent: "+arg1+"!="+arg2)//DEBUG
          if (tparam.variance == 0) arg1 =:= arg2
          else if (arg1.isInstanceOf[TypeVar])
            // if left-hand argument is a typevar, make it compatible with variance
            // this is for more precise pattern matching
            // todo: work this in the spec of this method
            // also: think what happens if there are embedded typevars?
            if (tparam.variance < 0) arg1 <:< arg2 else arg2 <:< arg1
          else true
        }
      case (et: ExistentialType, _) =>
        et.withTypeVars(isConsistent(_, tp2))
      case (_, et: ExistentialType) =>
        et.withTypeVars(isConsistent(tp1, _))
    }

    def check(tp1: Type, tp2: Type) =
      if (tp1.typeSymbol.isClass && tp1.typeSymbol.hasFlag(FINAL))
        tp1 <:< tp2 || isNumericValueClass(tp1.typeSymbol) && isNumericValueClass(tp2.typeSymbol)
      else tp1.baseClasses forall (bc =>
        tp2.baseTypeIndex(bc) < 0 || isConsistent(tp1.baseType(bc), tp2.baseType(bc)))

    check(tp1, tp2)/* && check(tp2, tp1)*/ // need to investgate why this can't be made symmetric -- neg/gadts1 fails, and run/existials also.
  }

  /** Does a pattern of type `patType` need an outer test when executed against
   *  selector type `selType` in context defined by `currentOwner`?
   */
  def needsOuterTest(patType: Type, selType: Type, currentOwner: Symbol) = {
    def createDummyClone(pre: Type): Type = {
      val dummy = currentOwner.enclClass.newValue(nme.ANYNAME).setInfo(pre.widen)
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
    patType match {
      case TypeRef(pre, sym, args) =>
        val pre1 = maybeCreateDummyClone(pre, sym)
        (pre1 ne NoType) && isPopulated(copyTypeRef(patType, pre1, sym, args), selType)
      case _ =>
        false
    }
  }

  private var subsametypeRecursions: Int = 0

  private def isUnifiable(pre1: Type, pre2: Type) =
    (beginsWithTypeVarOrIsRefined(pre1) || beginsWithTypeVarOrIsRefined(pre2)) && (pre1 =:= pre2)

  /** Returns true iff we are past phase specialize,
   *  sym1 and sym2 are two existential skolems with equal names and bounds,
   *  and pre1 and pre2 are equal prefixes
   */
  private def isSameSpecializedSkolem(sym1: Symbol, sym2: Symbol, pre1: Type, pre2: Type) = {
    sym1.isExistentialSkolem && sym2.isExistentialSkolem &&
    sym1.name == sym2.name &&
    phase.specialized &&
    sym1.info =:= sym2.info &&
    pre1 =:= pre2
  }

  private def equalSymsAndPrefixes(sym1: Symbol, pre1: Type, sym2: Symbol, pre2: Type): Boolean =
    if (sym1 == sym2) sym1.hasPackageFlag || phase.erasedTypes || pre1 =:= pre2
    else (sym1.name == sym2.name) && isUnifiable(pre1, pre2)

  /** Do `tp1` and `tp2` denote equivalent types? */
  def isSameType(tp1: Type, tp2: Type): Boolean = try {
    incCounter(sametypeCount)
    subsametypeRecursions += 1
    undoLog undoUnless {
      isSameType1(tp1, tp2)
    }
  } finally {
    subsametypeRecursions -= 1
    // XXX AM TODO: figure out when it is safe and needed to clear the log -- the commented approach below is too eager (it breaks #3281, #3866)
    // it doesn't help to keep separate recursion counts for the three methods that now share it
    // if (subsametypeRecursions == 0) undoLog.clear()
  }

  def isDifferentType(tp1: Type, tp2: Type): Boolean = try {
    subsametypeRecursions += 1
    undoLog undo { // undo type constraints that arise from operations in this block
      !isSameType1(tp1, tp2)
    }
  } finally {
    subsametypeRecursions -= 1
    // XXX AM TODO: figure out when it is safe and needed to clear the log -- the commented approach below is too eager (it breaks #3281, #3866)
    // it doesn't help to keep separate recursion counts for the three methods that now share it
    // if (subsametypeRecursions == 0) undoLog.clear()
  }

  def isDifferentTypeConstructor(tp1: Type, tp2: Type): Boolean = tp1 match {
    case TypeRef(pre1, sym1, _) =>
      tp2 match {
        case TypeRef(pre2, sym2, _) => sym1 != sym2 || isDifferentType(pre1, pre2)
        case _ => true
      }
    case _ => true
  }

  def normalizePlus(tp: Type) =
    if (isRawType(tp)) rawToExistential(tp)
    else tp.normalize

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
/*
  private def isSameType0(tp1: Type, tp2: Type): Boolean = {
    if (tp1 eq tp2) return true
    ((tp1, tp2) match {
      case (ErrorType, _) => true
      case (WildcardType, _) => true
      case (_, ErrorType) => true
      case (_, WildcardType) => true

      case (NoType, _) => false
      case (NoPrefix, _) => tp2.typeSymbol.isPackageClass
      case (_, NoType) => false
      case (_, NoPrefix) => tp1.typeSymbol.isPackageClass

      case (ThisType(sym1), ThisType(sym2))
      if (sym1 == sym2) =>
        true
      case (SingleType(pre1, sym1), SingleType(pre2, sym2))
      if (equalSymsAndPrefixes(sym1, pre1, sym2, pre2)) =>
        true
/*
      case (SingleType(pre1, sym1), ThisType(sym2))
      if (sym1.isModule &&
          sym1.moduleClass == sym2 &&
          pre1 =:= sym2.owner.thisType) =>
        true
      case (ThisType(sym1), SingleType(pre2, sym2))
      if (sym2.isModule &&
          sym2.moduleClass == sym1 &&
          pre2 =:= sym1.owner.thisType) =>
        true
*/
      case (ConstantType(value1), ConstantType(value2)) =>
        value1 == value2
      case (TypeRef(pre1, sym1, args1), TypeRef(pre2, sym2, args2)) =>
        equalSymsAndPrefixes(sym1, pre1, sym2, pre2) &&
        ((tp1.isHigherKinded && tp2.isHigherKinded && tp1.normalize =:= tp2.normalize) ||
         isSameTypes(args1, args2))
         // @M! normalize reduces higher-kinded case to PolyType's
      case (RefinedType(parents1, ref1), RefinedType(parents2, ref2)) =>
        def isSubScope(s1: Scope, s2: Scope): Boolean = s2.toList.forall {
          sym2 =>
            var e1 = s1.lookupEntry(sym2.name)
            (e1 ne null) && {
              val substSym = sym2.info.substThis(sym2.owner, e1.sym.owner.thisType)
              var isEqual = false
              while (!isEqual && (e1 ne null)) {
                isEqual = e1.sym.info =:= substSym
                e1 = s1.lookupNextEntry(e1)
              }
              isEqual
            }
        }
        //Console.println("is same? " + tp1 + " " + tp2 + " " + tp1.typeSymbol.owner + " " + tp2.typeSymbol.owner)//DEBUG
        isSameTypes(parents1, parents2) && isSubScope(ref1, ref2) && isSubScope(ref2, ref1)
      case (MethodType(params1, res1), MethodType(params2, res2)) =>
        // new dependent types: probably fix this, use substSym as done for PolyType
        (isSameTypes(tp1.paramTypes, tp2.paramTypes) &&
         res1 =:= res2 &&
         tp1.isImplicit == tp2.isImplicit)
      case (PolyType(tparams1, res1), PolyType(tparams2, res2)) =>
        // assert((tparams1 map (_.typeParams.length)) == (tparams2 map (_.typeParams.length)))
        (tparams1.length == tparams2.length) && (tparams1 corresponds tparams2)(_.info =:= _.info.substSym(tparams2, tparams1)) && // @M looks like it might suffer from same problem as #2210
          res1 =:= res2.substSym(tparams2, tparams1)
      case (ExistentialType(tparams1, res1), ExistentialType(tparams2, res2)) =>
        (tparams1.length == tparams2.length) && (tparams1 corresponds tparams2)(_.info =:= _.info.substSym(tparams2, tparams1)) && // @M looks like it might suffer from same problem as #2210
          res1 =:= res2.substSym(tparams2, tparams1)
      case (TypeBounds(lo1, hi1), TypeBounds(lo2, hi2)) =>
        lo1 =:= lo2 && hi1 =:= hi2
      case (BoundedWildcardType(bounds), _) =>
        bounds containsType tp2
      case (_, BoundedWildcardType(bounds)) =>
        bounds containsType tp1
      case (tv @ TypeVar(_,_), tp) =>
        tv.registerTypeEquality(tp, true)
      case (tp, tv @ TypeVar(_,_)) =>
        tv.registerTypeEquality(tp, false)
      case (AnnotatedType(_,_,_), _) =>
        annotationsConform(tp1, tp2) && annotationsConform(tp2, tp1) && tp1.withoutAnnotations =:= tp2.withoutAnnotations
      case (_, AnnotatedType(_,_,_)) =>
        annotationsConform(tp1, tp2) && annotationsConform(tp2, tp1) && tp1.withoutAnnotations =:= tp2.withoutAnnotations
      case (_: SingletonType, _: SingletonType) =>
        var origin1 = tp1
        while (origin1.underlying.isInstanceOf[SingletonType]) {
          assert(origin1 ne origin1.underlying, origin1)
          origin1 = origin1.underlying
        }
        var origin2 = tp2
        while (origin2.underlying.isInstanceOf[SingletonType]) {
          assert(origin2 ne origin2.underlying, origin2)
          origin2 = origin2.underlying
        }
        ((origin1 ne tp1) || (origin2 ne tp2)) && (origin1 =:= origin2)
      case _ =>
        false
    }) || {
      val tp1n = normalizePlus(tp1)
      val tp2n = normalizePlus(tp2)
      ((tp1n ne tp1) || (tp2n ne tp2)) && isSameType(tp1n, tp2n)
    }
  }
*/
  private def isSameType1(tp1: Type, tp2: Type): Boolean = {
    if ((tp1 eq tp2) ||
        (tp1 eq ErrorType) || (tp1 eq WildcardType) ||
        (tp2 eq ErrorType) || (tp2 eq WildcardType))
      true
    else if ((tp1 eq NoType) || (tp2 eq NoType))
      false
    else if (tp1 eq NoPrefix)
      tp2.typeSymbol.isPackageClass
    else if (tp2 eq NoPrefix)
      tp1.typeSymbol.isPackageClass
    else {
      isSameType2(tp1, tp2) || {
        val tp1n = normalizePlus(tp1)
        val tp2n = normalizePlus(tp2)
        ((tp1n ne tp1) || (tp2n ne tp2)) && isSameType(tp1n, tp2n)
      }
    }
  }

  def isSameType2(tp1: Type, tp2: Type): Boolean = {
    tp1 match {
      case tr1: TypeRef =>
        tp2 match {
          case tr2: TypeRef =>
            return (equalSymsAndPrefixes(tr1.sym, tr1.pre, tr2.sym, tr2.pre) &&
              ((tp1.isHigherKinded && tp2.isHigherKinded && tp1.normalize =:= tp2.normalize) ||
               isSameTypes(tr1.args, tr2.args))) ||
               ((tr1.pre, tr2.pre) match {
                 case (tv @ TypeVar(_,_), _) => tv.registerTypeSelection(tr1.sym, tr2)
                 case (_, tv @ TypeVar(_,_)) => tv.registerTypeSelection(tr2.sym, tr1)
                 case _ => false
               })
          case _: SingleType =>
            return isSameType2(tp2, tp1)  // put singleton type on the left, caught below
          case _ =>
        }
      case tt1: ThisType =>
        tp2 match {
          case tt2: ThisType =>
            if (tt1.sym == tt2.sym) return true
          case _ =>
        }
      case st1: SingleType =>
        tp2 match {
          case st2: SingleType =>
            if (equalSymsAndPrefixes(st1.sym, st1.pre, st2.sym, st2.pre)) return true
          case TypeRef(pre2, sym2, Nil) =>
            if (sym2.isModuleClass && equalSymsAndPrefixes(st1.sym, st1.pre, sym2.sourceModule, pre2)) return true
          case _ =>
        }
      case ct1: ConstantType =>
        tp2 match {
          case ct2: ConstantType =>
            return (ct1.value == ct2.value)
          case _ =>
        }
      case rt1: RefinedType =>
        tp2 match {
          case rt2: RefinedType => //
            def isSubScope(s1: Scope, s2: Scope): Boolean = s2.toList.forall {
              sym2 =>
                var e1 = s1.lookupEntry(sym2.name)
                (e1 ne null) && {
                  val substSym = sym2.info.substThis(sym2.owner, e1.sym.owner)
                  var isEqual = false
                  while (!isEqual && (e1 ne null)) {
                    isEqual = e1.sym.info =:= substSym
                    e1 = s1.lookupNextEntry(e1)
                  }
                  isEqual
                }
            }
            //Console.println("is same? " + tp1 + " " + tp2 + " " + tp1.typeSymbol.owner + " " + tp2.typeSymbol.owner)//DEBUG
            return isSameTypes(rt1.parents, rt2.parents) && {
              val decls1 = rt1.decls
              val decls2 = rt2.decls
              isSubScope(decls1, decls2) && isSubScope(decls2, decls1)
            }
          case _ =>
        }
      case mt1: MethodType =>
        tp2 match {
          case mt2: MethodType =>
            return isSameTypes(mt1.paramTypes, mt2.paramTypes) &&
              mt1.resultType =:= mt2.resultType.substSym(mt2.params, mt1.params) &&
              mt1.isImplicit == mt2.isImplicit
          // note: no case NullaryMethodType(restpe) => return mt1.params.isEmpty && mt1.resultType =:= restpe
          case _ =>
        }
      case NullaryMethodType(restpe1) =>
        tp2 match {
          // note: no case mt2: MethodType => return mt2.params.isEmpty && restpe  =:= mt2.resultType
          case NullaryMethodType(restpe2) =>
            return restpe1 =:= restpe2
          case _ =>
        }
      case PolyType(tparams1, res1) =>
        tp2 match {
          case PolyType(tparams2, res2) =>
//            assert((tparams1 map (_.typeParams.length)) == (tparams2 map (_.typeParams.length)))
              // @M looks like it might suffer from same problem as #2210
              return (
                (sameLength(tparams1, tparams2)) && // corresponds does not check length of two sequences before checking the predicate
                (tparams1 corresponds tparams2)(_.info =:= _.info.substSym(tparams2, tparams1)) &&
                res1 =:= res2.substSym(tparams2, tparams1)
              )
          case _ =>
        }
      case ExistentialType(tparams1, res1) =>
        tp2 match {
          case ExistentialType(tparams2, res2) =>
            // @M looks like it might suffer from same problem as #2210
            return (
              // corresponds does not check length of two sequences before checking the predicate -- faster & needed to avoid crasher in #2956
              sameLength(tparams1, tparams2) &&
              (tparams1 corresponds tparams2)(_.info =:= _.info.substSym(tparams2, tparams1)) &&
              res1 =:= res2.substSym(tparams2, tparams1)
            )
          case _ =>
        }
      case TypeBounds(lo1, hi1) =>
        tp2 match {
          case TypeBounds(lo2, hi2) =>
            return lo1 =:= lo2 && hi1 =:= hi2
          case _ =>
        }
      case BoundedWildcardType(bounds) =>
        return bounds containsType tp2
      case _ =>
    }
    tp2 match {
      case BoundedWildcardType(bounds) =>
        return bounds containsType tp1
      case _ =>
    }
    tp1 match {
      case tv @ TypeVar(_,_) =>
        return tv.registerTypeEquality(tp2, true)
      case _ =>
    }
    tp2 match {
      case tv @ TypeVar(_,_) =>
        return tv.registerTypeEquality(tp1, false)
      case _ =>
    }
    tp1 match {
      case _: AnnotatedType =>
        return annotationsConform(tp1, tp2) && annotationsConform(tp2, tp1) && tp1.withoutAnnotations =:= tp2.withoutAnnotations
      case _ =>
    }
    tp2 match {
      case _: AnnotatedType =>
        return annotationsConform(tp1, tp2) && annotationsConform(tp2, tp1) && tp1.withoutAnnotations =:= tp2.withoutAnnotations
      case _ =>
    }
    tp1 match {
      case _: SingletonType =>
        tp2 match {
          case _: SingletonType =>
            @inline def chaseDealiasedUnderlying(tp: Type): Type = {
              var origin = tp
              var next = origin.underlying.dealias
              while (next.isInstanceOf[SingletonType]) {
                assert(origin ne next, origin)
                origin = next
                next = origin.underlying.dealias
              }
              origin
            }
            val origin1 = chaseDealiasedUnderlying(tp1)
            val origin2 = chaseDealiasedUnderlying(tp2)
            ((origin1 ne tp1) || (origin2 ne tp2)) && (origin1 =:= origin2)
          case _ =>
            false
        }
      case _ =>
        false
    }
  }

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

  private val pendingSubTypes = new mutable.HashSet[SubTypePair]
  private var basetypeRecursions: Int = 0
  private val pendingBaseTypes = new mutable.HashSet[Type]

  def isSubType(tp1: Type, tp2: Type): Boolean = isSubType(tp1, tp2, AnyDepth)

  def isSubType(tp1: Type, tp2: Type, depth: Int): Boolean = try {
    subsametypeRecursions += 1

    undoLog undoUnless { // if subtype test fails, it should not affect constraints on typevars
      if (subsametypeRecursions >= LogPendingSubTypesThreshold) {
        val p = new SubTypePair(tp1, tp2)
        if (pendingSubTypes(p))
          false
        else
          try {
            pendingSubTypes += p
            isSubType2(tp1, tp2, depth)
          } finally {
            pendingSubTypes -= p
          }
      } else {
        isSubType2(tp1, tp2, depth)
      }
    }
  } finally {
    subsametypeRecursions -= 1
    // XXX AM TODO: figure out when it is safe and needed to clear the log -- the commented approach below is too eager (it breaks #3281, #3866)
    // it doesn't help to keep separate recursion counts for the three methods that now share it
    // if (subsametypeRecursions == 0) undoLog.clear()
  }

  /** Does this type have a prefix that begins with a type variable,
   *  or is it a refinement type? For type prefixes that fulfil this condition,
   *  type selections with the same name of equal (wrt) =:= prefixes are
   *  considered equal wrt =:=
   */
  def beginsWithTypeVarOrIsRefined(tp: Type): Boolean = tp match {
    case SingleType(pre, sym) =>
      !(sym hasFlag PACKAGE) && beginsWithTypeVarOrIsRefined(pre)
    case tv@TypeVar(_, constr) =>
      !tv.instValid || beginsWithTypeVarOrIsRefined(constr.inst)
    case RefinedType(_, _) =>
      true
    case _ =>
      false
  }

  def instTypeVar(tp: Type): Type = tp match {
    case TypeRef(pre, sym, args) =>
      copyTypeRef(tp, instTypeVar(pre), sym, args)
    case SingleType(pre, sym) =>
      singleType(instTypeVar(pre), sym)
    case TypeVar(_, constr) =>
      instTypeVar(constr.inst)
    case _ =>
      tp
  }

  def isErrorOrWildcard(tp: Type) = (tp eq ErrorType) || (tp eq WildcardType)

  def isSingleType(tp: Type) = tp match {
    case ThisType(_) | SuperType(_, _) | SingleType(_, _) => true
    case _ => false
  }

  def isConstantType(tp: Type) = tp match {
    case ConstantType(_) => true
    case _ => false
  }

  // @assume tp1.isHigherKinded || tp2.isHigherKinded
  def isHKSubType0(tp1: Type, tp2: Type, depth: Int): Boolean = (
    tp1.typeSymbol == NothingClass
    ||
    tp2.typeSymbol == AnyClass // @M Any and Nothing are super-type resp. subtype of every well-kinded type
    || // @M! normalize reduces higher-kinded case to PolyType's
    ((tp1.normalize.withoutAnnotations , tp2.normalize.withoutAnnotations) match {
      case (PolyType(tparams1, res1), PolyType(tparams2, res2)) => // @assume tp1.isHigherKinded && tp2.isHigherKinded (as they were both normalized to PolyType)
        sameLength(tparams1, tparams2) && {
          if (tparams1.head.owner.isMethod) {  // fast-path: polymorphic method type -- type params cannot be captured
            (tparams1 corresponds tparams2)((p1, p2) => p2.info.substSym(tparams2, tparams1) <:< p1.info) &&
            res1 <:< res2.substSym(tparams2, tparams1)
          } else { // normalized higher-kinded type
            //@M for an example of why we need to generate fresh symbols, see neg/tcpoly_ticket2101.scala
            val tpsFresh = cloneSymbols(tparams1)

            (tparams1 corresponds tparams2)((p1, p2) =>
              p2.info.substSym(tparams2, tpsFresh) <:< p1.info.substSym(tparams1, tpsFresh)) &&
            res1.substSym(tparams1, tpsFresh) <:< res2.substSym(tparams2, tpsFresh)

            //@M the forall in the previous test could be optimised to the following,
            // but not worth the extra complexity since it only shaves 1s from quick.comp
            //   (List.forall2(tpsFresh/*optimisation*/, tparams2)((p1, p2) =>
            //   p2.info.substSym(tparams2, tpsFresh) <:< p1.info /*optimisation, == (p1 from tparams1).info.substSym(tparams1, tpsFresh)*/) &&
            // this optimisation holds because inlining cloneSymbols in `val tpsFresh = cloneSymbols(tparams1)` gives:
            // val tpsFresh = tparams1 map (_.cloneSymbol)
            // for (tpFresh <- tpsFresh) tpFresh.setInfo(tpFresh.info.substSym(tparams1, tpsFresh))
        }
      } && annotationsConform(tp1.normalize, tp2.normalize)
      case (_, _) => false // @assume !tp1.isHigherKinded || !tp2.isHigherKinded
      // --> thus, cannot be subtypes (Any/Nothing has already been checked)
    }))

  def isSubArg(t1: Type, t2: Type, variance: Int) =
    (variance > 0 || t2 <:< t1) && (variance < 0 || t1 <:< t2)

  def isSubArgs(tps1: List[Type], tps2: List[Type], tparams: List[Symbol]): Boolean =
    corresponds3(tps1, tps2, tparams map (_.variance))(isSubArg)

  def differentOrNone(tp1: Type, tp2: Type) = if (tp1 eq tp2) NoType else tp1

  /** Does type `tp1` conform to `tp2`? */
  private def isSubType2(tp1: Type, tp2: Type, depth: Int): Boolean = {
    if ((tp1 eq tp2) || isErrorOrWildcard(tp1) || isErrorOrWildcard(tp2)) return true
    if ((tp1 eq NoType) || (tp2 eq NoType)) return false
    if (tp1 eq NoPrefix) return (tp2 eq NoPrefix) || tp2.typeSymbol.isPackageClass
    if (tp2 eq NoPrefix) return tp1.typeSymbol.isPackageClass
    if (isSingleType(tp1) && isSingleType(tp2) || isConstantType(tp1) && isConstantType(tp2)) return tp1 =:= tp2
    if (tp1.isHigherKinded || tp2.isHigherKinded) return isHKSubType0(tp1, tp2, depth)

    /** First try, on the right:
     *   - unwrap Annotated types, BoundedWildcardTypes,
     *   - bind TypeVars  on the right, if lhs is not Annotated nor BoundedWildcard
     *   - handle common cases for first-kind TypeRefs on both sides as a fast path.
     */
    def firstTry = tp2 match {
      // fast path: two typerefs, none of them HK
      case tr2: TypeRef =>
        tp1 match {
          case tr1: TypeRef =>
            val sym1 = tr1.sym
            val sym2 = tr2.sym
            val pre1 = tr1.pre
            val pre2 = tr2.pre
            (((if (sym1 == sym2) phase.erasedTypes || pre1 <:< pre2
               else (sym1.name == sym2.name && !sym1.isModuleClass && !sym2.isModuleClass &&
                     (isUnifiable(pre1, pre2) || isSameSpecializedSkolem(sym1, sym2, pre1, pre2)))) &&
                    isSubArgs(tr1.args, tr2.args, sym1.typeParams))
             ||
             sym2.isClass && {
               val base = tr1 baseType sym2
               (base ne tr1) && base <:< tr2
             }
             ||
             thirdTryRef(tr1, tr2))
          case _ =>
            secondTry
        }
      case AnnotatedType(_, _, _) =>
        tp1.withoutAnnotations <:< tp2.withoutAnnotations && annotationsConform(tp1, tp2)
      case BoundedWildcardType(bounds) =>
        tp1 <:< bounds.hi
      case tv2 @ TypeVar(_, constr2) =>
        tp1 match {
          case AnnotatedType(_, _, _) | BoundedWildcardType(_) =>
            secondTry
          case _ =>
            tv2.registerBound(tp1, true)
        }
      case _ =>
        secondTry
    }

    /** Second try, on the left:
     *   - unwrap AnnotatedTypes, BoundedWildcardTypes,
     *   - bind typevars,
     *   - handle existential types by skolemization.
     */
    def secondTry = tp1 match {
      case AnnotatedType(_, _, _) =>
        tp1.withoutAnnotations <:< tp2.withoutAnnotations && annotationsConform(tp1, tp2)
      case BoundedWildcardType(bounds) =>
        tp1.bounds.lo <:< tp2
      case tv @ TypeVar(_,_) =>
        tv.registerBound(tp2, false)
      case ExistentialType(_, _) =>
        try {
          skolemizationLevel += 1
          tp1.skolemizeExistential <:< tp2
        } finally {
          skolemizationLevel -= 1
        }
      case _ =>
        thirdTry
    }

    def thirdTryRef(tp1: Type, tp2: TypeRef): Boolean = {
      val sym2 = tp2.sym
      sym2 match {
        case NotNullClass => tp1.isNotNull
        case SingletonClass => tp1.isStable || fourthTry
        case _: ClassSymbol =>
          if (isRaw(sym2, tp2.args))
            isSubType(tp1, rawToExistential(tp2), depth)
          else if (sym2.name == tpnme.REFINE_CLASS_NAME)
            isSubType(tp1, sym2.info, depth)
          else
            fourthTry
        case _: TypeSymbol =>
          if (sym2 hasFlag DEFERRED) {
            val tp2a = tp2.bounds.lo
            isDifferentTypeConstructor(tp2, tp2a) && tp1 <:< tp2a || fourthTry
          } else {
            isSubType(tp1.normalize, tp2.normalize, depth)
          }
        case _ =>
          fourthTry
      }
    }

    /** Third try, on the right:
     *   - decompose refined types.
     *   - handle typerefs, existentials, and notnull types.
     *   - handle left+right method types, polytypes, typebounds
     */
    def thirdTry = tp2 match {
      case tr2: TypeRef =>
        thirdTryRef(tp1, tr2)
      case rt2: RefinedType =>
        (rt2.parents forall (tp1 <:< _)) &&
        (rt2.decls forall tp1.specializes)
      case et2: ExistentialType =>
        et2.withTypeVars(tp1 <:< _, depth) || fourthTry
      case nn2: NotNullType =>
        tp1.isNotNull && tp1 <:< nn2.underlying
      case mt2: MethodType =>
        tp1 match {
          case mt1 @ MethodType(params1, res1) =>
            val params2 = mt2.params
            val res2 = mt2.resultType
            (sameLength(params1, params2) &&
             mt1.isImplicit == mt2.isImplicit &&
             matchingParams(params1, params2, mt1.isJava, mt2.isJava) &&
             (res1 <:< res2.substSym(params2, params1)))
          // TODO: if mt1.params.isEmpty, consider NullaryMethodType?
          case _ =>
            false
        }
      case pt2 @ NullaryMethodType(_) =>
        tp1 match {
          // TODO: consider MethodType mt for which mt.params.isEmpty??
          case pt1 @ NullaryMethodType(_) =>
            pt1.resultType <:< pt2.resultType
          case _ =>
            false
        }
      case TypeBounds(lo2, hi2) =>
        tp1 match {
          case TypeBounds(lo1, hi1) =>
            lo2 <:< lo1 && hi1 <:< hi2
          case _ =>
            false
        }
      case _ =>
        fourthTry
    }

    /** Fourth try, on the left:
     *   - handle typerefs, refined types, notnull and singleton types.
     */
    def fourthTry = tp1 match {
      case tr1 @ TypeRef(pre1, sym1, _) =>
        sym1 match {
          case NothingClass => true
          case NullClass =>
            tp2 match {
              case TypeRef(_, sym2, _) =>
                containsNull(sym2)
              case _ =>
                isSingleType(tp2) && tp1 <:< tp2.widen
            }
          case _: ClassSymbol =>
            if (isRaw(sym1, tr1.args))
              isSubType(rawToExistential(tp1), tp2, depth)
            else if (sym1.isModuleClass) tp2 match {
              case SingleType(pre2, sym2) => equalSymsAndPrefixes(sym1.sourceModule, pre1, sym2, pre2)
              case _                      => false
            }
            else if (sym1.isRefinementClass)
              isSubType(sym1.info, tp2, depth)
            else false

          case _: TypeSymbol =>
            if (sym1 hasFlag DEFERRED) {
              val tp1a = tp1.bounds.hi
              isDifferentTypeConstructor(tp1, tp1a) && tp1a <:< tp2
            } else {
              isSubType(tp1.normalize, tp2.normalize, depth)
            }
          case _ =>
            false
        }
      case RefinedType(parents1, _) =>
        parents1 exists (_ <:< tp2)
      case _: SingletonType | _: NotNullType =>
        tp1.underlying <:< tp2
      case _ =>
        false
    }

    firstTry
  }

  private def containsNull(sym: Symbol): Boolean =
    sym.isClass && sym != NothingClass &&
    !(sym isNonBottomSubClass AnyValClass) &&
    !(sym isNonBottomSubClass NotNullClass)

  /** Are `tps1` and `tps2` lists of equal length such that all elements
   *  of `tps1` conform to corresponding elements of `tps2`?
   */
  def isSubTypes(tps1: List[Type], tps2: List[Type]): Boolean = (tps1 corresponds tps2)(_ <:< _)

  /** Does type `tp` implement symbol `sym` with same or
   *  stronger type? Exact only if `sym` is a member of some
   *  refinement type, otherwise we might return false negatives.
   */
  def specializesSym(tp: Type, sym: Symbol): Boolean =
    tp.typeSymbol == NothingClass ||
    tp.typeSymbol == NullClass && containsNull(sym.owner) ||
    (tp.nonPrivateMember(sym.name).alternatives exists
      (alt => sym == alt || specializesSym(tp.narrow, alt, sym.owner.thisType, sym)))

  /** Does member `sym1` of `tp1` have a stronger type
   *  than member `sym2` of `tp2`?
   */
  private def specializesSym(tp1: Type, sym1: Symbol, tp2: Type, sym2: Symbol): Boolean = {
    val info1 = tp1.memberInfo(sym1)
    val info2 = tp2.memberInfo(sym2).substThis(tp2.typeSymbol, tp1)
    //System.out.println("specializes "+tp1+"."+sym1+":"+info1+sym1.locationString+" AND "+tp2+"."+sym2+":"+info2)//DEBUG
    (    sym2.isTerm && (info1 <:< info2) && (!sym2.isStable || sym1.isStable)
      || sym2.isAbstractType && {
            val memberTp1 = tp1.memberType(sym1)
            // println("kinds conform? "+(memberTp1, tp1, sym2, kindsConform(List(sym2), List(memberTp1), tp2, sym2.owner)))
            info2.bounds.containsType(memberTp1) &&
            kindsConform(List(sym2), List(memberTp1), tp1, sym1.owner)
        }
      || sym2.isAliasType && tp2.memberType(sym2).substThis(tp2.typeSymbol, tp1) =:= tp1.memberType(sym1) //@MAT ok
    )
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
          matchesType(tp1, res2, true)
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
            alwaysMatchSimple && matchesType(tp1, res2, true)
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
            alwaysMatchSimple && matchesType(tp1, res2, true)
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
            alwaysMatchSimple && matchesType(tp1, res2, true)
          case _ =>
            false // remember that tparams1.nonEmpty is now an invariant of PolyType
        }
      case ExistentialType(tparams1, res1) =>
        tp2 match {
          case ExistentialType(tparams2, res2) =>
            matchesQuantified(tparams1, tparams2, res1, res2)
          case _ =>
            if (alwaysMatchSimple) matchesType(res1, tp2, true)
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
        params1.length == params2.length && // useful pre-secreening optimization
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
  private def matchingParams(syms1: List[Symbol], syms2: List[Symbol], syms1isJava: Boolean, syms2isJava: Boolean): Boolean = syms1 match {
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

  /** like map2, but returns list `xs` itself - instead of a copy - if function
   *  `f` maps all elements to themselves.
   */
  def map2Conserve[A <: AnyRef, B](xs: List[A], ys: List[B])(f: (A, B) => A): List[A] =
    if (xs.isEmpty) xs
    else {
      val x1 = f(xs.head, ys.head)
      val xs1 = map2Conserve(xs.tail, ys.tail)(f)
      if ((x1 eq xs.head) && (xs1 eq xs.tail)) xs
      else x1 :: xs1
    }

  /** Solve constraint collected in types `tvars`.
   *
   *  @param tvars      All type variables to be instantiated.
   *  @param tparams    The type parameters corresponding to `tvars`
   *  @param variances  The variances of type parameters; need to reverse
   *                    solution direction for all contravariant variables.
   *  @param upper      When `true` search for max solution else min.
   */
  def solve(tvars: List[TypeVar], tparams: List[Symbol],
            variances: List[Int], upper: Boolean): Boolean =
     solve(tvars, tparams, variances, upper, AnyDepth)

  def solve(tvars: List[TypeVar], tparams: List[Symbol],
            variances: List[Int], upper: Boolean, depth: Int): Boolean = {

    def solveOne(tvar: TypeVar, tparam: Symbol, variance: Int) {
      if (tvar.constr.inst == NoType) {
        val up = if (variance != CONTRAVARIANT) upper else !upper
        tvar.constr.inst = null
        val bound: Type = if (up) tparam.info.bounds.hi else tparam.info.bounds.lo
        //Console.println("solveOne0(tv, tp, v, b)="+(tvar, tparam, variance, bound))
        var cyclic = bound contains tparam
        foreach3(tvars, tparams, variances)((tvar2, tparam2, variance2) => {
          val ok = (tparam2 != tparam) && (
               (bound contains tparam2)
            ||  up && (tparam2.info.bounds.lo =:= tparam.tpeHK)
            || !up && (tparam2.info.bounds.hi =:= tparam.tpeHK)
          )
          if (ok) {
            if (tvar2.constr.inst eq null) cyclic = true
            solveOne(tvar2, tparam2, variance2)
          }
        })
        if (!cyclic) {
          if (up) {
            if (bound.typeSymbol != AnyClass)
              tvar addHiBound bound.instantiateTypeParams(tparams, tvars)
            for (tparam2 <- tparams)
              tparam2.info.bounds.lo.dealias match {
                case TypeRef(_, `tparam`, _) =>
                  tvar addHiBound tparam2.tpeHK.instantiateTypeParams(tparams, tvars)
                case _ =>
              }
          } else {
            if (bound.typeSymbol != NothingClass && bound.typeSymbol != tparam) {
              tvar addLoBound bound.instantiateTypeParams(tparams, tvars)
            }
            for (tparam2 <- tparams)
              tparam2.info.bounds.hi.dealias match {
                case TypeRef(_, `tparam`, _) =>
                  tvar addLoBound tparam2.tpeHK.instantiateTypeParams(tparams, tvars)
                case _ =>
              }
          }
        }
        tvar.constr.inst = NoType // necessary because hibounds/lobounds may contain tvar

        //println("solving "+tvar+" "+up+" "+(if (up) (tvar.constr.hiBounds) else tvar.constr.loBounds)+((if (up) (tvar.constr.hiBounds) else tvar.constr.loBounds) map (_.widen)))

        tvar setInst (
          if (up) {
            if (depth != AnyDepth) glb(tvar.constr.hiBounds, depth) else glb(tvar.constr.hiBounds)
          } else {
            if (depth != AnyDepth) lub(tvar.constr.loBounds, depth) else lub(tvar.constr.loBounds)
          })

        //Console.println("solving "+tvar+" "+up+" "+(if (up) (tvar.constr.hiBounds) else tvar.constr.loBounds)+((if (up) (tvar.constr.hiBounds) else tvar.constr.loBounds) map (_.widen))+" = "+tvar.constr.inst)//@MDEBUG
      }
    }

    // println("solving "+tvars+"/"+tparams+"/"+(tparams map (_.info)))
    foreach3(tvars, tparams, variances)(solveOne)
    tvars forall (tvar => tvar.constr.isWithinBounds(tvar.constr.inst))
  }

  /** Do type arguments `targs` conform to formal parameters `tparams`?
   */
  def isWithinBounds(pre: Type, owner: Symbol, tparams: List[Symbol], targs: List[Type]): Boolean = {
    var bounds = instantiatedBounds(pre, owner, tparams, targs)
    if (targs.exists(_.annotations.nonEmpty))
      bounds = adaptBoundsToAnnotations(bounds, tparams, targs)
    (bounds corresponds targs)(_ containsType _)
  }

  def instantiatedBounds(pre: Type, owner: Symbol, tparams: List[Symbol], targs: List[Type]): List[TypeBounds] =
    tparams map (_.info.asSeenFrom(pre, owner).instantiateTypeParams(tparams, targs).bounds)

// Lubs and Glbs ---------------------------------------------------------

  private def printLubMatrix(btsMap: Map[Type, List[Type]], depth: Int) {
    import scala.tools.nsc.util.TableDef
    import TableDef.Column
    def str(tp: Type) = {
      if (tp == NoType) ""
      else {
        val s = ("" + tp).replaceAll("""[\w.]+\.(\w+)""", "$1")
        if (s.length < 60) s
        else (s take 57) + "..."
      }
    }

    val sorted       = btsMap.toList.sortWith((x, y) => x._1.typeSymbol isLess y._1.typeSymbol)
    val maxSeqLength = sorted map (_._2.size) max
    val padded       = sorted map (_._2.padTo(maxSeqLength, NoType))
    val transposed   = padded.transpose

    val columns: List[Column[List[Type]]] = mapWithIndex(sorted) {
      case ((k, v), idx) =>
        Column(str(k), (xs: List[Type]) => str(xs(idx)), true)
    }

    val tableDef = TableDef(columns: _*)
    val formatted = tableDef.table(transposed)
    println("** Depth is " + depth + "\n" + formatted)
  }

  /** From a list of types, find any which take type parameters
   *  where the type parameter bounds contain references to other
   *  any types in the list (including itself.)
   *
   *  @return List of symbol pairs holding the recursive type
   *    parameter and the parameter which references it.
   */
  def findRecursiveBounds(ts: List[Type]): List[(Symbol, Symbol)] = {
    if (ts.isEmpty) Nil
    else {
      val sym = ts.head.typeSymbol
      require(ts.tail forall (_.typeSymbol == sym), ts)
      for (p <- sym.typeParams ; in <- sym.typeParams ; if in.info.bounds contains p) yield
        p -> in
    }
  }

  /** Given a matrix `tsBts` whose columns are basetype sequences (and the symbols `tsParams` that should be interpreted as type parameters in this matrix),
   * compute its least sorted upwards closed upper bound relative to the following ordering <= between lists of types:
   *
   *    xs <= ys   iff   forall y in ys exists x in xs such that x <: y
   *
   *  @arg tsParams for each type in the original list of types `ts0`, its list of type parameters (if that type is a type constructor)
   *                (these type parameters may be referred to by type arguments in the BTS column of those types,
   *                and must be interpreted as bound variables; i.e., under a type lambda that wraps the types that refer to these type params)
   *  @arg tsBts    a matrix whose columns are basetype sequences
   *                the first row is the original list of types for which we're computing the lub
   *                  (except that type constructors have been applied to their dummyArgs)
   *  @See baseTypeSeq  for a definition of sorted and upwards closed.
   */
  private def lubList(ts: List[Type], depth: Int): List[Type] = {
    // Matching the type params of one of the initial types means dummies.
    val initialTypeParams = ts map (_.typeParams)
    def isHotForTs(xs: List[Type]) = initialTypeParams contains xs.map(_.typeSymbol)

    def elimHigherOrderTypeParam(tp: Type) = tp match {
      case TypeRef(pre, sym, args) if args.nonEmpty && isHotForTs(args) => tp.typeConstructor
      case _                                                            => tp
    }
    var lubListDepth = 0
    def loop(tsBts: List[List[Type]]): List[Type] = {
      lubListDepth += 1

      if (tsBts.isEmpty || tsBts.exists(_.isEmpty)) Nil
      else if (tsBts.tail.isEmpty) tsBts.head
      else {
        // ts0 is the 1-dimensional frontier of symbols cutting through 2-dimensional tsBts.
        // Invariant: all symbols "under" (closer to the first row) the frontier
        // are smaller (according to _.isLess) than the ones "on and beyond" the frontier
        val ts0  = tsBts map (_.head)

        // Is the frontier made up of types with the same symbol?
        val isUniformFrontier = (ts0: @unchecked) match {
          case t :: ts  => ts forall (_.typeSymbol == t.typeSymbol)
        }

        // Produce a single type for this frontier by merging the prefixes and arguments of those
        // typerefs that share the same symbol: that symbol is the current maximal symbol for which
        // the invariant holds, i.e., the one that conveys most information wrt subtyping. Before
        // merging, strip targs that refer to bound tparams (when we're computing the lub of type
        // constructors.) Also filter out all types that are a subtype of some other type.
        if (isUniformFrontier) {
          if (settings.debug.value || printLubs) {
            val fbounds = findRecursiveBounds(ts0)
            if (fbounds.nonEmpty) {
              println("Encountered " + fbounds.size + " recursive bounds while lubbing " + ts0.size + " types.")
              for ((p0, p1) <- fbounds) {
                val desc = if (p0 == p1) "its own bounds" else "the bounds of " + p1

                println("  " + p0.fullLocationString + " appears in " + desc)
                println("    " + p1 + " " + p1.info.bounds)
              }
              println("")
            }
          }
          val tails = tsBts map (_.tail)
          mergePrefixAndArgs(elimSub(ts0 map elimHigherOrderTypeParam, depth), 1, depth) match {
            case Some(tp) => tp :: loop(tails)
            case _        => loop(tails)
          }
        }
        else {
          // frontier is not uniform yet, move it beyond the current minimal symbol;
          // lather, rinSe, repeat
          val sym    = minSym(ts0)
          val newtps = tsBts map (ts => if (ts.head.typeSymbol == sym) ts.tail else ts)
          if (printLubs) {
            val str = (newtps.zipWithIndex map { case (tps, idx) =>
              tps.map("        " + _ + "\n").mkString("   (" + idx + ")\n", "", "\n")
            }).mkString("")

            println("Frontier(\n" + str + ")")
            printLubMatrix(ts zip tsBts toMap, lubListDepth)
          }

          loop(newtps)
        }
      }
    }

    val initialBTSes = ts map (_.baseTypeSeq.toList)
    if (printLubs)
      printLubMatrix(ts zip initialBTSes toMap, depth)

    loop(initialBTSes)
  }

  /** The minimal symbol (wrt Symbol.isLess) of a list of types */
  private def minSym(tps: List[Type]): Symbol =
    (tps.head.typeSymbol /: tps.tail) {
      (sym1, tp2) => if (tp2.typeSymbol isLess sym1) tp2.typeSymbol else sym1
    }

  /** A minimal type list which has a given list of types as its base type sequence */
  def spanningTypes(ts: List[Type]): List[Type] = ts match {
    case List() => List()
    case first :: rest =>
      first :: spanningTypes(
        rest filter (t => !first.typeSymbol.isSubClass(t.typeSymbol)))
  }

  /** Eliminate from list of types all elements which are a supertype
   *  of some other element of the list. */
  private def elimSuper(ts: List[Type]): List[Type] = ts match {
    case List() => List()
    case t :: ts1 =>
      val rest = elimSuper(ts1 filter (t1 => !(t <:< t1)))
      if (rest exists (t1 => t1 <:< t)) rest else t :: rest
  }
  def elimAnonymousClass(t: Type) = t match {
    case TypeRef(pre, clazz, Nil) if clazz.isAnonymousClass =>
      clazz.classBound.asSeenFrom(pre, clazz.owner)
    case _ =>
      t
  }
  def elimRefinement(t: Type) = t match {
    case RefinedType(parents, decls) if !decls.isEmpty => intersectionType(parents)
    case _                                             => t
  }

  /** Eliminate from list of types all elements which are a subtype
   *  of some other element of the list. */
  private def elimSub(ts: List[Type], depth: Int): List[Type] = {
    def elimSub0(ts: List[Type]): List[Type] = ts match {
      case List() => List()
      case t :: ts1 =>
        val rest = elimSub0(ts1 filter (t1 => !isSubType(t1, t, decr(depth))))
        if (rest exists (t1 => isSubType(t, t1, decr(depth)))) rest else t :: rest
    }
    val ts0 = elimSub0(ts)
    if (ts0.isEmpty || ts0.tail.isEmpty) ts0
    else {
      val ts1 = ts0 mapConserve (t => elimAnonymousClass(t.underlying))
      if (ts1 eq ts0) ts0
      else elimSub(ts1, depth)
    }
  }

  private def stripExistentialsAndTypeVars(ts: List[Type]): (List[Type], List[Symbol]) = {
    val quantified = ts flatMap {
      case ExistentialType(qs, _) => qs
      case t => List()
    }
    def stripType(tp: Type) = tp match {
      case ExistentialType(_, res) =>
        res
      case tv@TypeVar(_, constr) =>
        if (tv.instValid) constr.inst
        else if (tv.untouchable) tv
        else abort("trying to do lub/glb of typevar "+tp)
      case t => t
    }
    val strippedTypes = ts mapConserve stripType
    (strippedTypes, quantified)
  }

  def weakLub(ts: List[Type]) =
    if (ts.nonEmpty && (ts forall isNumericValueType)) (numericLub(ts), true)
    else if (ts.nonEmpty && (ts exists (_.annotations.nonEmpty)))
      (annotationsLub(lub(ts map (_.withoutAnnotations)), ts), true)
    else (lub(ts), false)

  def weakGlb(ts: List[Type]) = {
    if (ts.nonEmpty && (ts forall isNumericValueType)) {
      val nglb = numericGlb(ts)
      if (nglb != NoType) (nglb, true)
      else (glb(ts), false)
    } else if (ts.nonEmpty && (ts exists (_.annotations.nonEmpty))) {
      (annotationsGlb(glb(ts map (_.withoutAnnotations)), ts), true)
    } else (glb(ts), false)
  }

  def numericLub(ts: List[Type]) =
    ts reduceLeft ((t1, t2) =>
      if (isNumericSubType(t1, t2)) t2
      else if (isNumericSubType(t2, t1)) t1
      else IntClass.tpe)

  def numericGlb(ts: List[Type]) =
    ts reduceLeft ((t1, t2) =>
      if (isNumericSubType(t1, t2)) t1
      else if (isNumericSubType(t2, t1)) t2
      else NoType)

  def isWeakSubType(tp1: Type, tp2: Type) =
    tp1.deconst.normalize match {
      case TypeRef(_, sym1, _) if isNumericValueClass(sym1) =>
        tp2.deconst.normalize match {
          case TypeRef(_, sym2, _) if isNumericValueClass(sym2) =>
            isNumericSubClass(sym1, sym2)
          case tv2 @ TypeVar(_, _) =>
            tv2.registerBound(tp1, isLowerBound = true, isNumericBound = true)
          case _ =>
            isSubType(tp1, tp2)
        }
      case tv1 @ TypeVar(_, _) =>
        tp2.deconst.normalize match {
          case TypeRef(_, sym2, _) if isNumericValueClass(sym2) =>
            tv1.registerBound(tp2, isLowerBound = false, isNumericBound = true)
          case _ =>
            isSubType(tp1, tp2)
        }
      case _ =>
        isSubType(tp1, tp2)
    }

  /** The isNumericValueType tests appear redundant, but without them
   *  test/continuations-neg/function3.scala goes into an infinite loop.
   *  (Even if the calls are to typeSymbolDirect.)
   */
  def isNumericSubType(tp1: Type, tp2: Type) = (
       isNumericValueType(tp1)
    && isNumericValueType(tp2)
    && isNumericSubClass(tp1.typeSymbol, tp2.typeSymbol)
  )

  private val lubResults = new mutable.HashMap[(Int, List[Type]), Type]
  private val glbResults = new mutable.HashMap[(Int, List[Type]), Type]

  def lub(ts: List[Type]): Type = ts match {
    case List() => NothingClass.tpe
    case List(t) => t
    case _ =>
      try {
        lub(ts, lubDepth(ts))
      } finally {
        lubResults.clear()
        glbResults.clear()
      }
  }

  /** The least upper bound wrt <:< of a list of types */
  private def lub(ts: List[Type], depth: Int): Type = {
    def lub0(ts0: List[Type]): Type = elimSub(ts0, depth) match {
      case List() => NothingClass.tpe
      case List(t) => t
      case ts @ PolyType(tparams, _) :: _ =>
        val tparams1 = map2(tparams, matchingBounds(ts, tparams).transpose)((tparam, bounds) =>
          tparam.cloneSymbol.setInfo(glb(bounds, depth)))
        PolyType(tparams1, lub0(matchingInstTypes(ts, tparams1)))
      case ts @ MethodType(params, _) :: rest =>
        MethodType(params, lub0(matchingRestypes(ts, params map (_.tpe))))
      case ts @ NullaryMethodType(_) :: rest =>
        NullaryMethodType(lub0(matchingRestypes(ts, Nil)))
      case ts @ TypeBounds(_, _) :: rest =>
        TypeBounds(glb(ts map (_.bounds.lo), depth), lub(ts map (_.bounds.hi), depth))
      case ts =>
        lubResults get (depth, ts) match {
          case Some(lubType) =>
            lubType
          case None =>
            lubResults((depth, ts)) = AnyClass.tpe
            val res = if (depth < 0) AnyClass.tpe else lub1(ts)
            lubResults((depth, ts)) = res
            res
        }
    }
    def lub1(ts0: List[Type]): Type = {
      val (ts, tparams) = stripExistentialsAndTypeVars(ts0)
      val lubBaseTypes: List[Type] = lubList(ts, depth)
      val lubParents = spanningTypes(lubBaseTypes)
      val lubOwner = commonOwner(ts)
      val lubBase = intersectionType(lubParents, lubOwner)
      val lubType =
        if (phase.erasedTypes || depth == 0) lubBase
        else {
          val lubRefined  = refinedType(lubParents, lubOwner)
          val lubThisType = lubRefined.typeSymbol.thisType
          val narrowts    = ts map (_.narrow)
          def excludeFromLub(sym: Symbol) = (
               sym.isClass
            || sym.isConstructor
            || !sym.isPublic
            || isGetClass(sym)
            || narrowts.exists(t => !refines(t, sym))
          )
          def lubsym(proto: Symbol): Symbol = {
            val prototp = lubThisType.memberInfo(proto)
            val syms = narrowts map (t =>
              t.nonPrivateMember(proto.name).suchThat(sym =>
                sym.tpe matches prototp.substThis(lubThisType.typeSymbol, t)))
            if (syms contains NoSymbol) NoSymbol
            else {
              val symtypes =
                map2(narrowts, syms)((t, sym) => t.memberInfo(sym).substThis(t.typeSymbol, lubThisType))
              if (proto.isTerm) // possible problem: owner of info is still the old one, instead of new refinement class
                proto.cloneSymbol(lubRefined.typeSymbol).setInfoOwnerAdjusted(lub(symtypes, decr(depth)))
              else if (symtypes.tail forall (symtypes.head =:=))
                proto.cloneSymbol(lubRefined.typeSymbol).setInfoOwnerAdjusted(symtypes.head)
              else {
                def lubBounds(bnds: List[TypeBounds]): TypeBounds =
                  TypeBounds(glb(bnds map (_.lo), decr(depth)), lub(bnds map (_.hi), decr(depth)))
                lubRefined.typeSymbol.newAbstractType(proto.name.toTypeName, proto.pos)
                  .setInfoOwnerAdjusted(lubBounds(symtypes map (_.bounds)))
              }
            }
          }
          def refines(tp: Type, sym: Symbol): Boolean = {
            val syms = tp.nonPrivateMember(sym.name).alternatives;
            !syms.isEmpty && (syms forall (alt =>
              // todo alt != sym is strictly speaking not correct, but without it we lose
              // efficiency.
              alt != sym && !specializesSym(lubThisType, sym, tp, alt)))
          }
          // add a refinement symbol for all non-class members of lubBase
          // which are refined by every type in ts.
          for (sym <- lubBase.nonPrivateMembers ; if !excludeFromLub(sym)) {
            try {
              val lsym = lubsym(sym)
              if (lsym != NoSymbol) addMember(lubThisType, lubRefined, lsym)
            } catch {
              case ex: NoCommonType =>
            }
          }
          if (lubRefined.decls.isEmpty) lubBase
          else if (!verifyLubs) lubRefined
          else {
            // Verify that every given type conforms to the calculated lub.
            // In theory this should not be necessary, but higher-order type
            // parameters are not handled correctly.
            val ok = ts forall { t =>
              (t <:< lubRefined) || {
                if (settings.debug.value || printLubs) {
                  Console.println(
                    "Malformed lub: " + lubRefined + "\n" +
                    "Argument " + t + " does not conform.  Falling back to " + lubBase
                  )
                }
                false
              }
            }
            // If not, fall back on the more conservative calculation.
            if (ok) lubRefined
            else lubBase
          }
        }
      existentialAbstraction(tparams, lubType)
    }
    if (printLubs) {
      println(indent + "lub of " + ts + " at depth "+depth)//debug
      indent = indent + "  "
      assert(indent.length <= 100)
    }
    val res = lub0(ts)
    if (printLubs) {
      indent = indent stripSuffix "  "
      println(indent + "lub of " + ts + " is " + res)//debug
    }
    if (ts forall (_.isNotNull)) res.notNull else res
  }

  val GlbFailure = new Throwable

  /** A global counter for glb calls in the `specializes` query connected to the `addMembers`
   *  call in `glb`. There's a possible infinite recursion when `specializes` calls
   *  memberType, which calls baseTypeSeq, which calls mergePrefixAndArgs, which calls glb.
   *  The counter breaks this recursion after two calls.
   *  If the recursion is broken, no member is added to the glb.
   */
  private var globalGlbDepth = 0
  private final val globalGlbLimit = 2

  /** The greatest lower bound wrt <:< of a list of types */
  def glb(ts: List[Type]): Type = elimSuper(ts) match {
    case List() => AnyClass.tpe
    case List(t) => t
    case ts0 =>
      try {
        glbNorm(ts0, lubDepth(ts0))
      } finally {
        lubResults.clear()
        glbResults.clear()
      }
  }

  private def glb(ts: List[Type], depth: Int): Type = elimSuper(ts) match {
    case List() => AnyClass.tpe
    case List(t) => t
    case ts0 => glbNorm(ts0, depth)
  }

  /** The greatest lower bound wrt <:< of a list of types, which have been normalized
   *  wrt elimSuper */
  protected def glbNorm(ts: List[Type], depth: Int): Type = {
    def glb0(ts0: List[Type]): Type = ts0 match {
      case List() => AnyClass.tpe
      case List(t) => t
      case ts @ PolyType(tparams, _) :: _ =>
        val tparams1 = map2(tparams, matchingBounds(ts, tparams).transpose)((tparam, bounds) =>
          tparam.cloneSymbol.setInfo(lub(bounds, depth)))
        PolyType(tparams1, glbNorm(matchingInstTypes(ts, tparams1), depth))
      case ts @ MethodType(params, _) :: rest =>
        MethodType(params, glbNorm(matchingRestypes(ts, params map (_.tpe)), depth))
      case ts @ NullaryMethodType(_) :: rest =>
        NullaryMethodType(glbNorm(matchingRestypes(ts, Nil), depth))
      case ts @ TypeBounds(_, _) :: rest =>
        TypeBounds(lub(ts map (_.bounds.lo), depth), glb(ts map (_.bounds.hi), depth))
      case ts =>
        glbResults get (depth, ts) match {
          case Some(glbType) =>
            glbType
          case _ =>
            glbResults((depth, ts)) = NothingClass.tpe
            val res = if (depth < 0) NothingClass.tpe else glb1(ts)
            glbResults((depth, ts)) = res
            res
        }
    }
    def glb1(ts0: List[Type]): Type = {
      try {
        val (ts, tparams) = stripExistentialsAndTypeVars(ts0)
        val glbOwner = commonOwner(ts)
        def refinedToParents(t: Type): List[Type] = t match {
          case RefinedType(ps, _) => ps flatMap refinedToParents
          case _ => List(t)
        }
        def refinedToDecls(t: Type): List[Scope] = t match {
          case RefinedType(ps, decls) =>
            val dss = ps flatMap refinedToDecls
            if (decls.isEmpty) dss else decls :: dss
          case _ => List()
        }
        val ts1 = ts flatMap refinedToParents
        val glbBase = intersectionType(ts1, glbOwner)
        val glbType =
          if (phase.erasedTypes || depth == 0) glbBase
          else {
            val glbRefined = refinedType(ts1, glbOwner)
            val glbThisType = glbRefined.typeSymbol.thisType
            def glbsym(proto: Symbol): Symbol = {
              val prototp = glbThisType.memberInfo(proto)
              val syms = for (t <- ts;
                    alt <- (t.nonPrivateMember(proto.name).alternatives);
                if glbThisType.memberInfo(alt) matches prototp
              ) yield alt
              val symtypes = syms map glbThisType.memberInfo
              assert(!symtypes.isEmpty)
              proto.cloneSymbol(glbRefined.typeSymbol).setInfoOwnerAdjusted(
                if (proto.isTerm) glb(symtypes, decr(depth))
                else {
                  def isTypeBound(tp: Type) = tp match {
                    case TypeBounds(_, _) => true
                    case _ => false
                  }
                  def glbBounds(bnds: List[Type]): TypeBounds = {
                    val lo = lub(bnds map (_.bounds.lo), decr(depth))
                    val hi = glb(bnds map (_.bounds.hi), decr(depth))
                    if (lo <:< hi) TypeBounds(lo, hi)
                    else throw GlbFailure
                  }
                  val symbounds = symtypes filter isTypeBound
                  var result: Type =
                    if (symbounds.isEmpty)
                      TypeBounds.empty
                    else glbBounds(symbounds)
                  for (t <- symtypes if !isTypeBound(t))
                    if (result.bounds containsType t) result = t
                    else throw GlbFailure
                  result
                })
            }
            if (globalGlbDepth < globalGlbLimit)
              try {
                globalGlbDepth += 1
                val dss = ts flatMap refinedToDecls
                for (ds <- dss; sym <- ds.iterator)
                  if (globalGlbDepth < globalGlbLimit && !(glbThisType specializes sym))
                    try {
                      addMember(glbThisType, glbRefined, glbsym(sym))
                    } catch {
                      case ex: NoCommonType =>
                    }
              } finally {
                globalGlbDepth -= 1
              }
            if (glbRefined.decls.isEmpty) glbBase else glbRefined
          }
        existentialAbstraction(tparams, glbType)
      } catch {
        case GlbFailure =>
          if (ts forall (t => NullClass.tpe <:< t)) NullClass.tpe
          else NothingClass.tpe
      }
    }
    // if (settings.debug.value) { println(indent + "glb of " + ts + " at depth "+depth); indent = indent + "  " } //DEBUG

    val res = glb0(ts)

    // if (settings.debug.value) { indent = indent.substring(0, indent.length() - 2); log(indent + "glb of " + ts + " is " + res) }//DEBUG

    if (ts exists (_.isNotNull)) res.notNull else res
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
  /** Make each type var in this type use its original type for comparisons instead
   * of collecting constraints.
   */
  def suspendTypeVarsInType(tp: Type): List[TypeVar] = {
    val tvs = typeVarsInType(tp)
    // !!! Is it somehow guaranteed that this will not break under nesting?
    // In general one has to save and restore the contents of the field...
    tvs foreach (_.suspended = true)
    tvs
  }

  /** Compute lub (if `variance == 1`) or glb (if `variance == -1`) of given list
   *  of types `tps`. All types in `tps` are typerefs or singletypes
   *  with the same symbol.
   *  Return `Some(x)` if the computation succeeds with result `x`.
   *  Return `None` if the computation fails.
   */
  def mergePrefixAndArgs(tps: List[Type], variance: Int, depth: Int): Option[Type] = tps match {
    case List(tp) =>
      Some(tp)
    case TypeRef(_, sym, _) :: rest =>
      val pres = tps map (_.prefix) // prefix normalizes automatically
      val pre = if (variance == 1) lub(pres, depth) else glb(pres, depth)
      val argss = tps map (_.normalize.typeArgs) // symbol equality (of the tp in tps) was checked using typeSymbol, which normalizes, so should normalize before retrieving arguments
      val capturedParams = new ListBuffer[Symbol]
      try {
        if (sym == ArrayClass && phase.erasedTypes) {
          // special treatment for lubs of array types after erasure:
          // if argss contain one value type and some other type, the lub is Object
          // if argss contain several reference types, the lub is an array over lub of argtypes
          if (argss exists (_.isEmpty)) {
            None  // something is wrong: an array without a type arg.
          } else {
            val args = argss map (_.head)
            if (args.tail forall (_ =:= args.head)) Some(typeRef(pre, sym, List(args.head)))
            else if (args exists (arg => isPrimitiveValueClass(arg.typeSymbol))) Some(ObjectClass.tpe)
            else Some(typeRef(pre, sym, List(lub(args))))
          }
        }
        else {
          val args = map2(sym.typeParams, argss.transpose) { (tparam, as) =>
            if (depth == 0) {
              if (tparam.variance == variance) {
                // Take the intersection of the upper bounds of the type parameters
                // rather than falling all the way back to "Any", otherwise we end up not
                // conforming to bounds.
                val bounds0 = sym.typeParams map (_.info.bounds.hi) filterNot (_.typeSymbol == AnyClass)
                if (bounds0.isEmpty) AnyClass.tpe
                else intersectionType(bounds0 map (b => b.asSeenFrom(tps.head, sym)))
              }
              else if (tparam.variance == -variance) NothingClass.tpe
              else NoType
            }
            else {
              if (tparam.variance == variance) lub(as, decr(depth))
              else if (tparam.variance == -variance) glb(as, decr(depth))
              else {
                val l = lub(as, decr(depth))
                val g = glb(as, decr(depth))
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
          if (args contains NoType) None
          else Some(existentialAbstraction(capturedParams.toList, typeRef(pre, sym, args)))
        }
      } catch {
        case ex: MalformedType => None
        case ex: IndexOutOfBoundsException =>  // transpose freaked out because of irregular argss
        // catching just in case (shouldn't happen, but also doesn't cost us)
        debuglog("transposed irregular matrix!?"+ (tps, argss))
        None
      }
    case SingleType(_, sym) :: rest =>
      val pres = tps map (_.prefix)
      val pre = if (variance == 1) lub(pres, depth) else glb(pres, depth)
      try {
        Some(singleType(pre, sym))
      } catch {
        case ex: MalformedType => None
      }
    case ExistentialType(tparams, quantified) :: rest =>
      mergePrefixAndArgs(quantified :: rest, variance, depth) map (existentialAbstraction(tparams, _))
    case _ =>
      assert(false, tps); None
  }

  /** Make symbol `sym` a member of scope `tp.decls`
   *  where `thistp` is the narrowed owner type of the scope.
   */
  def addMember(thistp: Type, tp: Type, sym: Symbol) {
    assert(sym != NoSymbol)
    // debuglog("add member " + sym+":"+sym.info+" to "+thistp) //DEBUG
    if (!(thistp specializes sym)) {
      if (sym.isTerm)
        for (alt <- tp.nonPrivateDecl(sym.name).alternatives)
          if (specializesSym(thistp, sym, thistp, alt))
            tp.decls unlink alt;
      tp.decls enter sym
    }
  }

  /** All types in list must be polytypes with type parameter lists of
   *  same length as tparams.
   *  Returns list of list of bounds infos, where corresponding type
   *  parameters are renamed to tparams.
   */
  private def matchingBounds(tps: List[Type], tparams: List[Symbol]): List[List[Type]] = {
    def getBounds(tp: Type): List[Type] = tp match {
      case PolyType(tparams1, _) if sameLength(tparams1, tparams) =>
        tparams1 map (tparam => tparam.info.substSym(tparams1, tparams))
      case tp =>
        if (tp ne tp.normalize) getBounds(tp.normalize)
        else throw new NoCommonType(tps)
    }
    tps map getBounds
  }

  /** All types in list must be polytypes with type parameter lists of
   *  same length as tparams.
   *  Returns list of instance types, where corresponding type
   *  parameters are renamed to tparams.
   */
  private def matchingInstTypes(tps: List[Type], tparams: List[Symbol]): List[Type] = {
    def transformResultType(tp: Type): Type = tp match {
      case PolyType(tparams1, restpe) if sameLength(tparams1, tparams) =>
        restpe.substSym(tparams1, tparams)
      case tp =>
        if (tp ne tp.normalize) transformResultType(tp.normalize)
        else throw new NoCommonType(tps)
    }
    tps map transformResultType
  }

  /** All types in list must be method types with equal parameter types.
   *  Returns list of their result types.
   */
  private def matchingRestypes(tps: List[Type], pts: List[Type]): List[Type] =
    tps map {
      case MethodType(params1, res) if (isSameTypes(params1 map (_.tpe), pts)) =>
        res
      case NullaryMethodType(res) if pts isEmpty =>
        res
      case _ =>
        throw new NoCommonType(tps)
    }

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
    if (settings.debug.value) printStackTrace()
  }

  class NoCommonType(tps: List[Type]) extends Throwable(
    "lub/glb of incompatible types: " + tps.mkString("", " and ", "")) with ControlThrowable

  /** A throwable signalling a malformed type */
  class MalformedType(msg: String) extends TypeError(msg) {
    def this(pre: Type, tp: String) = this("malformed type: " + pre + "#" + tp)
  }

  /** The current indentation string for traces */
  private var indent: String = ""

  /** Perform operation `p` on arguments `tp1`, `arg2` and print trace of computation. */
  protected def explain[T](op: String, p: (Type, T) => Boolean, tp1: Type, arg2: T): Boolean = {
    Console.println(indent + tp1 + " " + op + " " + arg2 + "?" /* + "("+tp1.getClass+","+arg2.getClass+")"*/)
    indent = indent + "  "
    val result = p(tp1, arg2)
    indent = indent stripSuffix "  "
    Console.println(indent + result)
    result
  }

  /** If option `explaintypes` is set, print a subtype trace for `found <:< required`. */
  def explainTypes(found: Type, required: Type) {
    if (settings.explaintypes.value) withTypesExplained(found <:< required)
  }

  /** If option `explaintypes` is set, print a subtype trace for `op(found, required)`. */
  def explainTypes(op: (Type, Type) => Any, found: Type, required: Type) {
    if (settings.explaintypes.value) withTypesExplained(op(found, required))
  }

  /** Execute `op` while printing a trace of the operations on types executed. */
  def withTypesExplained[A](op: => A): A = {
    val s = explainSwitch
    try { explainSwitch = true; op } finally { explainSwitch = s }
  }

  def isUnboundedGeneric(tp: Type) = tp match {
    case t @ TypeRef(_, sym, _) => sym.isAbstractType && !(t <:< AnyRefClass.tpe)
    case _                      => false
  }
  def isBoundedGeneric(tp: Type) = tp match {
    case TypeRef(_, sym, _) if sym.isAbstractType => (tp <:< AnyRefClass.tpe)
    case TypeRef(_, sym, _)                       => !isPrimitiveValueClass(sym)
    case _                                        => false
  }
  // Add serializable to a list of parents, unless one of them already is
  def addSerializable(ps: Type*): List[Type] = (
    if (ps exists (_ <:< SerializableClass.tpe)) ps.toList
    else (ps :+ SerializableClass.tpe).toList
  )

  def objToAny(tp: Type): Type =
    if (!phase.erasedTypes && tp.typeSymbol == ObjectClass) AnyClass.tpe
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


  /** The maximum number of recursions allowed in toString
   */
  final val maxTostringRecursions = 50

  private var tostringRecursions = 0

  protected def typeToString(tpe: Type): String =
    if (tostringRecursions >= maxTostringRecursions)
      "..."
    else
      try {
        tostringRecursions += 1
        tpe.safeToString
      } finally {
        tostringRecursions -= 1
      }

}

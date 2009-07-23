/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
//

package scala.tools.nsc.symtab

import scala.collection.immutable
import scala.collection.mutable.{ListBuffer, HashMap, WeakHashMap}
import scala.tools.nsc.ast.TreeGen
import scala.tools.nsc.util.{HashSet, Position, NoPosition}
import Flags._

/* A standard type pattern match:
  case ErrorType =>
    // internal: error
  case WildcardType =>
    // internal: unknown
  case NoType =>
  case NoPrefix =>
  case ThisType(sym) =>
    // sym.this.type
  case SingleType(pre, sym) =>
    // pre.sym.type
  case ConstantType(value) =>
    // int(2)
  case TypeRef(pre, sym, args) =>
    // pre.sym[targs]
  case RefinedType(parents, defs) =>
    // parent1 with ... with parentn { defs }
  case AnnotatedType(annots, tp, selfsym) =>
    // tp @annots

  // the following are non-value types; you cannot write them down in Scala source.

  case TypeBounds(lo, hi) =>
    // >: lo <: hi
  case ClassInfoType(parents, defs, clazz) =>
    // same as RefinedType except as body of class
  case MethodType(paramtypes, result) =>
    // (paramtypes)result
  case PolyType(tparams, result) =>
    // [tparams]result where result is a MethodType or ClassInfoType
    // or
    // []T  for a eval-by-name type
  case ExistentialType(tparams, result) =>
    // exists[tparams]result

  // the last five types are not used after phase `typer'.

  case OverloadedType(pre, tparams, alts) =>
    // all alternatives of an overloaded ident
  case AntiPolyType(pre: Type, targs) =>
  case TypeVar(_, _) =>
    // a type variable
  case DeBruijnIndex(level, index)
*/

trait Types {
  self: SymbolTable =>
  import definitions._


  //statistics
  var singletonBaseTypeSeqCount = 0
  var compoundBaseTypeSeqCount = 0
  var typerefBaseTypeSeqCount = 0
  var findMemberCount = 0
  var noMemberCount = 0
  var multMemberCount = 0
  var findMemberNanos = 0l
  var subtypeCount = 0
  var sametypeCount = 0
  var subtypeNanos = 0l

  private var explainSwitch = false

  private final val LogPendingSubTypesThreshold = 50
  private final val LogPendingBaseTypesThreshold = 50

  /** A don't care value for the depth parameter in lubs/glbs and related operations */
  private final val AnyDepth = -3

  /** Decrement depth unless it is a don't care */
  private final def decr(depth: Int) = if (depth == AnyDepth) AnyDepth else depth - 1

  private final val printLubs = false

  /** The current skolemization level, needed for the algorithms
   *  in isSameType, isSubType that do constraint solving under a prefix
   */
  var skolemizationLevel = 0

  /** A log of type variable with their original constraints. Used in order
   *  to undo constraints in the case of isSubType/isSameType failure.
   */
  type UndoLog = List[(TypeVar, TypeConstraint)]
  var undoLog: UndoLog = List()

  /** A map from lists to compound types that have the given list as parents.
   *  This is used to avoid duplication in the computation of base type sequences and baseClasses.
   *  It makes use of the fact that these two operations depend only on the parents,
   *  not on the refinement.
   */
  var intersectionWitness = new WeakHashMap[List[Type], Type]

  private object gen extends {
    val global : Types.this.type = Types.this
  } with TreeGen

  import gen._

  // @M toString that is safe during debugging (does not normalize, ...)
  def debugString(tp: Type): String = tp match {
    case TypeRef(pre, sym, args) => "TypeRef"+(debugString(pre), sym, args map debugString)
    case ThisType(sym) => "ThisType("+sym+")"
    case SingleType(pre, sym) => "SingleType"+(debugString(pre), sym)
    case RefinedType(parents, defs) => "RefinedType"+(parents map debugString, defs.toList)
    case ClassInfoType(parents, defs, clazz) =>  "ClassInfoType"+(parents map debugString, defs.toList, clazz)
    case PolyType(tparams, result) => "PolyType"+(tparams, debugString(result))
    case TypeBounds(lo, hi) => "TypeBounds "+debugString(lo)+","+debugString(hi)
    case TypeVar(origin, constr) => "TypeVar "+origin+","+constr
    case ExistentialType(tparams, qtpe) => "ExistentialType("+(tparams map (_.defString))+","+debugString(qtpe)+")"
    case _ => tp.toString
  }

  /** A proxy for a type (identified by field `underlying') that forwards most
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

  /** A proxy for a type (identified by field `underlying') that forwards most
   *  operations to it. Every operation that is overridden for some kind of types is
   *  forwarded here. Some opererations are rewrapped again.
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
    override def cloneInfo(owner: Symbol) = maybeRewrap(underlying.cloneInfo(owner))
    override def prefixString = underlying.prefixString
    override def isComplete = underlying.isComplete
    override def complete(sym: Symbol) = underlying.complete(sym)
    override def load(sym: Symbol) { underlying.load(sym) }
    override def withAnnotations(annots: List[AnnotationInfo]) = maybeRewrap(underlying.withAnnotations(annots))
    override def withoutAnnotations = maybeRewrap(underlying.withoutAnnotations)
  }

  /** The base class for all types */
  abstract class Type {

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

    /** Is this type guaranteed not to have `null' as a value? */
    def isNotNull: Boolean = false

    /** Does this depend on an enclosing method parameter? */
    def isDependent: Boolean = IsDependentCollector.collect(this)

    /** The term symbol associated with the type
      * Note that the symbol of the normalized type is returned (@see normalize)
      */
    def termSymbol: Symbol = NoSymbol

    /** The type symbol associated with the type
      * Note that the symbol of the normalized type is returned (@see normalize)
      */
    def typeSymbol: Symbol = NoSymbol

    /** The term symbol *directly* associated with the type
      */
    def termSymbolDirect: Symbol = termSymbol

    /** The type symbol *directly* associated with the type
      */
    def typeSymbolDirect: Symbol = typeSymbol

    /** The base type underlying a type proxy,
     *  identity on all other types */
    def underlying: Type = this

    /** Widen from singleton type to its underlying non-singleton base type
     *  by applying one or more `underlying' derefernces,
     *  identity for all other types */
    def widen: Type = this

    /** Map a constant type or not-null-type to its underlying base type,
     *  identity for all other types */
    def deconst: Type = this

    /** The type of `this' of a class type or reference type
     */
    def typeOfThis: Type = typeSymbol.typeOfThis

    /** Map to a singleton type which is a subtype of this type.
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
    def bounds: TypeBounds = mkTypeBounds(this, this)

    /** For a class or intersection type, its parents.
     *  For a TypeBounds type, the parents of its hi bound.
     *  inherited by typerefs, singleton types, and refinement types,
     *  The empty list for all other types */
    def parents: List[Type] = List()

    /** For a typeref or single-type, the prefix of the normalized type (@see normalize). NoType for all other types. */
    def prefix: Type = NoType

    /** A chain of all typeref or singletype prefixes of this type, longest first */
    def prefixChain: List[Type] = this match {
      case TypeRef(pre, _, _) => pre :: pre.prefixChain
      case SingleType(pre, _) => pre :: pre.prefixChain
      case _ => List()
    }

    /** For a typeref, its arguments. The empty list for all other types */
    def typeArgs: List[Type] = List()

    /** For a method or poly type, its direct result type,
     *  the type itself for all other types */
    def resultType: Type = this

    def resultType(actuals: List[Type]) = this

    def resultApprox: Type = ApproximateDeBruijnMap(resultType)

    /** For a curried method or poly type its non-method result type,
     *  the type itself for all other types */
    def finalResultType: Type = this

    /** For a method or poly type, the number of its value parameter sections,
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
    def boundSyms: List[Symbol] = List()

    /** Mixin a NotNull trait unless type already has one */
    def notNull: Type =
      if (isNotNull || phase.erasedTypes) this else NotNullType(this)

    /** Replace formal type parameter symbols with actual type arguments.
     *
     * Amounts to substitution except for higher-kinded types. (See overridden method in TypeRef) -- @M (contact adriaan.moors at cs.kuleuven.be)
     */
    def instantiateTypeParams(formals: List[Symbol], actuals: List[Type]): Type = this.subst(formals, actuals)

    def skolemizeExistential(owner: Symbol, origin: AnyRef): Type = this

    /** Reduce to beta eta-long normal form. Expands type aliases and converts higher-kinded TypeRef's to PolyTypes. @M */
    def normalize = this // @MAT

    /** Is this type produced as a repair for an error? */
    def isError: Boolean = typeSymbol.isError || termSymbol.isError

    /** Is this type produced as a repair for an error? */
    def isErroneous: Boolean = ErroneousCollector.collect(this)

    /** Does this type denote a reference type which can be null? */
    // def isNullable: Boolean = false

    /** For a classtype or refined type, its defined or declared members;
     *  inherited by subtypes and typerefs.
     *  The empty scope for all other types */
    def decls: Scope = EmptyScope

    /** The defined or declared members with name `name' in this type;
     *  an OverloadedSymbol if several exist, NoSymbol if none exist.
     *  Alternatives of overloaded symbol appear in the order they are declared.
     */
    def decl(name: Name): Symbol = findDecl(name, 0)

    /** The non-private defined or declared members with name `name' in this type;
     *  an OverloadedSymbol if several exist, NoSymbol if none exist.
     *  Alternatives of overloaded symbol appear in the order they are declared.
     */
    def nonPrivateDecl(name: Name): Symbol = findDecl(name, PRIVATE)

    /** A list of all members of this type (defined or inherited)
     *  Members appear in linearization order of their owners.
     *  Members with the same owner appear in reverse order of their declarations.
     */
    def members: List[Symbol] = findMember(nme.ANYNAME, 0, 0, false)(NoSymbol).alternatives

    /** A list of all non-private members of this type (defined or inherited) */
    def nonPrivateMembers: List[Symbol] =
      findMember(nme.ANYNAME, PRIVATE | BRIDGE, 0, false)(NoSymbol).alternatives

    /** A list of all implicit symbols of this type  (defined or inherited) */
    def implicitMembers: List[Symbol] =
      findMember(nme.ANYNAME, BRIDGE, IMPLICIT, false)(NoSymbol).alternatives

    /** A list of all deferred symbols of this type  (defined or inherited) */
    def deferredMembers: List[Symbol] =
      findMember(nme.ANYNAME, BRIDGE, DEFERRED, false)(NoSymbol).alternatives

    /** The member with given name,
     *  an OverloadedSymbol if several exist, NoSymbol if none exist */
    def member(name: Name): Symbol = findMember(name, BRIDGE, 0, false)(NoSymbol)

    /** The non-private member with given name,
     *  an OverloadedSymbol if several exist, NoSymbol if none exist */
    def nonPrivateMember(name: Name): Symbol =
      findMember(name, PRIVATE | BRIDGE, 0, false)(NoSymbol)

    /** The non-local member with given name,
     *  an OverloadedSymbol if several exist, NoSymbol if none exist */
    def nonLocalMember(name: Name)(from : Symbol): Symbol =
      findMember(name, LOCAL | BRIDGE, 0, false)(from)

    /** The least type instance of given class which is a supertype
     *  of this type */
    def baseType(clazz: Symbol): Type = NoType

    /** This type as seen from prefix `pre' and class
     *  `clazz'. This means:
     *  Replace all thistypes of `clazz' or one of its subclasses
     *  by `pre' and instantiate all parameters by arguments of
     *  `pre'.
     *  Proceed analogously for thistypes referring to outer classes.
     */
    def asSeenFrom(pre: Type, clazz: Symbol): Type =
      if (!isTrivial && (!phase.erasedTypes || pre.typeSymbol == ArrayClass)) {
        val m = new AsSeenFromMap(pre.normalize, clazz)
        val tp = m apply this
        existentialAbstraction(m.capturedParams, tp)
      } else this

    /** The info of `sym', seen as a member of this type.
     */
    def memberInfo(sym: Symbol): Type =
      sym.info.asSeenFrom(this, sym.owner)

    /** The type of `sym', seen as a member of this type. */
    def memberType(sym: Symbol): Type = {
      trackTypeIDE(sym)
      //@M don't prematurely instantiate higher-kinded types, they will be instantiated by transform, typedTypeApply, etc. when really necessary
      sym.tpeHK match {
        case ov @ OverloadedType(pre, alts) =>
          OverloadedType(this, alts)
/*
          val pre1 = pre match {
            case ClassInfoType(_, _, clazz) => clazz.tpe
            case _ => pre
          }
          if (this =:= pre1) ov
          else if (this =:= pre1.narrow) OverloadedType(this, alts)
          else {
            Console.println("bad memberType of overloaded symbol: "+this+"/"+pre1+"/"+pre1.narrow)
            assert(false)
            ov
          }
*/
        case tp =>
          val res = tp.asSeenFrom(this, sym.owner)
/*
          if (sym.name.toString == "Elem") {
            println("pre = "+this)
            println("pre.normalize = "+this.widen.normalize)
            println("sym = "+sym+" in "+sym.ownerChain)
            println("result = "+res)
          }
*/
          res
      }
    }

    /** Substitute types `to' for occurrences of references to
     *  symbols `from' in this type.
     */
    def subst(from: List[Symbol], to: List[Type]): Type =
      new SubstTypeMap(from, to) apply this

    /** Substitute symbols `to' for occurrences of symbols
     *  `from' in this type.
     * !!! NOTE !!!: If you need to do a substThis and a substSym, the substThis has to come
     * first, as otherwise symbols will immediately get rebound in typeRef to the old
     * symbol.
     */
    def substSym(from: List[Symbol], to: List[Symbol]): Type = if (from eq to) this
    else new SubstSymMap(from, to) apply this

    /** Substitute all occurrences of `ThisType(from)' in this type
     *  by `to'.
     * !!! NOTE !!!: If you need to do a substThis and a substSym, the substThis has to come
     * first, as otherwise symbols will immediately get rebound in typeRef to the old
     * symbol.
     */
    def substThis(from: Symbol, to: Type): Type =
      new SubstThisMap(from, to) apply this

    def substSuper(from: Type, to: Type): Type =
      new SubstSuperMap(from, to) apply this

    /** Returns all parts of this type which satisfy predicate `p' */
    def filter(p: Type => Boolean): List[Type] = new FilterTypeCollector(p).collect(this).toList

    /** Returns optionally first type (in a preorder traversal) which satisfies predicate `p',
     *  or None if none exists.
     */
    def find(p: Type => Boolean): Option[Type] = new FindTypeCollector(p).collect(this)

    /** Apply `f' to each part of this type */
    def foreach(f: Type => Unit) { new ForEachTypeTraverser(f).traverse(this) }

    /** Is there part of this type which satisfies predicate `p'? */
    def exists(p: Type => Boolean): Boolean = !find(p).isEmpty

    /** Does this type contain a reference to this symbol? */
    def contains(sym: Symbol): Boolean = new ContainsCollector(sym).collect(this)

    /** Does this type contain a reference to this type */
    def containsTp(tp: Type): Boolean = new ContainsTypeCollector(tp).collect(this)

    /** Is this type a subtype of that type? */
    def <:<(that: Type): Boolean = {
//      val startTime = if (util.Statistics.enabled) System.nanoTime() else 0l
      val result =
        ((this eq that) ||
         (if (explainSwitch) explain("<", isSubType, this, that)
          else isSubType(this, that, AnyDepth)))
//      if (util.Statistics.enabled) {
//        subtypeNanos += System.nanoTime() - startTime
//        subtypeCount += 1
//      }
      result
    }

    /** Is this type equivalent to that type? */
    def =:=(that: Type): Boolean = (
      (this eq that) ||
      (if (explainSwitch) explain("=", isSameType, this, that)
       else isSameType(this, that))
    );

    /** Does this type implement symbol `sym' with same or stronger type?
     */
    def specializes(sym: Symbol): Boolean =
      if (explainSwitch) explain("specializes", specializesSym, this, sym)
      else specializesSym(this, sym)

    /** Is this type close enough to that type so that
     *  members with the two type would override each other?
     *  This means:
     *    - Either both types are polytypes with the same number of
     *      type parameters and their result types match after renaming
     *      corresponding type parameters
     *    - Or both types are method types with equivalent type parameter types
     *      and matching result types
     *    - Or both types are equivalent
     *    - Or phase.erasedTypes is false and both types are neither method nor
     *      poly types.
     */
    def matches(that: Type): Boolean = matchesType(this, that, !phase.erasedTypes)

    /** Same as matches, except that non-method types are always assumed to match.
     */
    def looselyMatches(that: Type): Boolean = matchesType(this, that, true)

    /** The shortest sorted upwards closed array of types that contains
     *  this type as first element.
     *
     *  A list or array of types ts is upwards closed if
     *
     *    for all t in ts:
     *      for all typerefs p.s[args] such that t &lt;: p.s[args]
     *      there exists a typeref p'.s[args'] in ts such that
     *      t &lt;: p'.s['args] &lt;: p.s[args],
     *      and
     *      for all singleton types p.s such that t &lt;: p.s
     *      there exists a singleton type p'.s in ts such that
     *      t &lt;: p'.s &lt;: p.s
     *
     *  Sorting is with respect to Symbol.isLess() on type symbols.
     */
    def baseTypeSeq: BaseTypeSeq = baseTypeSingletonSeq(this)

    /** The maximum depth (@see maxDepth) of each type in the BaseTypeSeq of this type. */
    def baseTypeSeqDepth: Int = 1

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
        else throw new Error()
      }
      -1
    }

    /** If this is a poly- or methodtype, a copy with cloned type / value parameters
     *  owned by `owner'. Identity for all other types.
     */
    def cloneInfo(owner: Symbol) = this

    protected def objectPrefix = "object "
    protected def packagePrefix = "package "

    def trimPrefix(str: String) =
      if (str.startsWith(objectPrefix)) str.substring(objectPrefix.length)
      else if (str.startsWith(packagePrefix)) str.substring(packagePrefix.length)
      else str

    /** The string representation of this type used as a prefix */
    def prefixString = trimPrefix(toString) + "#"

    /** The string representation of this type, with singletypes explained */
    def toLongString = {
      val str = toString
      if (str endsWith ".type") str + " (with underlying type " + widen + ")"
      else str
    }

    /** A test whether a type contains any unification type variables */
    def isGround: Boolean = this match {
      case TypeVar(_, constr) =>
        constr.inst != NoType && constr.inst.isGround
      case TypeRef(pre, sym, args) =>
        sym.isPackageClass || pre.isGround && (args forall (_.isGround))
      case SingleType(pre, sym) =>
        sym.isPackageClass || pre.isGround
      case ThisType(_) | NoPrefix | WildcardType | NoType | ErrorType | ConstantType(_) =>
        true
      case _ =>
        typeVarToOriginMap(this) eq this
    }

    /** Is this type completed (i.e. not a lazy type)?
     */
    def isComplete: Boolean = true

    /** Is this type a varargs parameter?
     */
    def isVarargs: Boolean = typeSymbol == RepeatedParamClass

    /** If this is a lazy type, assign a new type to `sym'. */
    def complete(sym: Symbol) {}

    /** If this is a symbol loader type, load and assign a new type to
     *  `sym'.
     */
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
     *  @param name           The member's name, where nme.ANYNAME means `unspecified'
     *  @param excludedFlags  Returned members do not have these flags
     *  @param requiredFlags  Returned members do have these flags
     *  @param stableOnly     If set, return only members that are types or stable values
     *  @param from           ??
     */
    //TODO: use narrow only for modules? (correct? efficiency gain?)
    def findMember(name: Name, excludedFlags: Int, requiredFlags: Long, stableOnly: Boolean)(from:Symbol): Symbol = {
      // if this type contains type variables, get rid of them;
      // without this, the matchesType call would lead to type variables on both sides
      // of a subtyping/equality judgement, which can lead to recursive types being constructed.
      // See (t0851) for a situation where this happens.
      if (!this.isGround)
        return typeVarToOriginMap(this).findMember(name, excludedFlags, requiredFlags, stableOnly)(from)
      if (util.Statistics.enabled) findMemberCount += 1
//      val startTime = if (util.Statistics.enabled) System.nanoTime() else 0l

      //Console.println("find member " + name.decode + " in " + this + ":" + this.baseClasses)//DEBUG
      var members: Scope = null
      var member: Symbol = NoSymbol
      var excluded = excludedFlags | DEFERRED
      var self: Type = null
      var continue = true
      while (continue) {
        continue = false
        val bcs0 = baseClasses
        var bcs = bcs0
        while (!bcs.isEmpty) {
          val decls = bcs.head.info.decls
          var entry =
            if (name == nme.ANYNAME) decls.elems else decls.lookupEntryWithContext(name)(from)
          while (entry ne null) {
            val sym = entry.sym
            if (sym.getFlag(requiredFlags) == requiredFlags) {
              val excl = sym.getFlag(excluded)
              if (excl == 0 &&
                  (// omit PRIVATE LOCALS unless selector class is contained in class owning the def.
                   (bcs eq bcs0) ||
                   sym.getFlag(PRIVATE | LOCAL) != (PRIVATE | LOCAL) ||
                   (bcs0.head.hasTransOwner(bcs.head)))) {
                if (name.isTypeName || stableOnly && sym.isStable) {
//                  if (util.Statistics.enabled) findMemberNanos += System.nanoTime() - startTime
                  return sym
                } else if (member == NoSymbol) {
                  member = sym
                } else if (members eq null) {
                  if (member.name != sym.name ||
                      !(member == sym ||
                        member.owner != sym.owner &&
                        !sym.hasFlag(PRIVATE) && {
                          if (self eq null) self = this.narrow;
                          (self.memberType(member) matches self.memberType(sym))
                        })) {
                    members = newThrowAwayScope(List(member, sym))
                  }
                } else {
                  var prevEntry = members.lookupEntryWithContext(sym.name)(from)
                  while ((prevEntry ne null) &&
                         !(prevEntry.sym == sym ||
                           prevEntry.sym.owner != sym.owner &&
                           !sym.hasFlag(PRIVATE) && {
                             if (self eq null) self = this.narrow;
                             (self.memberType(prevEntry.sym) matches self.memberType(sym))
                           })) {
                    prevEntry = members lookupNextEntry prevEntry
                  }
                  if (prevEntry eq null) {
                    members enter sym
                  }
                }
              } else if (excl == DEFERRED) {
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
//      if (util.Statistics.enabled) findMemberNanos += System.nanoTime() - startTime
      if (members eq null) {
        if (util.Statistics.enabled) if (member == NoSymbol) noMemberCount += 1;
        member
      } else {
        if (util.Statistics.enabled) multMemberCount += 1;
        //val pre = if (this.typeSymbol.isClass) this.typeSymbol.thisType else this;
        (baseClasses.head.newOverloaded(this, members.toList))
      }
    }

    /** The existential skolems and existentially quantifed variables which are free in this type */
    def existentialSkolems: List[Symbol] = {
      var boundSyms: List[Symbol] = List()
      var skolems: List[Symbol] = List()
      for (t <- this) {
        t match {
          case ExistentialType(quantified, qtpe) =>
            boundSyms = boundSyms ::: quantified
          case TypeRef(_, sym, _) =>
            if ((sym hasFlag EXISTENTIAL) && !(boundSyms contains sym) && !(skolems contains sym))
              skolems = sym :: skolems
          case _ =>
        }
      }
      skolems
    }

    /** Return the annotations on this type. */
    def annotations: List[AnnotationInfo] = Nil

    /** Test for the presence of an annotation */
    def hasAnnotation(clazz: Symbol) = annotations exists { _.atp.typeSymbol == clazz }

    /** Add an annotation to this type */
    def withAnnotation(annot: AnnotationInfo) = withAnnotations(List(annot))

    /** Add a number of annotations to this type */
    def withAnnotations(annots: List[AnnotationInfo]): Type =
      annots match {
        case Nil => this
        case _ => AnnotatedType(annots, this, NoSymbol)
      }

    /** Remove any annotations from this type */
    def withoutAnnotations = this

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

    override def toString: String =
      if (tostringRecursions >= maxTostringRecursions)
        "..."
      else
        try {
          tostringRecursions += 1
          safeToString
        } finally {
          tostringRecursions -= 1
        }

    def safeToString: String = super.toString
  }

  private final val maxTostringRecursions = 50
  private var tostringRecursions = 0

// Subclasses ------------------------------------------------------------

  trait UniqueType {
    override lazy val hashCode: Int = super.hashCode()
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
      if (util.Statistics.enabled) singletonBaseTypeSeqCount += 1
      underlying.baseTypeSeq prepend this
    }
    override def safeToString: String = prefixString + "type"
/*
    override def typeOfThis: Type = typeSymbol.typeOfThis
    override def bounds: TypeBounds = mkTypeBounds(this, this)
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
    override def findMember(name: Name, excludedFlags: Int,
                            requiredFlags: Long, stableOnly: Boolean)(from : Symbol): Symbol = {
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

  /** An object representing an unknown type */
  case object WildcardType extends Type {
    override def safeToString: String = "?"
    // override def isNullable: Boolean = true
    override def kind = "WildcardType"
  }

  case class BoundedWildcardType(override val bounds: TypeBounds) extends Type {
    override def safeToString: String = "?" + bounds
    override def kind = "BoundedWildcardType"
  }

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
  case class ThisType(sym: Symbol) extends SingletonType {
    //assert(sym.isClass && !sym.isModuleClass || sym.isRoot, sym)
    override def isTrivial: Boolean = sym.isPackageClass
    override def isNotNull = true
    override def typeSymbol = sym
    override def underlying: Type = sym.typeOfThis
    override def isVolatile = false
    override def prefixString =
      if (settings.debug.value) sym.nameString + ".this."
      else if (sym.isRoot || sym.isEmptyPackageClass || sym.isInterpreterWrapper || sym.isScalaPackageClass) ""
      else if (sym.isAnonymousClass || sym.isRefinementClass) "this."
      else if (sym.isModuleClass) sym.fullNameString + "."
      else sym.nameString + ".this."
    override def safeToString: String =
      if (sym.isRoot) "<root>"
      else if (sym.isEmptyPackageClass) "<empty>"
      else super.safeToString
    override def narrow: Type = this
    override def kind = "ThisType"
  }

  case class DeBruijnIndex(level: Int, paramId: Int) extends Type {
    override def isTrivial = true
    override def isStable = true
    override def safeToString = "<param "+level+"."+paramId+">"
    override def kind = "DeBruijnIndex"
    // todo: this should be a subtype, which forwards to underlying
  }

  /** A class for singleton types of the form &lt;prefix&gt;.&lt;sym.name&gt;.type.
   *  Cannot be created directly; one should always use
   *  `singleType' for creation.
   */
  case class SingleType(pre: Type, sym: Symbol) extends SingletonType {
    override val isTrivial: Boolean = pre.isTrivial
    // override def isNullable = underlying.isNullable
    override def isNotNull = underlying.isNotNull
    private var underlyingCache: Type = NoType
    private var underlyingPeriod = NoPeriod
    override def underlying: Type = {
      val period = underlyingPeriod
      if (period != currentPeriod) {
        underlyingPeriod = currentPeriod
        if (!isValid(period)) {
          underlyingCache = pre.memberType(sym).resultType;
        }
      }
      assert(underlyingCache ne this, this)
      underlyingCache
    }

    override def isVolatile : Boolean = underlying.isVolatile && (!sym.isStable)
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
    override def prefixString: String =
      if ((sym.isEmptyPackage || sym.isInterpreterWrapper || sym.isPredefModule || sym.isScalaPackage) && !settings.debug.value) ""
      else pre.prefixString + sym.nameString + "."
    override def kind = "SingleType"
  }

  case class SuperType(thistpe: Type, supertpe: Type) extends SingletonType {
    override val isTrivial: Boolean = thistpe.isTrivial && supertpe.isTrivial
    override def isNotNull = true;
    override def typeSymbol = thistpe.typeSymbol
    override def underlying = supertpe
    override def prefix: Type = supertpe.prefix
    override def prefixString =
      if (thistpe.prefixString.endsWith("this."))
        thistpe.prefixString.substring(0, thistpe.prefixString.length() - 5) + "super."
      else thistpe.prefixString;
    override def narrow: Type = thistpe.narrow
    override def kind = "SuperType"
  }

  /** A class for the bounds of abstract types and type parameters
   */
  case class TypeBounds(lo: Type, hi: Type) extends SubType {
    def supertype = hi
    override val isTrivial: Boolean = lo.isTrivial && hi.isTrivial
    override def bounds: TypeBounds = this
    def containsType(that: Type) = that match {
      case TypeBounds(_, _) => that <:< this
      case _ => lo <:< that && that <:< hi
    }
    // override def isNullable: Boolean = NullClass.tpe <:< lo;
    override def safeToString = ">: " + lo + " <: " + hi
    override def kind = "TypeBoundsType"
  }

  /** A common base class for intersection types and class types
   */
  abstract class CompoundType extends Type {

    var baseTypeSeqCache: BaseTypeSeq = _
    private var baseTypeSeqPeriod = NoPeriod
    private var baseClassesCache: List[Symbol] = _
    private var baseClassesPeriod = NoPeriod

    override def baseTypeSeq: BaseTypeSeq = {
      val period = baseTypeSeqPeriod;
      if (period != currentPeriod) { // no caching in IDE
        baseTypeSeqPeriod = currentPeriod
        if (!isValidForBaseClasses(period)) {
          if (util.Statistics.enabled)
            compoundBaseTypeSeqCount += 1
          baseTypeSeqCache = undetBaseTypeSeq
          baseTypeSeqCache = memo(compoundBaseTypeSeq(this))(_.baseTypeSeq updateHead typeSymbol.tpe)
//          println("normalizing baseTypeSeq of "+typeSymbol+"/"+parents+": "+baseTypeSeqCache)//DEBUG
          baseTypeSeqCache.normalize(parents)
//          println("normalized baseTypeSeq of "+typeSymbol+"/"+parents+": "+baseTypeSeqCache)//DEBUG
        }
        //Console.println("baseTypeSeq(" + typeSymbol + ") = " + baseTypeSeqCache.toList);//DEBUG
      }
      if (baseTypeSeqCache eq undetBaseTypeSeq)
        throw new TypeError("illegal cyclic inheritance involving " + typeSymbol)
      baseTypeSeqCache
    }

    override def baseTypeSeqDepth: Int = baseTypeSeq.maxDepth

    override def baseClasses: List[Symbol] = {
      def computeBaseClasses: List[Symbol] =
        if (parents.isEmpty) List(typeSymbol)
        else {
          //Console.println("computing base classes of " + typeSymbol + " at phase " + phase);//DEBUG
          // optimized, since this seems to be performance critical
          val superclazz = parents.head
          var mixins = parents.tail
          val sbcs = superclazz.baseClasses
          var bcs = sbcs
          def isNew(clazz: Symbol): Boolean = (
            superclazz.baseTypeIndex(clazz) < 0 &&
            { var p = bcs;
              while ((p ne sbcs) && (p.head != clazz)) p = p.tail;
              p eq sbcs
            }
          );
          while (!mixins.isEmpty) {
            def addMixinBaseClasses(mbcs: List[Symbol]): List[Symbol] =
              if (mbcs.isEmpty) bcs
              else if (isNew(mbcs.head)) mbcs.head :: addMixinBaseClasses(mbcs.tail)
              else addMixinBaseClasses(mbcs.tail);
            bcs = addMixinBaseClasses(mixins.head.baseClasses)
            mixins = mixins.tail
          }
          typeSymbol :: bcs
         }
      val period = baseClassesPeriod
      if (period != currentPeriod) {
        baseClassesPeriod = currentPeriod
        if (!isValidForBaseClasses(period)) {
          baseClassesCache = null
          baseClassesCache = memo(computeBaseClasses)(typeSymbol :: _.baseClasses.tail)
        }
      }
      if (baseClassesCache eq null)
        throw new TypeError("illegal cyclic reference involving " + typeSymbol)
      baseClassesCache
    }

    def memo[A](op1: => A)(op2: Type => A) = intersectionWitness get parents match {
      case Some(w) =>
        if (w eq this) op1 else op2(w)
      case None =>
        intersectionWitness(parents) = this
        op1
    }

    override def baseType(sym: Symbol): Type = {
      val index = baseTypeIndex(sym)
      if (index >= 0) baseTypeSeq(index) else NoType
    }

    override def narrow: Type = typeSymbol.thisType
    override def isNotNull: Boolean = parents exists (_.isNotNull)

    // override def isNullable: Boolean =
    // parents forall (p => p.isNullable && !p.typeSymbol.isAbstractType);

    override def safeToString: String =
      parents.mkString("", " with ", "") +
      (if (settings.debug.value || parents.isEmpty || (decls.elems ne null))
        decls.mkString("{", "; ", "}") else "")
  }

  /** A class representing intersection types with refinements of the form
   *    `&lt;parents_0&gt; with ... with &lt;parents_n&gt; { decls }'
   *  Cannot be created directly;
   *  one should always use `refinedType' for creation.
   */
  case class RefinedType(override val parents: List[Type],
                         override val decls: Scope) extends CompoundType {

    override def isHigherKinded =
      !parents.isEmpty && (parents forall (_.isHigherKinded)) // @MO to AM: please check this class!

    override def typeParams =
      if (isHigherKinded) parents.head.typeParams
      else super.typeParams

    private def higherKindedArgs =
      typeParams map (_.typeConstructor)

    /* MO to AM: This is probably not correct
     * If they are several higher-kinded parents with different bounds we need
     * to take the intersection of their bounds
     */
    override def normalize =
      if (isHigherKinded)
        PolyType(
          typeParams,
          refinementOfClass(
            typeSymbol,
            parents map {
              case TypeRef(pre, sym, List()) => TypeRef(pre, sym, higherKindedArgs)
              case p => p
            },
            decls))
      else super.normalize

    /** A refined type P1 with ... with Pn { decls } is volatile if
     *  one of the parent types Pi is an abstract type, and
     *  either i > 1, or decls or a following parent Pj, j > 1, contributes
     *  an abstract member.
     *  A type contributes an abstract member if it has an abstract member which
     *  is also a member of the whole refined type. A scope `decls' contributes
     *  an abstract member if it has an abstract definition which is also
     *  a member of the whole type.
     */
    override def isVolatile = {
      def isVisible(m: Symbol) =
        this.nonPrivateMember(m.name).alternatives contains m
      def contributesAbstractMembers(p: Type) =
        p.deferredMembers exists isVisible

      (parents exists (_.isVolatile)) ||
      (parents dropWhile (! _.typeSymbol.isAbstractType) match {
        case ps @ (_ :: ps1) =>
          (ps ne parents) ||
          (ps1 exists contributesAbstractMembers) ||
          (decls.iterator exists (m => m.isDeferred && isVisible(m)))
        case _ =>
          false
       })
    }

    override def kind = "RefinedType"
  }

  /** A class representing a class info
   */
  case class ClassInfoType(
    override val parents: List[Type],
    override val decls: Scope,
    override val typeSymbol: Symbol) extends CompoundType
  {

    /** refs indices */
    private final val NonExpansive = 0
    private final val Expansive = 1

    /** initialization states */
    private final val UnInitialized = 0
    private final val Initializing = 1
    private final val Initialized = 2

    private type RefMap = Map[Symbol, collection.immutable.Set[Symbol]]

    /** All type parameters reachable from given type parameter
     *  by a path which contains at least one expansive reference.
     *  @See Kennedy, Pierce: On Decidability of Nominal Subtyping with Variance
     */
    def expansiveRefs(tparam: Symbol) = {
      if (state == UnInitialized) {
        computeRefs()
        while (state != Initialized) propagate()
      }
      getRefs(Expansive, tparam)
    }

    /* The rest of this class is auxiliary code for `expansiveRefs'
     */

    /** The type parameters which are referenced type parameters of this class.
     *  Two entries: refs(0): Non-expansive references
     *               refs(1): Expansive references
     */
    private var refs: Array[RefMap] = _

    /** The initialization state of the class: UnInialized --> Initializing --> Initialized
     */
    private var state = UnInitialized

    /** Get references for given type parameter
     *  @param  which in {NonExpansive, Expansive}
     *  @param  from  The type parameter from which references originate.
     */
    private def getRefs(which: Int, from: Symbol): Set[Symbol] = refs(which) get from match {
      case Some(set) => set
      case None => Set()
    }

    /** Augment existing refs map with reference <pre>from -> to</pre>
     *  @param  which <- {NonExpansive, Expansive}
     */
    private def addRef(which: Int, from: Symbol, to: Symbol) {
      refs(which) = refs(which) + (from -> (getRefs(which, from) + to))
    }

    /** Augment existing refs map with references <pre>from -> sym</pre>, for
     *  all elements <pre>sym</pre> of set `to'.
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

    /** Compute initial (one-step) references and set state to `Initializing'.
     */
    private def computeRefs() {
      refs = Array(Map(), Map())
      for (tparam <- typeSymbol.typeParams) {
        val enterRefs = new TypeMap {
          def apply(tp: Type): Type = {
            tp match {
              case TypeRef(_, sym, args) =>
                for ((tparam1, arg) <- sym.info.typeParams zip args)
                  if (arg contains tparam) {
                    addRef(NonExpansive, tparam, tparam1)
                    if (arg.typeSymbol != tparam) addRef(Expansive, tparam, tparam1)
                  }
              case _ =>
            }
            mapOver(tp)
          }
        }
        for (p <- parents) enterRefs(p)
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
  }

  class PackageClassInfoType(decls: Scope, clazz: Symbol, val lazyLoader : LazyType)
  extends ClassInfoType(List(), decls, clazz) {
    def reset = clazz.setInfo(lazyLoader)
  }

  /** A class representing a constant type.
   *
   *  @param value ...
   */
  case class ConstantType(value: Constant) extends SingletonType {
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

  /** A class for named types of the form
   *  `&lt;prefix&gt;.&lt;sym.name&gt;[args]'
   *  Cannot be created directly; one should always use `typeRef'
   *  for creation. (@M: Otherwise hashing breaks)
   *
   * @M: Higher-kinded types are represented as TypeRefs with a symbol that has type parameters, but with args==List()
   *  @param pre  ...
   *  @param sym  ...
   *  @param args ...
   */
  case class TypeRef(pre: Type, sym: Symbol, args: List[Type]) extends Type {
//    assert(!sym.isAbstractType || pre.isStable || pre.isError)
//    assert(!pre.isInstanceOf[ClassInfoType], this)
//    assert(!(sym hasFlag (PARAM | EXISTENTIAL)) || pre == NoPrefix, this)
//    assert(args.isEmpty || !sym.info.typeParams.isEmpty, this)

    private var parentsCache: List[Type] = _
    private var parentsPeriod = NoPeriod
    private var baseTypeSeqCache: BaseTypeSeq = _
    private var baseTypeSeqPeriod = NoPeriod

    override def isStable: Boolean = {
      sym == SingletonClass ||
      sym.isAliasType && normalize.isStable ||
      sym.isAbstractType && (bounds.hi.typeSymbol isSubClass SingletonClass)
    }

    override def isVolatile: Boolean =
      sym.isAliasType && normalize.isVolatile ||
      sym.isAbstractType && bounds.hi.isVolatile

    override val isTrivial: Boolean =
      pre.isTrivial && !sym.isTypeParameter && args.forall(_.isTrivial)

    override def isNotNull =
      sym.isModuleClass || sym == NothingClass || isValueClass(sym) || super.isNotNull

    // @M: propagate actual type params (args) to `tp', by replacing formal type parameters with actual ones
    def transform(tp: Type): Type = {
      val args = argsMaybeDummy
      if (args.length == sym.typeParams.length)
        tp.asSeenFrom(pre, sym.owner).instantiateTypeParams(sym.typeParams, argsMaybeDummy)
      else { assert(sym.typeParams.isEmpty || (args exists (_.isError)), tp); tp }
      // @M TODO maybe we shouldn't instantiate type params if isHigherKinded -- probably needed for partial type application though
    }

    //@M! use appliedType on the polytype that represents the bounds (or if aliastype, the rhs)
    def transformInfo(tp: Type): Type =
      appliedType(tp.asSeenFrom(pre, sym.owner), argsMaybeDummy)
      // TODO: argsMaybeDummy --> ok? or don't instantiate type params if isHigherKinded

    def thisInfo     =
      if (sym.isAliasType) normalize
      else if (sym.isTypeMember) transformInfo(sym.info)
      else sym.info

    def relativeInfo = if (sym.isTypeMember) transformInfo(pre.memberInfo(sym)) else pre.memberInfo(sym)

    override def typeSymbol = if (sym.isAliasType) normalize.typeSymbol else sym
    override def termSymbol = if (sym.isAliasType) normalize.termSymbol else super.termSymbol
    override def typeSymbolDirect = sym
    override def termSymbolDirect = super.termSymbol

/* @MAT
whenever you see `tp.typeSymbol.isXXXX' and then act on tp based on that predicate, you're on thin ice,
as `typeSymbol' (and `prefix') automatically normalize, but the other inspectors don't.
In other words, even if `tp.normalize.sym.isXXX' is true, `tp.sym.isXXX' may be false (if sym were a public method to access the non-normalized typeSymbol)...

In retrospect, I think `tp.typeSymbol.isXXX' or (worse) `tp.typeSymbol==XXX' should be replaced by `val tp = tp0.asXXX'.
A type's typeSymbol should never be inspected directly.
*/

    override def bounds: TypeBounds =
      if (sym.isAbstractType) thisInfo.bounds // transform(thisInfo.bounds).asInstanceOf[TypeBounds] // ??? seems to be doing asSeenFrom twice
      else super.bounds

    override def parents: List[Type] = {
      val period = parentsPeriod
      if (period != currentPeriod) {
        parentsPeriod = currentPeriod
        if (!isValidForBaseClasses(period)) {
          parentsCache = thisInfo.parents map transform
        }
      }
      parentsCache
    }

    override def typeOfThis = transform(sym.typeOfThis)

/*
    override def narrow =
      if (sym.isModuleClass) transform(sym.thisType)
      else if (sym.isAliasType) normalize.narrow
      else super.narrow
*/
    override def narrow =
      if (sym.isModuleClass) singleType(pre, sym.sourceModule)
      else if (sym.isAliasType) normalize.narrow
      else super.narrow

    override def prefix: Type =
      if (sym.isAliasType) normalize.prefix
      else pre

    override def typeArgs: List[Type] = args

    override def typeParams: List[Symbol] =
      if (args.isEmpty) sym.unsafeTypeParams else List()
         // @MAT was typeSymbol.unsafeTypeParams, but typeSymbol normalizes now

    override def instantiateTypeParams(formals: List[Symbol], actuals: List[Type]): Type =
      if (isHigherKinded) {
        val substTps = formals.intersect(typeParams)

        if (substTps.length == typeParams.length)
          typeRef(pre, sym, actuals)
        else // partial application (needed in infer when bunching type arguments from classes and methods together)
          typeRef(pre, sym, higherKindedArgs).subst(formals, actuals)
      }
      else
        super.instantiateTypeParams(formals, actuals)

    override def isHigherKinded = !typeParams.isEmpty  //@M args.isEmpty is checked in typeParams

    private def higherKindedArgs = typeParams map (_.typeConstructor) //@M must be .typeConstructor
    private def argsMaybeDummy = if (isHigherKinded) higherKindedArgs else args

    private var normalized: Type = null

    def normalize0: Type =
      if (sym.isAliasType) { // beta-reduce
        if (sym.info.typeParams.length == args.length || !isHigherKinded) {
          /* !isHigherKinded && sym.info.typeParams.length != args.length only happens when compiling e.g.,
           `val x: Class' with -Xgenerics, while `type Class = java.lang.Class' had already been compiled without -Xgenerics */
          val xform = transform(sym.info.resultType)
          assert(xform ne this, this)
          xform.normalize // cycles have been checked in typeRef
        } else {
          PolyType(typeParams, transform(sym.info.resultType).normalize)  // eta-expand
          // @M TODO: should not use PolyType, as that's the type of a polymorphic value -- we really want a type *function*
        }
      } else if (isHigherKinded) {
        // @M TODO: should not use PolyType, as that's the type of a polymorphic value -- we really want a type *function*
        // @M: initialize needed (see test/files/pos/ticket0137.scala)
        PolyType(typeParams, typeRef(pre, sym.initialize, higherKindedArgs))
      } else if (sym.isRefinementClass) {
        sym.info.normalize // @MO to AM: OK?
      } else {
        super.normalize
      }

    override def normalize: Type =
      if (phase.erasedTypes) normalize0
      else {
        if (normalized == null) normalized = normalize0
        normalized
      }

    override def decls: Scope = {
      sym.info match {
        case TypeRef(_, sym1, _) =>
          assert(sym1 != sym, this) // @MAT was != typeSymbol
        case _ =>
      }
      thisInfo.decls
    }

    override def baseType(clazz: Symbol): Type =
      if (sym == clazz) this
      else if (sym.isClass) transform(sym.info.baseType(clazz))
      else
        try {
          basetypeRecursions += 1
          if (basetypeRecursions < LogPendingBaseTypesThreshold)
            relativeInfo.baseType(clazz)
          else if (pendingBaseTypes contains this)
            if (clazz == AnyClass) clazz.tpe else NoType
          else
            try {
              pendingBaseTypes += this
              relativeInfo.baseType(clazz)
            } finally {
              pendingBaseTypes -= this
            }
        } finally {
          basetypeRecursions -= 1
        }

    override def baseTypeSeq: BaseTypeSeq = {
      val period = baseTypeSeqPeriod
      if (period != currentPeriod) {
        baseTypeSeqPeriod = currentPeriod
        if (!isValidForBaseClasses(period)) {
          if (util.Statistics.enabled)
            typerefBaseTypeSeqCount += 1
          baseTypeSeqCache = undetBaseTypeSeq
          baseTypeSeqCache =
            if (sym.isAbstractType) transform(bounds.hi).baseTypeSeq prepend this
            else sym.info.baseTypeSeq map transform

        }
      }
      if (baseTypeSeqCache == undetBaseTypeSeq)
        throw new TypeError("illegal cyclic inheritance involving " + sym)
      baseTypeSeqCache
    }

    override def baseTypeSeqDepth: Int = baseTypeSeq.maxDepth

    override def baseClasses: List[Symbol] = thisInfo.baseClasses

    // override def isNullable: Boolean = sym.info.isNullable

    override def safeToString: String = {
      if (!settings.debug.value) {
        if (sym == RepeatedParamClass && !args.isEmpty)
          return args(0).toString + "*"
        if (sym == ByNameParamClass && !args.isEmpty)
          return "=> " + args(0).toString
        if (isFunctionType(this))
          return normalize.typeArgs.init.mkString("(", ", ", ")") + " => " + normalize.typeArgs.last
        if (isTupleType(this))
          return normalize.typeArgs.mkString("(", ", ", if (normalize.typeArgs.length == 1) ",)" else ")")
        if (sym.isAliasType && (prefixChain exists (_.termSymbol hasFlag SYNTHETIC))) {
          val normed = normalize;
          if (normed ne this) return normed.toString
        }
      }
      val monopart =
        if (!settings.debug.value &&
            (shorthands contains sym.fullNameString) &&
            (sym.ownerChain forall (_.isClass))) // ensure that symbol is not a local copy with a name coincidence
          sym.name.toString
        else
          pre.prefixString + sym.nameString

      var str = monopart + (if (args.isEmpty) "" else args.mkString("[", ",", "]"))
      //if (sym.nameString startsWith "moduleType")
      //  str += ("_in_"+sym.ownerChain)
      if (sym.isPackageClass)
        packagePrefix + str
      else if (sym.isModuleClass)
        objectPrefix + str
      else if (sym.isAnonymousClass && sym.isInitialized && !settings.debug.value)
        thisInfo.parents.mkString("", " with ", "{ ... }")
      else if (sym.isRefinementClass && sym.isInitialized)
        thisInfo.toString
      else str
    }

    override def prefixString =
      if (settings.debug.value)
        super.prefixString
      else if (sym.isRoot || sym.isEmptyPackageClass || sym.isInterpreterWrapper ||
               sym.isAnonymousClass || sym.isRefinementClass || sym.isScalaPackageClass)
        ""
      else if (sym.isPackageClass)
        sym.fullNameString + "."
      else if (isStable && (sym.name.toString endsWith ".type"))
        sym.name.toString.substring(0, sym.name.length - 4)
      else
        super.prefixString

      override def kind = "TypeRef"
  }

  /** A class representing a method type with parameters.
   */
  case class MethodType(override val params: List[Symbol],
                        override val resultType: Type) extends Type {
    override val isTrivial: Boolean =
      paramTypes.forall(_.isTrivial) && resultType.isTrivial

    //assert(paramTypes forall (pt => !pt.typeSymbol.isImplClass))//DEBUG
    override def paramSectionCount: Int = resultType.paramSectionCount + 1

    override def paramss: List[List[Symbol]] = params :: resultType.paramss

    override def paramTypes = params map (_.tpe)

    override def boundSyms = params ::: resultType.boundSyms

    override def resultType(actuals: List[Type]) = {
      val map = new InstantiateDeBruijnMap(actuals)
      val rawResTpe = map.apply(resultType)

      if (phase.erasedTypes)
        rawResTpe
      else
        existentialAbstraction(map.existentialsNeeded, rawResTpe)
    }

    override def finalResultType: Type = resultType.finalResultType

    private def dependentToString(base: Int): String = {
      val params = for ((pt, n) <- paramTypes.zipWithIndex) yield "x$"+n+":"+pt
      val res = resultType match {
        case mt: MethodType => mt.dependentToString(base + params.length)
        case rt => rt.toString
      }
      params.mkString("(", ",", ")")+res
    }

    override def safeToString: String =
      if (resultType.isDependent) dependentToString(0)
      else params.map(_.defString).mkString("(", ",", ")") + resultType

    override def cloneInfo(owner: Symbol) = {
      val vparams = cloneSymbols(params, owner)
      copyMethodType(this, vparams, resultType.substSym(params, vparams).cloneInfo(owner))
    }

    override def kind = "MethodType"
  }

  // todo: this class is no longer needed, a method type is implicit if the first
  // parameter has the IMPLICIT flag
  class ImplicitMethodType(ps: List[Symbol], rt: Type) extends MethodType(ps, rt)

  class JavaMethodType(ps: List[Symbol], rt: Type) extends MethodType(ps, rt)

  /** A class representing a polymorphic type or, if tparams.length == 0,
   *  a parameterless method type.
   *  (@M: note that polymorphic nullary methods have non-empty tparams,
   *   e.g., isInstanceOf or def makeList[T] = new List[T].
   *   Ideally, there would be a NullaryMethodType, but since the only polymorphic values are methods, it's not that problematic.
   *   More pressingly, we should add a TypeFunction type for anonymous type constructors -- for now, PolyType is used in:
   *     - normalize: for eta-expansion of type aliases
   *     - abstractTypeSig )
   */
  case class PolyType(override val typeParams: List[Symbol], override val resultType: Type)
       extends Type {

    override def paramSectionCount: Int = resultType.paramSectionCount
    override def paramss: List[List[Symbol]] = resultType.paramss
    override def params: List[Symbol] = resultType.params
    override def paramTypes: List[Type] = resultType.paramTypes
    override def parents: List[Type] = resultType.parents
    override def decls: Scope = resultType.decls
    override def termSymbol: Symbol = resultType.termSymbol
    override def typeSymbol: Symbol = resultType.typeSymbol
    override def boundSyms: List[Symbol] = typeParams ::: resultType.boundSyms
    override def prefix: Type = resultType.prefix
    override def baseTypeSeq: BaseTypeSeq = resultType.baseTypeSeq
    override def baseTypeSeqDepth: Int = resultType.baseTypeSeqDepth
    override def baseClasses: List[Symbol] = resultType.baseClasses
    override def baseType(clazz: Symbol): Type = resultType.baseType(clazz)
    override def narrow: Type = resultType.narrow
    override def isVolatile = resultType.isVolatile
    override def finalResultType: Type = resultType.finalResultType

    /** @M: abstractTypeSig now wraps a TypeBounds in a PolyType
     *  to represent a higher-kinded type parameter
     *  wrap lo&hi in polytypes to bind variables
     */
    override def bounds: TypeBounds =
      TypeBounds(PolyType(typeParams, resultType.bounds.lo),
                 PolyType(typeParams, resultType.bounds.hi))

    override def isHigherKinded = !typeParams.isEmpty

    override def safeToString: String =
      (if (typeParams.isEmpty) "=> "
       else (typeParams map (_.defString) mkString ("[", ",", "]")))+resultType

    override def cloneInfo(owner: Symbol) = {
      val tparams = cloneSymbols(typeParams, owner)
      PolyType(tparams, resultType.substSym(typeParams, tparams).cloneInfo(owner))
    }

    override def kind = "PolyType"
  }

  case class ExistentialType(quantified: List[Symbol],
                             override val underlying: Type) extends RewrappingTypeProxy
  {
    override protected def rewrap(newtp: Type) = existentialAbstraction(quantified, newtp)

    override def isTrivial = false
    override def isStable: Boolean = false
    override def bounds = TypeBounds(maybeRewrap(underlying.bounds.lo), maybeRewrap(underlying.bounds.hi))
    override def parents = underlying.parents map maybeRewrap
    override def boundSyms: List[Symbol] = quantified
    override def prefix = maybeRewrap(underlying.prefix)
    override def typeArgs = underlying.typeArgs map maybeRewrap
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

    override def skolemizeExistential(owner: Symbol, origin: AnyRef) = {
      def mkSkolem(tparam: Symbol): Symbol = {
        val skolem = new TypeSkolem(
          if (owner == NoSymbol) tparam.owner else owner,
          tparam.pos, tparam.name, origin)
        skolem.setInfo(tparam.info.cloneInfo(skolem))
              .setFlag(tparam.flags | EXISTENTIAL)
              .resetFlag(PARAM)
      }
      val skolems = quantified map mkSkolem
      for (skolem <- skolems)
        skolem setInfo skolem.info.substSym(quantified, skolems)
      underlying.substSym(quantified, skolems)
    }

    private def wildcardArgsString(available: Set[Symbol], args: List[Type]): List[String] = args match {
      case TypeRef(_, sym, _) :: args1 if (quantified contains sym) =>
        ("_"+sym.infoString(sym.info)) :: wildcardArgsString(available - sym, args1)
      case arg :: args1 if !(quantified exists (arg contains _)) =>
        arg.toString :: wildcardArgsString(available, args1)
      case _ =>
        List()
    }

    override def safeToString: String = {
      if (!(quantified exists (_.isSingletonExistential)) && !settings.debug.value)
        // try to represent with wildcards first
        underlying match {
          case TypeRef(pre, sym, args) if (!args.isEmpty) =>
            val wargs = wildcardArgsString(Predef.Set()++quantified, args)
            if (wargs.length == args.length)
              return TypeRef(pre, sym, List()).toString+wargs.mkString("[", ", ", "]")
          case _ =>
        }
      var ustr = underlying.toString
      underlying match {
        case MethodType(_, _) | PolyType(_, _) => ustr = "("+ustr+")"
        case _ =>
      }
      val str =
        ustr+(quantified map (_.existentialToString) mkString(" forSome { ", "; ", " }"))
      if (settings.explaintypes.value) "("+str+")" else str
    }

    override def cloneInfo(owner: Symbol) = {
      val tparams = cloneSymbols(quantified, owner)
      ExistentialType(tparams, underlying.substSym(quantified, tparams))
    }

    override def kind = "ExistentialType"

    def withTypeVars(op: Type => Boolean): Boolean = withTypeVars(op, AnyDepth)

    def withTypeVars(op: Type => Boolean, depth: Int): Boolean = {
      val tvars = quantified map (tparam => new TypeVar(tparam.tpe, new TypeConstraint))
      val underlying1 = underlying.instantiateTypeParams(quantified, tvars)
      op(underlying1) && {
        solve(tvars, quantified, quantified map (x => 0), false, depth) &&
        isWithinBounds(NoPrefix, NoSymbol, quantified, tvars map (_.constr.inst))
      }
    }
  }

  /** A class containing the alternatives and type prefix of an overloaded symbol.
   *  Not used after phase `typer'.
   */
  case class OverloadedType(pre: Type, alternatives: List[Symbol]) extends Type {
    override def prefix: Type = pre
    override def safeToString =
      (alternatives map pre.memberType).mkString("", " <and> ", "")
    override def kind = "OverloadedType"
  }

  /** A class remembering a type instantiation for some a set of overloaded
   *  polymorphic symbols.
   *  Not used after phase `typer'.
   */
  case class AntiPolyType(pre: Type, targs: List[Type]) extends Type {
    override def safeToString =
      pre.toString + targs.mkString("(with type arguments ", ",", ")");
    override def memberType(sym: Symbol) = pre.memberType(sym) match {
      case PolyType(tparams, restp) =>
        restp.subst(tparams, targs)
/* I don't think this is needed, as existential types close only over value types
      case ExistentialType(tparams, qtpe) =>
        existentialAbstraction(tparams, qtpe.memberType(sym))
*/
      case ErrorType =>
        ErrorType
    }
    override def kind = "AntiPolyType"
  }

  //private var tidCount = 0  //DEBUG

  /** A class representing a type variable
   *  Not used after phase `typer'.
   */
  case class TypeVar(origin: Type, constr0: TypeConstraint) extends Type {

    // var tid = { tidCount += 1; tidCount } //DEBUG

    /** The constraint associated with the variable */
    var constr = constr0

    /** The variable's skolemizatuon level */
    val level = skolemizationLevel

    override def isHigherKinded = origin.isHigherKinded

    def setInst(tp: Type) {
//      assert(!(tp containsTp this), this)
      constr.inst = tp
    }

    def tryInstantiate(tp: Type): Boolean =
      if (constr.lobounds.forall(_ <:< tp) && constr.hibounds.forall(tp <:< _)) {
        setInst(tp)
        true
      } else false

    override def typeSymbol = origin.typeSymbol
    override def safeToString: String = {
      def varString = "?"+(if (settings.explaintypes.value) level else "")+origin// +"#"+tid //DEBUG
      if (constr.inst eq null) "<null " + origin + ">"
      else if (settings.debug.value) varString+constr.toString
      else if (constr.inst eq NoType) varString
      else constr.inst.toString
    }
    override def isStable = origin.isStable
    override def isVolatile = origin.isVolatile
    override def kind = "TypeVar"
  }

  /** A type carrying some annotations. Created by the typechecker
   *  when eliminating ``Annotated'' trees (see typedAnnotated).
   *
   *  @param annotations the list of annotations on the type
   *  @param underlying the type without the annotation
   *  @param selfsym a "self" symbol with type <code>underlying</code>;
   *    only available if -Yself-in-annots is turned on. Can be NoSymbol
   *    if it is not used.
   */
  case class AnnotatedType(override val annotations: List[AnnotationInfo],
                           override val underlying: Type,
                           override val selfsym: Symbol)
  extends RewrappingTypeProxy {

    assert(!annotations.isEmpty)

    override protected def rewrap(tp: Type) = AnnotatedType(annotations, tp, selfsym)

    override def safeToString: String = {
      val attString =
        if (annotations.isEmpty)
          ""
        else
          annotations.mkString(" @", " @", "")

      underlying + attString
    }

    /** Add a number of annotations to this type */
    override def withAnnotations(annots: List[AnnotationInfo]): Type =
      AnnotatedType(annots:::this.annotations, this, selfsym)

    /** Remove any annotations from this type */
    override def withoutAnnotations = underlying.withoutAnnotations

    /** Set the self symbol */
    override def withSelfsym(sym: Symbol) =
      AnnotatedType(annotations, underlying, sym)

    /** Drop the annotations on the bounds, unless but the low and high bounds are
     *  exactly tp. */
    override def bounds: TypeBounds = {
       val oftp = underlying.bounds
       oftp match {
         case TypeBounds(lo, hi) if ((lo eq this) && (hi eq this)) => mkTypeBounds(this,this)
         case _ => oftp
       }
    }

    // ** Replace formal type parameter symbols with actual type arguments. * /
    override def instantiateTypeParams(formals: List[Symbol], actuals: List[Type]) = {
      val annotations1 = annotations.map(info => AnnotationInfo(info.atp.instantiateTypeParams(
          formals, actuals), info.args, info.assocs))
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

  /** A class representing types with a name. When an application uses
   *  named arguments, the named argument types for calling isApplicable
   *  are represented as NamedType.
   */
  case class NamedType(name: Name, tp: Type) extends Type {
    override def safeToString: String = name.toString +": "+ tp
  }

  /** A class representing an as-yet unevaluated type.
   */
  abstract class LazyType extends Type {
    override def isComplete: Boolean = false
    override def complete(sym: Symbol)
    override def safeToString = "<?>"
    override def kind = "LazyType"
  }

// Creators ---------------------------------------------------------------

  /** Rebind symbol `sym' to an overriding member in type
   *  `pre'.
   */
  private def rebind(pre: Type, sym: Symbol): Symbol = {
    val owner = sym.owner
    if (owner.isClass && owner != pre.typeSymbol && !sym.isFinal && !sym.isClass) {
      //Console.println("rebind "+pre+" "+sym)//DEBUG
      val rebind = pre.nonPrivateMember(sym.name).suchThat(sym => sym.isType || sym.isStable)
      if (rebind == NoSymbol) sym
      else {
        // Console.println("rebound "+pre+" "+sym+" to "+rebind)//DEBUG
        rebind
      }
    } else sym
  }

  /** Convert a `super' prefix to a this-type if `sym'
   *  is abstract or final.
   */
  private def removeSuper(tp: Type, sym: Symbol): Type = tp match {
    case SuperType(thistp, _) =>
      if (sym.isFinal || sym.isDeferred) thistp
      else tp
    case _ =>
      tp
  }

  /** The canonical creator for this-types */
  def mkThisType(sym: Symbol): Type = {
    class UniqueThisType extends ThisType(sym) with UniqueType
    if (phase.erasedTypes) sym.tpe else unique(new UniqueThisType)
  }

  /** The canonical creator for single-types */
  def singleType(pre: Type, sym: Symbol): Type = {
    if (phase.erasedTypes)
      sym.tpe.resultType
    else if (sym.isRootPackage)
      mkThisType(RootClass)
    else {
      var sym1 = rebind(pre, sym)
      val pre1 = removeSuper(pre, sym1)
      if (pre1 ne pre) sym1 = rebind(pre1, sym1)

      class UniqueSingleType extends SingleType(pre1, sym1) with UniqueType
      unique(new UniqueSingleType)
    }
  }

  /** The canonical creator for super-types */
  def mkSuperType(thistp: Type, supertp: Type): Type =
    if (phase.erasedTypes) supertp
    else {
      class UniqueSuperType extends SuperType(thistp, supertp) with UniqueType
      unique(new UniqueSuperType)
    }

  /** The canonical creator for type bounds */
  def mkTypeBounds(lo: Type, hi: Type): TypeBounds = {
    class UniqueTypeBounds extends TypeBounds(lo, hi) with UniqueType
    unique(new UniqueTypeBounds)
  }

  def refinementOfClass(clazz: Symbol, parents: List[Type], decls: Scope) = {
    class RefinementOfClass extends RefinedType(parents, decls) {
      override def typeSymbol: Symbol = clazz
    }
    new RefinementOfClass
  }



  /** the canonical creator for a refined type with a given scope */
  def refinedType(parents: List[Type], owner: Symbol, decls: Scope, pos : Position): Type = {
    if (phase.erasedTypes)
      if (parents.isEmpty) ObjectClass.tpe else parents.head
    else {
      val clazz = recycle(owner.newRefinementClass(NoPosition))
      val result = refinementOfClass(clazz, parents, decls)
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
    refinedType(parents, owner, newTempScope, owner.pos)

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
        sym.setInfo(sym.info.substThis(original.typeSymbol, resultThis).substSym(syms1, syms2))
      result
    }

  /** the canonical creator for a constant type */
  def mkConstantType(value: Constant): ConstantType = {
    class UniqueConstantType extends ConstantType(value) with UniqueType {
      /** Save the type of 'value'. For Java enums, it depends on finding the linked class,
       *  which might not be found after 'flatten'. */
      private lazy val _tpe: Type = value.tpe
      override def underlying: Type = _tpe
    }
    unique(new UniqueConstantType)
  }

  /** The canonical creator for typerefs
   *  todo: see how we can clean this up a bit
   */
  def typeRef(pre: Type, sym: Symbol, args: List[Type]): Type = {
    var sym1 = if (sym.isAbstractType) rebind(pre, sym) else sym
    def transform(tp: Type): Type =
      tp.resultType.asSeenFrom(pre, sym1.owner).instantiateTypeParams(sym1.typeParams, args)
    if (sym1.isAliasType && sym1.info.typeParams.length == args.length) {
      if (!sym1.lockOK)
        throw new TypeError("illegal cyclic reference involving " + sym1)
      // note: we require that object is initialized,
      // that's why we use info.typeParams instead of typeParams.
/*
      sym1.lock {
        throw new TypeError("illegal cyclic reference involving " + sym1)
      }
      transform(sym1.info) // check there are no cycles
      sym1.unlock()
*/
      rawTypeRef(pre, sym1, args) // don't expand type alias (cycles checked above)
    } else {
      val pre1 = removeSuper(pre, sym1)
      if (pre1 ne pre) {
        if (sym1.isAbstractType) sym1 = rebind(pre1, sym1)
        typeRef(pre1, sym1, args)
      }
      else if (sym1.isClass && pre.isInstanceOf[CompoundType]) {
        // sharpen prefix so that it is maximal and still contains the class.
        var p = pre.parents.reverse
        while (!p.isEmpty && p.head.member(sym1.name) != sym1) p = p.tail
        if (p.isEmpty) rawTypeRef(pre, sym1, args)
        else typeRef(p.head, sym1, args)
      } else {
        rawTypeRef(pre, sym1, args)
      }
    }
  }

  /** create a type-ref as found, without checks or rebinds */
  def rawTypeRef(pre: Type, sym: Symbol, args: List[Type]): Type = {
    class rawTypeRef extends TypeRef(pre, sym, args) with UniqueType
    unique(new rawTypeRef)
  }

  /** The canonical creator for implicit method types */
  def ImplicitMethodType(params: List[Symbol], resultType: Type): ImplicitMethodType =
    new ImplicitMethodType(params, resultType) // don't unique this!

  /** The canonical creator for implicit method types */
  def JavaMethodType(params: List[Symbol], resultType: Type): JavaMethodType =
    new JavaMethodType(params, resultType) // don't unique this!

  /** Create a new MethodType of the same class as tp, i.e. keep Java / ImplicitMethodType */
  def copyMethodType(tp: Type, params: List[Symbol], restpe: Type): Type = tp match {
    case _: ImplicitMethodType => ImplicitMethodType(params, restpe)
    case _: JavaMethodType => JavaMethodType(params, restpe)
    case _ => MethodType(params, restpe)
  }

  /** A creator for intersection type where intersections of a single type are
   *  replaced by the type itself, and repeated parent classes are merged.
   */
  def intersectionType(tps: List[Type], owner: Symbol): Type = tps match {
    case List(tp) =>
      tp
    case _ =>
       refinedType(tps, owner)
/*
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
  }

  /** A creator for intersection type where intersections of a single type are
   *  replaced by the type itself. */
  def intersectionType(tps: List[Type]): Type = tps match {
    case List(tp) => tp
    case _ => refinedType(tps, commonOwner(tps))
  }

  /** A creator for type applications */
  def appliedType(tycon: Type, args: List[Type]): Type =
    if (args.isEmpty) tycon //@M! `if (args.isEmpty) tycon' is crucial (otherwise we create new types in phases after typer and then they don't get adapted (??))
    else tycon match {
      case TypeRef(pre, sym, _) => typeRef(pre, sym, args)
      case PolyType(tparams, restpe) => restpe.instantiateTypeParams(tparams, args)
      case ExistentialType(tparams, restpe) => ExistentialType(tparams, appliedType(restpe, args))
      case ErrorType => tycon
      case st: SingletonType => appliedType(st.widen, args) // @M TODO: what to do? see bug1
      case RefinedType(parents, decls) => RefinedType(parents map (appliedType(_, args)), decls) // MO to AM: please check
      case TypeBounds(lo, hi) => TypeBounds(appliedType(lo, args), appliedType(hi, args))
      case _ => throw new Error(debugString(tycon))
    }

  /** A creator for type parameterizations
   *  If tparams is empty, simply returns result type
   */
  def polyType(tparams: List[Symbol], tpe: Type): Type =
    if (tparams.isEmpty) tpe
    else
      PolyType(tparams, tpe match {
        case PolyType(List(), tpe1) => tpe1
        case _ => tpe
      })

  /** A creator for existential types. This generates:
   *
   *  tpe1 where { tparams }
   *
   *  where `tpe1' is the result of extrapolating `tpe' wrt to `tparams'. Extrapolating means
   *  that type variables in `tparams' occurring in covariant positions are replaced by upper bounds,
   *  (minus any SingletonClass markers),
   *  type variables in `tparams' occurring in contravariant positions are replaced by upper bounds,
   *  provided the resulting type is legal wrt to stability, and does not contain any
   *  type varianble in `tparams'.
   *  The abstraction drops all type parameters that are not directly or indirectly
   *  referenced by type `tpe1'.
   *  If there are no remaining type parameters, simply returns result type `tpe'.
   */
  def existentialAbstraction(tparams: List[Symbol], tpe0: Type): Type =
    if (tparams.isEmpty) tpe0
    else {
      var occurCount = emptySymCount ++ (tparams map (_ -> 0))
      val tpe = deAlias(tpe0)
      for (t <- tpe) {
        t match {
          case TypeRef(_, sym, _) =>
            occurCount get sym match {
              case Some(count) => occurCount += (sym -> (count + 1))
              case None =>
            }
          case _ =>
        }
      }

      val extrapolate = new TypeMap {
        variance = 1
        def apply(tp: Type): Type = {
          val tp1 = mapOver(tp)
          tp1 match {
            case TypeRef(pre, sym, args) if (variance != 0) && (occurCount isDefinedAt sym) =>
              val repl = if (variance == 1) dropSingletonType(tp1.bounds.hi) else tp1.bounds.lo
              //println("eliminate "+sym+"/"+repl+"/"+occurCount(sym)+"/"+(tparams exists (repl.contains)))//DEBUG
              if (repl.typeSymbol != NothingClass && repl.typeSymbol != NullClass &&
                  occurCount(sym) == 1 && !(tparams exists (repl.contains)))
                repl
              else tp1
            case _ =>
              tp1
          }
        }
        override def mapOver(tree: Tree) =
          tree match {
            case tree:Ident
            if tree.tpe.isStable
            =>
              // Do not discard the types of existential ident's.
              // The symbol of the Ident itself cannot be listed
              // in the existential's parameters, so the
              // resulting existential type would be ill-formed.
              Some(tree)

            case _ =>
              super.mapOver(tree)
          }



      }
      val tpe1 = extrapolate(tpe)
      var tparams0 = tparams
      var tparams1 = tparams0 filter tpe1.contains

      while (tparams1 != tparams0) {
        tparams0 = tparams1
        tparams1 = tparams filter { p =>
          tparams1 exists { p1 => p1 == p || (p1.info contains p) }
        }
      }
      if (tparams1.isEmpty) tpe1
      else tpe1 match {
        case ExistentialType(tparams2, tpe2) => ExistentialType(tparams1 ::: tparams2, tpe2)
        case _ => ExistentialType(tparams1, tpe1)
      }
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
        case TypeRef(_, sym, _) if (sym == SingletonClass) =>
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

// Hash consing --------------------------------------------------------------

  var uniques: HashSet[AnyRef] = _
  private var uniqueRunId = NoRunId

  def uniqueTypeCount = uniques.size // for statistics

  private def unique[T <: AnyRef](tp: T): T = {
    if (uniqueRunId != currentRunId) {
      uniques = new HashSet(20000)
      uniqueRunId = currentRunId
    }
    uniques.findEntry(tp) match {
      case null   =>
        //println("new unique type: "+tp)
        uniques.addEntry(tp);
        tp
      case tp1    => tp1.asInstanceOf[T]
    }
  }

// Helper Classes ---------------------------------------------------------

  /** A class expressing upper and lower bounds constraints
   *  for type variables, as well as their instantiations */
  class TypeConstraint(lo: List[Type], hi: List[Type]) {
    //var self: Type = _ //DEBUG
    def this() = this(List(), List())
    var lobounds: List[Type] = lo
    var hibounds: List[Type] = hi
    var inst: Type = NoType

    def duplicate = {
      val tc = new TypeConstraint(lo, hi)
      tc.inst = inst
      tc
    }

    override def toString =
      (lobounds map (_.safeToString)).mkString("[ _>:(", ",", ") ") +
      (hibounds map (_.safeToString)).mkString("| _<:(", ",", ") ] _= ") +
      inst.safeToString
  }

  /** A prototype for mapping a function over all possible types
   */
  abstract class TypeMap extends Function1[Type, Type] {
    // deferred inherited: def apply(tp: Type): Type

    /** The variance relative to start. If you want variances to be significant, set
     *  variance = 1
     *  at the top of the typemap.
     */
    var variance = 0

    /** Should this map drop annotations that are not
     *  type-constraint annotations?
     */
    val dropNonConstraintAnnotations = false

    /** Check whether two lists have elements that are eq-equal */
    def allEq[T <: AnyRef](l1: List[T], l2: List[T]): Boolean =
      (l1, l2) match {
        case (Nil, Nil) => true
        case (hd1::tl1, hd2::tl2) =>
          if (!(hd1 eq hd2))
            return false
          allEq(tl1, tl2)
        case _ => false
      }

    /** Map this function over given type */
    def mapOver(tp: Type): Type = tp match {
      case ErrorType => tp
      case WildcardType => tp
      case NoType => tp
      case NoPrefix => tp
      case ThisType(_) => tp
      case ConstantType(_) => tp
      case DeBruijnIndex(_, _) => tp
      case SingleType(pre, sym) =>
        if (sym.isPackageClass) tp // short path
        else {
          val pre1 = this(pre)
          if (pre1 eq pre) tp
          else singleType(pre1, sym)
        }
      case SuperType(thistp, supertp) =>
        val thistp1 = this(thistp)
        val supertp1 = this(supertp)
        if ((thistp1 eq thistp) && (supertp1 eq supertp)) tp
        else mkSuperType(thistp1, supertp1)
      case TypeRef(pre, sym, args) =>
        val pre1 = this(pre)
        //val args1 = args mapConserve this(_)
        val args1 = if (args.isEmpty) args
                    else {
                      val tparams = sym.typeParams
                      if (tparams.isEmpty) args
                      else mapOverArgs(args, tparams)
                    }
        if ((pre1 eq pre) && (args1 eq args)) tp
        else typeRef(pre1, sym, args1)
      case TypeBounds(lo, hi) =>
        variance = -variance
        val lo1 = this(lo)
        variance = -variance
        val hi1 = this(hi)
        if ((lo1 eq lo) && (hi1 eq hi)) tp
        else mkTypeBounds(lo1, hi1)
      case BoundedWildcardType(bounds) =>
        val bounds1 = this(bounds)
        if (bounds1 eq bounds) tp
        else BoundedWildcardType(bounds1.asInstanceOf[TypeBounds])
      case rtp @ RefinedType(parents, decls) =>
        val parents1 = List.mapConserve(parents)(this)
        val decls1 = mapOver(decls)
        //if ((parents1 eq parents) && (decls1 eq decls)) tp
        //else refinementOfClass(tp.typeSymbol, parents1, decls1)
        copyRefinedType(rtp, parents1, decls1)
/*
      case ClassInfoType(parents, decls, clazz) =>
        val parents1 = List.mapConserve(parents)(this);
        val decls1 = mapOver(decls);
        if ((parents1 eq parents) && (decls1 eq decls)) tp
        else cloneDecls(ClassInfoType(parents1, new Scope(), clazz), tp, decls1)
*/
      case MethodType(params, result) =>
        variance = -variance
        val params1 = mapOver(params)
        variance = -variance
        val result1 = this(result)
        if ((params1 eq params) && (result1 eq result)) tp
        // for new dependent types: result1.substSym(params, params1)?
        else copyMethodType(tp, params1, result1.substSym(params, params1))
      case PolyType(tparams, result) =>
        variance = -variance
        val tparams1 = mapOver(tparams)
        variance = -variance
        var result1 = this(result)
        if ((tparams1 eq tparams) && (result1 eq result)) tp
        else PolyType(tparams1, result1.substSym(tparams, tparams1))
      case ExistentialType(tparams, result) =>
        val tparams1 = mapOver(tparams)
        var result1 = this(result)
        if ((tparams1 eq tparams) && (result1 eq result)) tp
        else ExistentialType(tparams1, result1.substSym(tparams, tparams1))
      case OverloadedType(pre, alts) =>
        val pre1 = if (pre.isInstanceOf[ClassInfoType]) pre else this(pre)
        if (pre1 eq pre) tp
        else OverloadedType(pre1, alts)
      case AntiPolyType(pre, args) =>
        val pre1 = this(pre)
        val args1 = List.mapConserve(args)(this)
        if ((pre1 eq pre) && (args1 eq args)) tp
        else AntiPolyType(pre1, args1)
      case TypeVar(_, constr) =>
        if (constr.inst != NoType) this(constr.inst)
        else tp
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
      case _ =>
        tp
        // throw new Error("mapOver inapplicable for " + tp);
    }

    def mapOverArgs(args: List[Type], tparams: List[Symbol]): List[Type] =
      map2Conserve(args, tparams) { (arg, tparam) =>
        val v = variance
        if (tparam.isContravariant) variance = -variance
        else if (!tparam.isCovariant) variance = 0
        val arg1 = this(arg)
        variance = v
        arg1
      }

    /** Map this function over given scope */
    private def mapOver(scope: Scope): Scope = {
      val elems = scope.toList
      val elems1 = mapOver(elems)
      if (elems1 eq elems) scope
      else newScope(elems1)
    }

    /** Map this function over given list of symbols */
    def mapOver(origSyms: List[Symbol]): List[Symbol] = {
      val change = origSyms exists { sym =>
        val v = variance
        if (sym.isAliasType) variance = 0
        val result = this(sym.info)
        variance = v
        result ne sym.info
      }
      if (!change) origSyms // fast path in case nothing changes due to map
      else { // map is not the identity --> do cloning properly
        val clonedSyms = origSyms map (_.cloneSymbol)
        val clonedInfos = clonedSyms map (_.info.substSym(origSyms, clonedSyms))
        val transformedInfos = List.mapConserve(clonedInfos)(this)
        List.map2(clonedSyms, transformedInfos) {
          ((newSym, newInfo) => newSym.setInfo(newInfo))
        }
        clonedSyms
      }
    }


    def mapOverAnnotations(annots: List[AnnotationInfo])
    : List[AnnotationInfo] = {
      val newAnnots = annots.flatMap(mapOver(_))
      if (allEq(newAnnots, annots))
        annots
      else
        newAnnots
    }


    def mapOver(annot: AnnotationInfo): Option[AnnotationInfo] = {
      val AnnotationInfo(atp, args, assocs) = annot

      if (dropNonConstraintAnnotations &&
          !(atp.typeSymbol isNonBottomSubClass TypeConstraintClass))
        return None

      val atp1 = mapOver(atp)
      val args1 = mapOverAnnotArgs(args)
      // there is no need to rewrite assocs, as they are constants

      if ((args eq args1) && (atp eq atp1))
        Some(annot)
      else if (args1.length == args.length)
        Some(AnnotationInfo(atp1, args1, assocs))
      else
        None
    }

    /** Map over a set of annotation arguments.  If any
     *  of the arguments cannot be mapped, then return Nil.  */
    def mapOverAnnotArgs(args: List[Tree]): List[Tree] = {
      val args1 = args.flatMap(mapOver(_))
      if (args1.length != args.length)
        Nil
      else if (allEq(args, args1))
        args
      else
        args1
    }

    def mapOver(tree: Tree): Option[Tree] =
      Some(mapOver(tree, ()=>return None))

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

  /** A type map that always returns the input type unchanged */
  object IdentityTypeMap extends TypeMap {
    def apply(tp: Type) = tp
  }

  abstract class TypeTraverser extends TypeMap {
    def traverse(tp: Type): Unit
    def apply(tp: Type): Type = { traverse(tp); tp }
  }

  abstract class TypeCollector[T](initial: T) extends TypeTraverser {
    var result: T = _
    def collect(tp: Type) = {
      result = initial
      traverse(tp)
      result
    }
  }

  private val emptySymMap = scala.collection.immutable.Map[Symbol, Symbol]()
  private val emptySymCount = scala.collection.immutable.Map[Symbol, Int]()

  /** Make an existential variable.
   *  @param suffix  A suffix to be appended to the freshly generated name
   *                 It's ususally "", except for type variables abstracting
   *                 over values, where it is ".type".
   *  @param owner   The owner of the variable
   *  @param bounds  The variable's bounds
   */
  def makeExistential(name: String, owner: Symbol, bounds: TypeBounds): Symbol =
    recycle(
      owner.newAbstractType(owner.pos, newTypeName(name)).setFlag(EXISTENTIAL)
    ).setInfo(bounds)

  /** Make an existential variable with a fresh name. */
  def makeFreshExistential(suffix: String, owner: Symbol, bounds: TypeBounds): Symbol =
    makeExistential(freshName()+suffix, owner, bounds)

  def typeParamsToExistentials(clazz: Symbol, tparams: List[Symbol]): List[Symbol] = {
    val eparams = for ((tparam, i) <- tparams.zipWithIndex) yield {
      makeExistential("?"+i, clazz, tparam.info.bounds)
    }
    for (tparam <- eparams) tparam setInfo tparam.info.substSym(tparams, eparams)
    eparams
  }

  def isRaw(sym: Symbol, args: List[Type]) =
    !phase.erasedTypes && !sym.typeParams.isEmpty && sym.hasFlag(JAVA) && args.isEmpty
      //  note: it's important to write the two first tests in this order,
      //  as only typeParams forces the classfile to be read. See #400

  /** Is type tp a ``raw type''? */
  def isRawType(tp: Type) = tp match {
    case TypeRef(_, sym, args) => isRaw(sym, args)
    case _ => false
  }

  /** The raw to existential map converts a ``raw type'' to an existential type.
   *  It is necessary because we might have read a raw type of a
   *  parameterized Java class from a class file. At the time we read the type
   *  the corresponding class file might still not be read, so we do not
   *  know what the type parameters of the type are. Therefore
   *  the conversion of raw types to existential types might not have taken place
   *  in ClassFileparser.sigToType (where it is usually done)
   */
  object rawToExistential extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case TypeRef(pre, sym, List()) if !sym.typeParams.isEmpty && sym.hasFlag(JAVA) =>
        //  note: it's important to write the two tests in this order,
        //  as only typeParams forces the classfile to be read. See #400
        val eparams = typeParamsToExistentials(sym, sym.typeParams)
        existentialAbstraction(eparams, TypeRef(pre, sym, eparams map (_.tpe)))
      case _ =>
        mapOver(tp)
    }
  }

  def singletonBounds(hi: Type) = {
    mkTypeBounds(NothingClass.tpe, intersectionType(List(hi, SingletonClass.tpe)))
  }

  /** A map to compute the asSeenFrom method  */
  class AsSeenFromMap(pre: Type, clazz: Symbol) extends TypeMap {
    override val dropNonConstraintAnnotations = true

    var capturedParams: List[Symbol] = List()

    override def mapOver(tree: Tree, giveup: ()=>Nothing): Tree = {
      object annotationArgRewriter extends TypeMapTransformer {
        /** Rewrite "this" trees as needed for asSeenFrom */
        def rewriteThis(tree: Tree): Tree =
          tree match {
            case This(_)
            if (tree.symbol isNonBottomSubClass clazz) &&
               (pre.widen.typeSymbol isNonBottomSubClass tree.symbol) =>
              if (pre.isStable) {
                val termSym =
                  pre.typeSymbol.owner.newValue(
                    pre.typeSymbol.pos,
                    pre.typeSymbol.name).setInfo(pre)  // what symbol should really be used?
                mkAttributedQualifier(pre, termSym)
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

    var capturedPre = emptySymMap

    def stabilize(pre: Type, clazz: Symbol): Type = {
      capturedPre get clazz match {
        case None =>
          val qvar = makeFreshExistential(".type", clazz, singletonBounds(pre))
          capturedPre += (clazz -> qvar)
          capturedParams = qvar :: capturedParams
          qvar
        case Some(qvar) =>
          qvar
      }
    }.tpe

    /** Return pre.baseType(clazz), or if that's NoType and clazz is a refinement, pre itself.
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
              toPrefix(base(pre, clazz).prefix, clazz.owner);
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
        case TypeRef(prefix, sym, args) if (sym.isTypeParameter) =>
          def toInstance(pre: Type, clazz: Symbol): Type =
            if ((pre eq NoType) || (pre eq NoPrefix) || !clazz.isClass) mapOver(tp)
            //@M! see test pos/tcpoly_return_overriding.scala why mapOver is necessary
            else {
              def throwError : Nothing = throw new Error(
                "" + tp + sym.locationString + " cannot be instantiated from " + pre.widen
              )

              def instParam(ps: List[Symbol], as: List[Type]): Type =
                if (ps.isEmpty) throwError
                else if (sym eq ps.head)
                  // @M! don't just replace the whole thing, might be followed by type application
                  appliedType(as.head, List.mapConserve(args)(this)) // @M: was as.head
                else instParam(ps.tail, as.tail);
              val symclazz = sym.owner
              if (symclazz == clazz && (pre.widen.typeSymbol isNonBottomSubClass symclazz)) {
                pre.baseType(symclazz) match {
                  case TypeRef(_, basesym, baseargs) =>
                    //Console.println("instantiating " + sym + " from " + basesym + " with " + basesym.typeParams + " and " + baseargs+", pre = "+pre+", symclazz = "+symclazz);//DEBUG
                    if (basesym.typeParams.length == baseargs.length) {
                      instParam(basesym.typeParams, baseargs)
                    } else {
                      throw new TypeError(
                        "something is wrong (wrong class file?): "+basesym+
                        " with type parameters "+
                        basesym.typeParams.map(_.name).mkString("[",",","]")+
                        " gets applied to arguments "+baseargs.mkString("[",",","]")+", phase = "+phase)
                    }
                  case ExistentialType(tparams, qtpe) =>
                    capturedParams = capturedParams union tparams
                    toInstance(qtpe, clazz)
                  case _ =>
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
    /** Are `sym' and `sym1' the same.
     *  Can be tuned by subclasses.
     */
    protected def matches(sym: Symbol, sym1: Symbol): Boolean = sym eq sym1

    /** Map target to type, can be tuned by subclasses */
    protected def toType(fromtp: Type, t: T): Type

      def subst(tp: Type, sym: Symbol, from: List[Symbol], to: List[T]): Type =
        if (from.isEmpty) tp
        else if (matches(from.head, sym)) toType(tp, to.head)
        else subst(tp, sym, from.tail, to.tail)

    private def renameBoundSyms(tp: Type): Type = tp match {
      case MethodType(ps, restp) =>
        val ps1 = cloneSymbols(ps)
        copyMethodType(tp, ps1, renameBoundSyms(restp.substSym(ps, ps1)))
      case PolyType(bs, restp) =>
        val bs1 = cloneSymbols(bs)
        PolyType(bs1, renameBoundSyms(restp.substSym(bs, bs1)))
      case ExistentialType(bs, restp) =>
        val bs1 = cloneSymbols(bs)
        ExistentialType(bs1, restp.substSym(bs, bs1))
      case _ =>
        tp
    }

    def apply(tp0: Type): Type = if (from.isEmpty) tp0 else {
      val boundSyms = tp0.boundSyms
      val tp1 = if (boundSyms.isEmpty || !(boundSyms exists (from contains))) tp0
                else renameBoundSyms(tp0)
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
          subst(tp, sym, from, to) match {
            case r @ TypeRef(pre1, sym1, args1) =>
              if (args.isEmpty) r
              else rawTypeRef(pre1, sym1, args)
            case r =>
              r
          }
        case SingleType(NoPrefix, sym) =>
          subst(tp, sym, from, to)
        case _ =>
          tp
      }
    }
  }

  /** A map to implement the `substSym' method. */
  class SubstSymMap(from: List[Symbol], to: List[Symbol])
  extends SubstMap(from, to) {
    protected def toType(fromtp: Type, sym: Symbol) = fromtp match {
      case TypeRef(pre, _, args) => typeRef(pre, sym, args)
      case SingleType(pre, _) => singleType(pre, sym)
    }
    override def apply(tp: Type): Type = if (from.isEmpty) tp else {
      def subst(sym: Symbol, from: List[Symbol], to: List[Symbol]): Symbol =
        if (from.isEmpty) sym
        else if (matches(from.head, sym)) to.head
        else subst(sym, from.tail, to.tail)
      tp match {
        case TypeRef(pre, sym, args) if !(pre eq NoPrefix) =>
          mapOver(typeRef(pre, subst(sym, from, to), args))
            //@M TODO subst args? List.mapConserve(args)(this)
        case SingleType(pre, sym) if !(pre eq NoPrefix) =>
          mapOver(singleType(pre, subst(sym, from, to)))
        case _ =>
          super.apply(tp)
      }
    }


    override def mapOver(tree: Tree, giveup: ()=>Nothing): Tree = {
      object trans extends TypeMapTransformer {

        def termMapsTo(sym: Symbol) =
          if (from contains sym)
            Some(to(from.indexOf(sym)))
          else
            None

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
                case None => super.transform(tree)
              }
            case tree => super.transform(tree)
          }
      }
      trans.transform(tree)
    }
  }

  /** A map to implement the `subst' method. */
  class SubstTypeMap(from: List[Symbol], to: List[Type])
  extends SubstMap(from, to) {
    protected def toType(fromtp: Type, tp: Type) = tp


    override def mapOver(tree: Tree, giveup: ()=>Nothing): Tree = {
      object trans extends TypeMapTransformer {
        override def transform(tree: Tree) =
          tree match {
            case tree@Ident(_) if from contains tree.symbol =>
              val totpe = to(from.indexOf(tree.symbol))
              if (!totpe.isStable) {
                giveup()
              } else {
                tree.duplicate.setType(totpe)
              }

            case _ => super.transform(tree)
          }
      }
      trans.transform(tree)
      }


  }

  /** A map to implement the `substThis' method. */
  class SubstThisMap(from: Symbol, to: Type) extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case ThisType(sym) if (sym == from) => to
      case _ => mapOver(tp)
    }
  }

  class SubstSuperMap(from: Type, to: Type) extends TypeMap {
    def apply(tp: Type): Type = if (tp eq from) to else mapOver(tp)
  }

  class SubstWildcardMap(from: List[Symbol]) extends TypeMap {
    def apply(tp: Type): Type = try {
      tp match {
        case TypeRef(_, sym, _) if (from contains sym) => WildcardType
        case _ => mapOver(tp)
      }
    } catch {
      case ex: MalformedType =>
        WildcardType
    }
  }

  /** Most of the implementation for MethodType.resultType.  The
   *  caller also needs to existentially quantify over the
   *  variables in existentialsNeeded.
   */
  class InstantiateDeBruijnMap(actuals: List[Type]) extends TypeMap {
    override val dropNonConstraintAnnotations = true


    private var existSyms = immutable.Map.empty[Int, Symbol]
    def existentialsNeeded: List[Symbol] = existSyms.values.toList

    /* Return the type symbol for referencing a parameter index
     * inside the existential quantifier.  */
    def existSymFor(actualIdx: Int, oldSym: Symbol) =
      if (existSyms.isDefinedAt(actualIdx))
        existSyms(actualIdx)
      else {
        val symowner = oldSym.owner // what should be used??
        val bound = singletonBounds(actuals(actualIdx))

        val sym =
          symowner.newAbstractType(oldSym.pos, oldSym.name+".type")

        sym.setInfo(bound)
        sym.setFlag(oldSym.flags)
        sym.setFlag(EXISTENTIAL)


        existSyms = existSyms + (actualIdx -> sym)
        sym
      }

    def apply(tp: Type): Type = tp match {
      case DeBruijnIndex(level, pid) =>
        if (level == 1)
          if (pid < actuals.length) actuals(pid) else tp
        else DeBruijnIndex(level - 1, pid)
      case _ =>
        mapOver(tp)
    }

    override def mapOver(arg: Tree, giveup: ()=>Nothing): Tree = {
      object treeTrans extends TypeMapTransformer {
        override def transform(tree: Tree): Tree =
          tree match {
            case tree@Ident(name) =>
              tree.tpe.withoutAnnotations match {
                case DeBruijnIndex(level, pid) =>
                  if (level == 1) {
                    if (actuals(pid).isStable)
                      mkAttributedQualifier(actuals(pid), tree.symbol)
                    else {
                      val sym = existSymFor(pid, tree.symbol)
                      (Ident(tree.symbol.name)
                       copyAttrs tree
                       setType typeRef(NoPrefix, sym, Nil))
                    }
                  } else
                    tree.duplicate.setType(
                      DeBruijnIndex(level-1, pid))
                case _ => super.transform(tree)

              }
            case _ => super.transform(tree)
          }
      }

      treeTrans.transform(arg)
    }
  }

  object ApproximateDeBruijnMap extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case DeBruijnIndex(level, pid) =>
        WildcardType
      case _ =>
        mapOver(tp)
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
        TypeVar(tp, new TypeConstraint(List(bounds.lo), List(bounds.hi)))
      case _ =>
        mapOver(tp)
    }
  }

  /** A map to convert every occurrence of a type variable to a
      wildcard type */
  object typeVarToOriginMap extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case TypeVar(origin, _) => origin
      case _ => mapOver(tp)
    }
  }

  /** A map to implement the `contains' method */
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
      Some(arg)
    }
  }

  /** A map to implement the `contains' method */
  class ContainsTypeCollector(t: Type) extends TypeCollector(false) {
    def traverse(tp: Type) {
      if (!result) {
        if (tp eq t) result = true
        else mapOver(tp)
      }
    }
    override def mapOver(arg: Tree) = {
      for (t <- arg) {
        traverse(t.tpe)
      }
      Some(arg)
    }
  }

  /** A map to implement the `filter' method */
  class FilterTypeCollector(p: Type => Boolean) extends TypeCollector(new ListBuffer[Type]) {
    def traverse(tp: Type) {
      if (p(tp)) result += tp
      mapOver(tp)
    }
  }

  class ForEachTypeTraverser(f: Type => Unit) extends TypeTraverser {
    def traverse(tp: Type) {
      f(tp)
      mapOver(tp)
    }
  }

  /** A map to implement the `filter' method */
  class FindTypeCollector(p: Type => Boolean) extends TypeCollector[Option[Type]](None) {
    def traverse(tp: Type) {
      if (result.isEmpty) {
        if (p(tp)) result = Some(tp)
        mapOver(tp)
      }
    }
  }

  /** A map to implement the `contains' method */
  object ErroneousCollector extends TypeCollector(false) {
    def traverse(tp: Type) {
      if (!result) {
        result = tp.isError
        mapOver(tp)
      }
    }
  }

  object IsDependentCollector extends TypeCollector(false) {
    def traverse(tp: Type) {
      tp match {
        case DeBruijnIndex(_, _) => result = true
        case _ => if (!result) mapOver(tp)
      }
    }
  }

  /** A map to compute the most deeply nested owner that contains all the symbols
   *  of thistype or prefixless typerefs/singletype occurrences in given type.
   */
  object commonOwnerMap extends TypeMap {
    var result: Symbol = _
    def init = { result = NoSymbol }
    def apply(tp: Type): Type = {
      assert(tp ne null)
      tp.normalize match {
        case ThisType(sym) =>
          register(sym)
        case TypeRef(NoPrefix, sym, args) =>
          register(sym.owner); args foreach {arg => apply(arg); ()}
        case SingleType(NoPrefix, sym) =>
          register(sym.owner)
        case _ =>
          mapOver(tp)
      }
      tp
    }
    private def register(sym: Symbol) {
      while (result != NoSymbol && sym != result && !(sym isNestedIn result))
        result = result.owner;
    }
  }

  object adaptToNewRunMap extends TypeMap {
    private def adaptToNewRun(pre: Type, sym: Symbol): Symbol = {
      if (sym.isModuleClass && !phase.flatClasses) {
        adaptToNewRun(pre, sym.sourceModule).moduleClass
      } else if ((pre eq NoPrefix) || (pre eq NoType) || sym.owner.isPackageClass) {
        sym
      } else {
        var rebind0 = pre.findMember(sym.name, BRIDGE, 0, true)(NoSymbol)
        if (rebind0 == NoSymbol) {
          assert(false, ""+pre+"."+sym+" does no longer exist, phase = "+phase) }
        /** The two symbols have the same fully qualified name */
        def corresponds(sym1: Symbol, sym2: Symbol): Boolean =
          sym1.name == sym2.name && (sym1.isPackageClass || corresponds(sym1.owner, sym2.owner))
        if (!corresponds(sym.owner, rebind0.owner)) {
          if (settings.debug.value) Console.println("ADAPT1 pre = "+pre+", sym = "+sym+sym.locationString+", rebind = "+rebind0+rebind0.locationString)
          val bcs = pre.baseClasses.dropWhile(bc => !corresponds(bc, sym.owner));
          if (bcs.isEmpty)
            assert(pre.typeSymbol.isRefinementClass, pre) // if pre is a refinementclass it might be a structural type => OK to leave it in.
          else
            rebind0 = pre.baseType(bcs.head).member(sym.name)
          if (settings.debug.value) Console.println("ADAPT2 pre = "+pre+", bcs.head = "+bcs.head+", sym = "+sym+sym.locationString+", rebind = "+rebind0+(if (rebind0 == NoSymbol) "" else rebind0.locationString))
        }
        val rebind = rebind0.suchThat(sym => sym.isType || sym.isStable)
        if (rebind == NoSymbol) {
          if (settings.debug.value) Console.println("" + phase + " " +phase.flatClasses+sym.owner+sym.name+" "+sym.isType)
          throw new MalformedType(pre, sym.nameString)
        }
        rebind
      }
    }
    def apply(tp: Type): Type = tp match {
      case ThisType(sym) if (sym.isModuleClass) =>
        val sym1 = adaptToNewRun(sym.owner.thisType, sym)
        if (sym1 == sym) tp else mkThisType(sym1)
      case SingleType(pre, sym) =>
        if (sym.isPackage) tp
        else {
          val pre1 = this(pre)
          val sym1 = adaptToNewRun(pre1, sym)
          if ((pre1 eq pre) && (sym1 eq sym)) tp
          else singleType(pre1, sym1)
        }
      case TypeRef(pre, sym, args) =>
        if (sym.isPackageClass) tp
        else {
          val pre1 = this(pre)
          val args1 = List.mapConserve(args)(this)
          val sym1 = adaptToNewRun(pre1, sym)
          if ((pre1 eq pre) && (sym1 eq sym) && (args1 eq args)/* && sym.isExternal*/) tp
          else typeRef(pre1, sym1, args1)
        }
      case MethodType(params, restp) =>
        val restp1 = this(restp)
        if (restp1 eq restp) tp
        else copyMethodType(tp, params, restp1)
      case PolyType(tparams, restp) =>
        val restp1 = this(restp)
        if (restp1 eq restp) tp
        else PolyType(tparams, restp1)

      // Lukas: we need to check (together) whether we should also include parameter types
      // of PolyType and MethodType in adaptToNewRun

      case ClassInfoType(parents, decls, clazz) =>
        if (clazz.isPackageClass) tp
        else {
          val parents1 = List.mapConserve(parents)(this)
          if (parents1 eq parents) tp
          else ClassInfoType(parents1, decls, clazz)
        }
      case RefinedType(parents, decls) =>
        val parents1 = List.mapConserve(parents)(this)
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
        (tp1 =:= stp.tp1) && (tp2 =:= stp.tp2)
      case _ =>
        false
    }
    override def toString = tp1+" <:<? "+tp2
  }

// Helper Methods  -------------------------------------------------------------

  private var nextid = 0
  private def freshName() = {
    nextid += 1
    "_"+nextid
  }

  final val LubGlbMargin = 0

  /** The maximum allowable depth of lubs or glbs over types `ts'
    * This is the maximum depth of all types in the base type sequences
    * of each of the types `ts', plus LubGlbMargin
    */
  def lubDepth(ts: List[Type]) = {
    var d = 0
    for (tp <- ts) d = Math.max(d, tp.baseTypeSeqDepth)
    d + LubGlbMargin
  }

  final def isValid(period: Period): Boolean =
    period != 0 && runId(period) == currentRunId && {
      val pid = phaseId(period)
      if (phase.id > pid) infoTransformers.nextFrom(pid).pid >= phase.id
      else infoTransformers.nextFrom(phase.id).pid >= pid
    }

  final def isValidForBaseClasses(period: Period): Boolean = {
    def noChangeInBaseClasses(it: InfoTransformer, limit: Phase#Id): Boolean = (
      it.pid >= limit ||
      !it.changesBaseClasses && noChangeInBaseClasses(it.next, limit)
    );
    period != 0 && runId(period) == currentRunId && {
      val pid = phaseId(period)
      if (phase.id > pid) noChangeInBaseClasses(infoTransformers.nextFrom(pid), phase.id)
      else noChangeInBaseClasses(infoTransformers.nextFrom(phase.id), pid)
    }
  }

  /** Can variable `tv' be related in a constraint to type `tp'?
   *  This is not the case if `tp' contains type skolems whose
   *  skolemization level is higher than the level of `tv'.
   */
  private def isRelatable(tv: TypeVar, tp: Type): Boolean = {
    var ok = true
    for (t <- tp) {
      t.typeSymbol match {
        case ts: TypeSkolem => if (ts.level > tv.level) ok = false
        case _ =>
      }
    }
    if (ok) undoLog = (tv, tv.constr.duplicate) :: undoLog
    ok
  }

  /** Is intersection of given types populated? That is,
   *  for all types tp1, tp2 in intersection
   *    for all common base classes bc of tp1 and tp2
   *      let bt1, bt2 be the base types of tp1, tp2 relative to class bc
   *      Then:
   *        bt1 and bt2 have the same prefix, and
   *        any correspondiong non-variant type arguments of bt1 and bt2 are the same
   */
  def isPopulated(tp1: Type, tp2: Type): Boolean = {
    def isConsistent(tp1: Type, tp2: Type): Boolean = (tp1, tp2) match {
      case (TypeRef(pre1, sym1, args1), TypeRef(pre2, sym2, args2)) =>
        assert(sym1 == sym2)
        pre1 =:= pre2 &&
        !(List.map3(args1, args2, sym1.typeParams) {
          (arg1, arg2, tparam) =>
            //if (tparam.variance == 0 && !(arg1 =:= arg2)) Console.println("inconsistent: "+arg1+"!="+arg2)//DEBUG
          if (tparam.variance == 0) arg1 =:= arg2
          else if (arg1.isInstanceOf[TypeVar])
            // if left-hand argument is a typevar, make it compatible with variance
            // this is for more precise pattern matching
            // todo: work this in the spec of this method
            // also: think what happens if there are embedded typevars?
            if (tparam.variance < 0) arg1 <:< arg2 else arg2 <:< arg1
          else true
        } contains false)
      case (et: ExistentialType, _) =>
        et.withTypeVars(isConsistent(_, tp2))
      case (_, et: ExistentialType) =>
        et.withTypeVars(isConsistent(tp1, _))
    }
    if (tp1.typeSymbol.isClass && tp1.typeSymbol.hasFlag(FINAL))
      tp1 <:< tp2 || isNumericValueClass(tp1.typeSymbol) && isNumericValueClass(tp2.typeSymbol)
    else tp1.baseClasses forall (bc =>
      tp2.baseTypeIndex(bc) < 0 || isConsistent(tp1.baseType(bc), tp2.baseType(bc)))
  }

  /** Does a pattern of type `patType' need an outer test when executed against
   *  selector type `selType' in context defined by `currentOwner'?
   */
  def needsOuterTest(patType: Type, selType: Type, currentOwner: Symbol) = {
    def createDummyClone(pre: Type): Type = {
      val dummy = currentOwner.enclClass.newValue(NoPosition, nme.ANYNAME).setInfo(pre.widen)
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
        (pre1 ne NoType) && isPopulated(typeRef(pre1, sym, args), selType)
      case _ =>
        false
    }
  }

  /** Undo all changes to constraints to type variables upto `limit'
   */
  private def undoTo(limit: UndoLog) {
    while (undoLog ne limit)/* && !undoLog.isEmpty*/ { // @M added `&& !undoLog.isEmpty`
                    // Martin: I don't think the addition is necessary?
// @M TODO: I had an example, but seem to have misplaced it :-)
      val (tv, constr) = undoLog.head
      undoLog = undoLog.tail
      tv.constr = constr
    }
  }

  private var subsametypeRecursions: Int = 0

  private def isUnifiable(pre1: Type, pre2: Type) =
    (beginsWithTypeVar(pre1) || beginsWithTypeVar(pre2)) && (pre1 =:= pre2)

  private def equalSymsAndPrefixes(sym1: Symbol, pre1: Type, sym2: Symbol, pre2: Type): Boolean =
    if (sym1 == sym2) phase.erasedTypes || pre1 =:= pre2
    else (sym1.name == sym2.name) && isUnifiable(pre1, pre2)

  /** Do `tp1' and `tp2' denote equivalent types?
   */
  def isSameType(tp1: Type, tp2: Type): Boolean = try {
    subsametypeRecursions += 1
    val lastUndoLog = undoLog
    val result = isSameType0(tp1, tp2)
    sametypeCount += 1
    if (!result) undoTo(lastUndoLog)
    result
  } finally {
    subsametypeRecursions -= 1
    if (subsametypeRecursions == 0) undoLog = List()
  }

  def isDifferentType(tp1: Type, tp2: Type): Boolean = try {
    subsametypeRecursions += 1
    val lastUndoLog = undoLog
    val result = isSameType0(tp1, tp2)
    undoTo(lastUndoLog)
    !result
  } finally {
    subsametypeRecursions -= 1
    if (subsametypeRecursions == 0) undoLog = List()
  }

  def normalizePlus(tp: Type) =
    if (isRawType(tp)) rawToExistential(tp)
    else tp.normalize

  /*
  todo: change to:
  def normalizePlus(tp: Type) = tp match {
    case TypeRef(pre, sym, List()) =>
      if (!sym.isInitialized) sym.rawInfo.load(sym)
      if (sym.hasFlag(JAVA) && !sym.typeParams.isEmpty) rawToExistential(tp)
      else tp.normalize
    case _ => tp.normalize
  }
  */

  private def isSameType0(tp1: Type, tp2: Type): Boolean =
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
      if equalSymsAndPrefixes(sym1, pre1, sym2, pre2) =>
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
         tp1.isInstanceOf[ImplicitMethodType] == tp2.isInstanceOf[ImplicitMethodType])
      case (PolyType(tparams1, res1), PolyType(tparams2, res2)) =>
        (tparams1.length == tparams2.length &&
         List.forall2(tparams1, tparams2)
           ((p1, p2) => p1.info =:= p2.info.substSym(tparams2, tparams1)) &&
         res1 =:= res2.substSym(tparams2, tparams1))
      case (ExistentialType(tparams1, res1), ExistentialType(tparams2, res2)) =>
        (tparams1.length == tparams2.length &&
         List.forall2(tparams1, tparams2)
           ((p1, p2) => p1.info =:= p2.info.substSym(tparams2, tparams1)) &&
         res1 =:= res2.substSym(tparams2, tparams1))
      case (TypeBounds(lo1, hi1), TypeBounds(lo2, hi2)) =>
        lo1 =:= lo2 && hi1 =:= hi2
      case (BoundedWildcardType(bounds), _) =>
        bounds containsType tp2
      case (_, BoundedWildcardType(bounds)) =>
        bounds containsType tp1
      case (tv1 @ TypeVar(_, constr1), _) =>
        if (constr1.inst != NoType) constr1.inst =:= tp2
        else isRelatable(tv1, tp2) && (tv1 tryInstantiate wildcardToTypeVarMap(tp2))
      case (_, tv2 @ TypeVar(_, constr2)) =>
        if (constr2.inst != NoType) tp1 =:= constr2.inst
        else isRelatable(tv2, tp1) && (tv2 tryInstantiate wildcardToTypeVarMap(tp1))
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

  /** Are `tps1' and `tps2' lists of pairwise equivalent
   *  types?
   */
  def isSameTypes(tps1: List[Type], tps2: List[Type]): Boolean =
    tps1.length == tps2.length &&
    List.forall2(tps1, tps2)((tp1, tp2) => tp1 =:= tp2)

  private var pendingSubTypes = new collection.mutable.HashSet[SubTypePair]
  private var basetypeRecursions: Int = 0
  private var pendingBaseTypes = new collection.mutable.HashSet[Type]

  def isSubType(tp1: Type, tp2: Type): Boolean = isSubType(tp1, tp2, AnyDepth)

  def isSubType(tp1: Type, tp2: Type, depth: Int): Boolean = try {
    subsametypeRecursions += 1
    val lastUndoLog = undoLog
    val result =
      if (subsametypeRecursions >= LogPendingSubTypesThreshold) {
        val p = new SubTypePair(tp1, tp2)
        if (pendingSubTypes contains p)
          false
        else
          try {
            pendingSubTypes += p
            isSubType0(tp1, tp2, depth)
          } finally {
            pendingSubTypes -= p
          }
      } else {
        isSubType0(tp1, tp2, depth)
      }
    if (!result) undoTo(lastUndoLog)
    result
  } finally {
    subsametypeRecursions -= 1
    if (subsametypeRecursions == 0) undoLog = List()
  }

  /** Does this type have a prefix that begins with a type variable */
  def beginsWithTypeVar(tp: Type): Boolean = tp match {
    case SingleType(pre, sym) =>
      !(sym hasFlag PACKAGE) && beginsWithTypeVar(pre)
    case TypeVar(_, constr) =>
      constr.inst == NoType || beginsWithTypeVar(constr.inst)
    case _ =>
      false
  }

  def instTypeVar(tp: Type): Type = tp match {
    case TypeRef(pre, sym, args) =>
      typeRef(instTypeVar(pre), sym, args)
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

  def isHKSubType0(tp1: Type, tp2: Type, depth: Int): Boolean = (
    tp1.typeSymbol == NothingClass
    ||
    tp2.typeSymbol == AnyClass // @M Any and Nothing are super-type resp. subtype of every well-kinded type
    || // @M! normalize reduces higher-kinded case to PolyType's
    ((tp1.normalize, tp2.normalize) match {
      case (PolyType(tparams1, res1), PolyType(tparams2, res2)) =>
        tparams1.length == tparams2.length &&
        List.forall2(tparams1, tparams2) (
          (p1, p2) => p2.info.substSym(tparams2, tparams1) <:< p1.info) &&
        res1 <:< res2.substSym(tparams2, tparams1)

      case (tp1a, tp2a) =>
        (isDifferentType(tp1a, tp1) || isDifferentType(tp2a, tp2)) &&
        isSubType(tp1a, tp2a, depth)
    }))

  def isSubArgs(tps1: List[Type], tps2: List[Type], tparams: List[Symbol]): Boolean = (
    tps1.isEmpty && tps2.isEmpty
    ||
    !tps1.isEmpty && !tps2.isEmpty &&
    (tparams.head.isCovariant || (tps2.head <:< tps1.head)) &&
    (tparams.head.isContravariant || (tps1.head <:< tps2.head)) &&
    isSubArgs(tps1.tail, tps2.tail, tparams.tail)
  )

  def isSubType0(tp1: Type, tp2: Type, depth: Int): Boolean = {
    isSubType1(tp1, tp2, depth)
  }

  def differentOrNone(tp1: Type, tp2: Type) = if (tp1 eq tp2) NoType else tp1

  /** Does type `tp1' conform to `tp2'?
   */
  private def isSubType2(tp1: Type, tp2: Type, depth: Int): Boolean = {
    if (isErrorOrWildcard(tp1)) return true
    if (isErrorOrWildcard(tp2)) return true
    if (tp1 eq NoType) return false
    if (tp2 eq NoType) return false
    if (tp1 eq NoPrefix) return (tp2 eq NoPrefix) || tp2.typeSymbol.isPackageClass
    if (tp2 eq NoPrefix) return (tp1 eq NoPrefix) || tp1.typeSymbol.isPackageClass
    if (isSingleType(tp1) && isSingleType(tp2) ||
        isConstantType(tp1) && isConstantType(tp2)) return tp1 =:= tp2
    if (tp1.isHigherKinded || tp2.isHigherKinded) return isHKSubType0(tp1, tp2, depth)

    /** First try, on the right:
     *   - unwrap Annotated types, BoundedWildcardTypes,
     *   - bind TypeVars on the right, if lhs is not Annotated nor BoundedWildcard
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
            ((if (sym1 == sym2) phase.erasedTypes || pre1 <:< pre2
              else (sym1.name == sym2.name && isUnifiable(pre1, pre2))) &&
             isSubArgs(tr1.args, tr2.args, sym1.typeParams)
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
            if (constr2.inst != NoType) tp1 <:< constr2.inst
            else isRelatable(tv2, tp1) && { constr2.lobounds = tp1 :: constr2.lobounds; true }
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
      case tv1 @ TypeVar(_, constr1) =>
        if (constr1.inst != NoType) constr1.inst <:< tp2
        else isRelatable(tv1, tp2) && { constr1.hibounds = tp2 :: constr1.hibounds; true }
      case ExistentialType(_, _) =>
        try {
          skolemizationLevel += 1
          tp1.skolemizeExistential(NoSymbol, null) <:< tp2
        } finally {
          skolemizationLevel -= 1
        }
      case _ =>
        thirdTry
    }

    def thirdTryRef(tp1: Type, tp2: TypeRef): Boolean = {
      val sym2 = tp2.sym
      if (sym2.isAliasType) {
        isSubType(tp1.normalize, tp2.normalize, depth)
      } else if (sym2.isAbstractType) {
        val tp2a = tp2.bounds.lo
        isDifferentType(tp2, tp2a) && tp1 <:< tp2a || fourthTry
      } else if (sym2 == NotNullClass) {
        tp1.isNotNull
      } else if (sym2 == SingletonClass) {
        tp1.isStable
      } else if (isRaw(sym2, tp2.args)) {
        isSubType(tp1, rawToExistential(tp2), depth)
      } else if (sym2.isRefinementClass) {
        isSubType(tp1, sym2.info, depth)
      } else {
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
      case RefinedType(parents2, ref2) =>
        (parents2 forall (tp1 <:< _)) &&
        (ref2.toList forall tp1.specializes)
      case et: ExistentialType =>
        et.withTypeVars(tp1 <:< _, depth) || fourthTry
      case NotNullType(ntp2) =>
        tp1.isNotNull && tp1 <:< ntp2
      case MethodType(params2, res2) =>
        tp1 match {
          case MethodType(params1, res1) =>
            (params1.length == params2.length &&
             matchingParams(tp1.paramTypes, tp2.paramTypes, tp1.isInstanceOf[JavaMethodType], tp2.isInstanceOf[JavaMethodType]) &&
             (res1 <:< res2) &&
             tp1.isInstanceOf[ImplicitMethodType] == tp2.isInstanceOf[ImplicitMethodType])
          case _ =>
            false
        }
      case PolyType(List(), res2) =>
        tp1 match {
          case PolyType(List(), res1) =>
            // other polytypes were already checked in isHKSubType
            res1 <:< res2
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
      case TypeRef(pre1, sym1, args1) =>
        if (sym1.isAliasType) {
          isSubType(tp1.normalize, tp2.normalize, depth)
        } else if (sym1.isAbstractType) {
          val tp1a = tp1.bounds.hi
          isDifferentType(tp1, tp1a) && tp1a <:< tp2
        } else if (sym1 == NothingClass) {
          true
        } else if (sym1 == NullClass) {
          tp2 match {
            case TypeRef(_, sym2, _) =>
              (sym2 isNonBottomSubClass ObjectClass) &&
              !(tp2.normalize.typeSymbol isNonBottomSubClass NotNullClass)
            case _ =>
              isSingleType(tp2) && tp1 <:< tp2.widen
          }
        } else if (isRaw(sym1, args1)) {
          isSubType(rawToExistential(tp1), tp2, depth)
        } else if (sym1.isRefinementClass) {
          isSubType(sym1.info, tp2, depth)
        } else {
          false
        }
      case RefinedType(parents1, ref1) =>
        parents1 exists (_ <:< tp2)
      case _: SingletonType | _: NotNullType =>
        tp1.underlying <:< tp2
      case _ =>
        false
    }

    firstTry
  }

  /** Does type `tp1' conform to `tp2'?
   */
  private def isSubType1(tp1: Type, tp2: Type, depth: Int): Boolean = {
    ((tp1, tp2) match {
      case (ErrorType, _)    => true
      case (WildcardType, _) => true
      case (_, ErrorType)    => true
      case (_, WildcardType) => true

      case (NoType, _)   => false
      case (NoPrefix, _) => tp2 == NoPrefix || tp2.typeSymbol.isPackageClass
      case (_, NoType)   => false
      case (_, NoPrefix) => tp1 == NoPrefix || tp1.typeSymbol.isPackageClass

      case (ThisType(_), ThisType(_))           => tp1 =:= tp2
      case (ThisType(_), SingleType(_, _))      => tp1 =:= tp2
      case (SingleType(_, _), ThisType(_))      => tp1 =:= tp2
      case (SingleType(_, _), SingleType(_, _)) => tp1 =:= tp2
      case (ConstantType(_), ConstantType(_))   => tp1 =:= tp2
      case (TypeRef(pre1, sym1, args1), TypeRef(pre2, sym2, args2))
      if !(tp1.isHigherKinded || tp2.isHigherKinded) =>
        //Console.println("isSubType " + tp1 + " " + tp2);//DEBUG

        def isSubArgs(tps1: List[Type], tps2: List[Type],
                      tparams: List[Symbol]): Boolean = (
          tps1.isEmpty && tps2.isEmpty
          ||
          !tps1.isEmpty && !tps2.isEmpty &&
          (tparams.head.isCovariant || (tps2.head <:< tps1.head)) &&
          (tparams.head.isContravariant || (tps1.head <:< tps2.head)) &&
          isSubArgs(tps1.tail, tps2.tail, tparams.tail)
        );
        ((if (sym1 == sym2) phase.erasedTypes || pre1 <:< pre2
          else (sym1.name == sym2.name) && isUnifiable(pre1, pre2)) &&
         (sym2 == AnyClass || isSubArgs(args1, args2, sym1.typeParams)) //@M: Any is kind-polymorphic
         ||
         sym1.isAbstractType && isDifferentType(tp1, tp1.bounds.hi) && (tp1.bounds.hi <:< tp2)
         ||
         sym2.isAbstractType && isDifferentType(tp2, tp2.bounds.lo) && (tp1 <:< tp2.bounds.lo)
         ||
         sym2.isClass &&
         ({ val base = tp1 baseType sym2; !(base eq tp1) && (base <:< tp2) })
         ||
         sym1 == NothingClass
         ||
         //{ Console.println("last chance " + sym1 + " " + sym2 + " " + sym2.isClass + " " + (sym2 isSubClass ObjectClass)); true } &&
         sym1 == NullClass &&
         sym2.isClass && (sym2 isNonBottomSubClass ObjectClass) && (!(tp2.normalize.typeSymbol isNonBottomSubClass NotNullClass))
         ||
         {
           val tp1n = normalizePlus(tp1)
           val tp2n = normalizePlus(tp2)
           ((tp1n ne tp1) || (tp2n ne tp2)) && isSubType(tp1n, tp2n, depth)
         })
      case (MethodType(params1, res1), MethodType(params2, res2)) =>
        (params1.length == params2.length &&
         matchingParams(tp1.paramTypes, tp2.paramTypes, tp1.isInstanceOf[JavaMethodType], tp2.isInstanceOf[JavaMethodType]) &&
         (res1 <:< res2) &&
         tp1.isInstanceOf[ImplicitMethodType] == tp2.isInstanceOf[ImplicitMethodType])
      case (PolyType(tparams1, res1), PolyType(tparams2, res2)) =>
        (tparams1.length == tparams2.length &&
         List.forall2(tparams1, tparams2)
           ((p1, p2) => p2.info.substSym(tparams2, tparams1) <:< p1.info) &&
         res1 <:< res2.substSym(tparams2, tparams1))
      case (TypeBounds(lo1, hi1), TypeBounds(lo2, hi2)) =>
        lo2 <:< lo1 && hi1 <:< hi2
      case (AnnotatedType(_,_,_), _) =>
        annotationsConform(tp1, tp2) && tp1.withoutAnnotations <:< tp2.withoutAnnotations
      case (_, AnnotatedType(_,_,_)) =>
        annotationsConform(tp1, tp2) && tp1.withoutAnnotations <:< tp2.withoutAnnotations
      case (BoundedWildcardType(bounds), _) =>
        bounds.lo <:< tp2
      case (_, BoundedWildcardType(bounds)) =>
        tp1 <:< bounds.hi
      case (_, tv2 @ TypeVar(_, constr2)) =>
        if (constr2.inst != NoType) tp1 <:< constr2.inst
        else isRelatable(tv2, tp1) && { constr2.lobounds = tp1 :: constr2.lobounds; true }
      case (tv1 @ TypeVar(_, constr1), _) =>
        if (constr1.inst != NoType) constr1.inst <:< tp2
        else isRelatable(tv1, tp2) && { constr1.hibounds = tp2 :: constr1.hibounds; true }
      case (_, _)  if (tp1.isHigherKinded || tp2.isHigherKinded) =>
        (tp1.typeSymbol == NothingClass
         ||
         tp2.typeSymbol == AnyClass // @M Any and Nothing are super-type resp. subtype of every well-kinded type
         || // @M! normalize reduces higher-kinded case to PolyType's
         (tp1.isHigherKinded && tp2.isHigherKinded) && {
           val tp1a = tp1.normalize
           val tp2a = tp2.normalize
           assert((tp1a ne tp1) || (tp2a ne tp2))
           isSubType0(tp1a, tp2a, depth)
         })
      case (_, TypeRef(pre2, sym2, args2))
      if (sym2.isAbstractType && isDifferentType(tp2, tp2.bounds.lo) && (tp1 <:< tp2.bounds.lo) ||
          sym2 == NotNullClass && tp1.isNotNull) =>
        true
      case (_, TypeRef(pre2, sym2, args2))
      if (sym2 == SingletonClass && tp1.isStable) =>
        true
      case (_, RefinedType(parents2, ref2)) =>
        (parents2 forall (tp2 => tp1 <:< tp2)) &&
        (ref2.toList forall tp1.specializes) /* &&
 removed, replaced by stricter condition on stable values.
        (tp1.typeSymbol != NullClass || !parents2.exists(_.typeSymbol.isAbstractType))
*/
      case (ExistentialType(_, _), _) =>
        try {
          skolemizationLevel += 1
          tp1.skolemizeExistential(NoSymbol, null) <:< tp2
        } finally {
          skolemizationLevel -= 1
        }
      case (_, et: ExistentialType) if et.withTypeVars(tp1 <:< _, depth) =>
        true
      case (RefinedType(parents1, ref1), _) =>
        parents1 exists (_ <:< tp2)
      case (_, NotNullType(ntp2)) =>
        tp1.isNotNull && tp1 <:< ntp2
      case (NotNullType(ntp1), _) =>
        ntp1 <:< tp2
      case ((_: ThisType | _: SingleType | _: ConstantType), _) =>
        tp1.underlying <:< tp2

      case (TypeRef(pre1, sym1, args1), _) =>
        (sym1 == NothingClass && tp2 <:< AnyClass.tpe
         ||
         sym1 == NullClass && tp2.isInstanceOf[SingletonType] && (tp1 <:< tp2.widen)
         ||
         sym1.isAbstractType && (tp1.bounds.hi <:< tp2))
      case _ =>
        false
    }) || {
      val tp1n = normalizePlus(tp1)
      val tp2n = normalizePlus(tp2)
      ((tp1n ne tp1) || (tp2n ne tp2)) && isSubType(tp1n, tp2n, depth)
    }
  }

  /** Are `tps1' and `tps2' lists of equal length such
   *  that all elements of `tps1' conform to corresponding elements
   *  of `tps2'?
   */
  def isSubTypes(tps1: List[Type], tps2: List[Type]): Boolean =
    tps1.length == tps2.length &&
    List.forall2(tps1, tps2)((tp1, tp2) => tp1 <:< tp2)

  /** Does type `tp' implement symbol `sym' with same or
   *  stronger type? Exact only if `sym' is a member of some
   *  refinement type, otherwise we might return false negatives.
   */
  def specializesSym(tp: Type, sym: Symbol): Boolean =
    tp.typeSymbol == NothingClass ||
    tp.typeSymbol == NullClass && (sym.owner isSubClass ObjectClass) ||
    (tp.nonPrivateMember(sym.name).alternatives exists
      (alt => sym == alt || specializesSym(tp.narrow, alt, sym.owner.thisType, sym)))

  /** Does member `sym1' of `tp1' have a stronger type
   *  than member `sym2' of `tp2'?
   */
  private def specializesSym(tp1: Type, sym1: Symbol, tp2: Type, sym2: Symbol): Boolean = {
    val info1 = tp1.memberInfo(sym1)
    val info2 = tp2.memberInfo(sym2).substThis(tp2.typeSymbol, tp1)
    //System.out.println("specializes "+tp1+"."+sym1+":"+info1+sym1.locationString+" AND "+tp2+"."+sym2+":"+info2)//DEBUG
    sym2.isTerm && (info1 <:< info2) /*&& (!sym2.isStable || sym1.isStable) */ ||
    sym2.isAbstractType && info2.bounds.containsType(tp1.memberType(sym1)) ||
    sym2.isAliasType && tp2.memberType(sym2).substThis(tp2.typeSymbol, tp1) =:= tp1.memberType(sym1) //@MAT ok
  }

  /** A function implementing `tp1' matches `tp2' */
  def matchesType(tp1: Type, tp2: Type, alwaysMatchSimple: Boolean): Boolean = {
    def matchesQuantified(tparams1: List[Symbol], tparams2: List[Symbol], res1: Type, res2: Type): Boolean =
      tparams1.length == tparams2.length &&
      matchesType(res1, res2.substSym(tparams2, tparams1), alwaysMatchSimple)
    (tp1, tp2) match {
      case (MethodType(params1, res1), MethodType(params2, res2)) =>
        matchingParams(tp1.paramTypes, tp2.paramTypes, tp1.isInstanceOf[JavaMethodType], tp2.isInstanceOf[JavaMethodType]) &&
        matchesType(res1, res2, alwaysMatchSimple) &&
        tp1.isInstanceOf[ImplicitMethodType] == tp2.isInstanceOf[ImplicitMethodType]
      case (PolyType(tparams1, res1), PolyType(tparams2, res2)) =>
        matchesQuantified(tparams1, tparams2, res1, res2)
      case (PolyType(List(), rtp1), MethodType(List(), rtp2)) =>
        matchesType(rtp1, rtp2, alwaysMatchSimple)
      case (MethodType(List(), rtp1), PolyType(List(), rtp2)) =>
        matchesType(rtp1, rtp2, alwaysMatchSimple)
      case (ExistentialType(tparams1, res1), ExistentialType(tparams2, res2)) =>
        matchesQuantified(tparams1, tparams2, res1, res2)
      case (ExistentialType(_, res1), _) if alwaysMatchSimple =>
        matchesType(res1, tp2, alwaysMatchSimple)
      case (_, ExistentialType(_, res2)) if alwaysMatchSimple =>
        matchesType(tp1, res2, alwaysMatchSimple)
      case (PolyType(List(), rtp1), _) =>
        matchesType(rtp1, tp2, alwaysMatchSimple)
      case (_, PolyType(List(), rtp2)) =>
        matchesType(tp1, rtp2, alwaysMatchSimple)
      case (MethodType(_, _), _) => false
      case (PolyType(_, _), _)   => false
      case (_, MethodType(_, _)) => false
      case (_, PolyType(_, _))   => false
      case _ =>
        alwaysMatchSimple || tp1 =:= tp2
    }
  }

  /** Are `tps1' and `tps2' lists of pairwise equivalent types? */
  private def matchingParams(tps1: List[Type], tps2: List[Type], tps1isJava: Boolean, tps2isJava: Boolean): Boolean =
    tps1.length == tps2.length &&
    List.forall2(tps1, tps2)((tp1, tp2) =>
      (tp1 =:= tp2) ||
      tps1isJava && tp2.typeSymbol == ObjectClass && tp1.typeSymbol == AnyClass ||
      tps2isJava && tp1.typeSymbol == ObjectClass && tp2.typeSymbol == AnyClass)

  /** like map2, but returns list `xs' itself - instead of a copy - if function
   *  `f' maps all elements to themselves.
   */
  def map2Conserve[A <: AnyRef, B](xs: List[A], ys: List[B])(f: (A, B) => A): List[A] =
    if (xs.isEmpty) xs
    else {
      val x1 = f(xs.head, ys.head)
      val xs1 = map2Conserve(xs.tail, ys.tail)(f)
      if ((x1 eq xs.head) && (xs1 eq xs.tail)) xs
      else x1 :: xs1
    }

  /** Solve constraint collected in types `tvars'.
   *
   *  @param tvars      All type variables to be instantiated.
   *  @param tparams    The type parameters corresponding to `tvars'
   *  @param variances  The variances of type parameters; need to reverse
   *                    solution direction for all contravariant variables.
   *  @param upper      When `true' search for max solution else min.
   */
  def solve(tvars: List[TypeVar], tparams: List[Symbol],
            variances: List[Int], upper: Boolean): Boolean =
     solve(tvars, tparams, variances, upper, AnyDepth)

  def solve(tvars: List[TypeVar], tparams: List[Symbol],
            variances: List[Int], upper: Boolean, depth: Int): Boolean = {
    val config = tvars zip (tparams zip variances)

    def solveOne(tvar: TypeVar, tparam: Symbol, variance: Int) {
      if (tvar.constr.inst == NoType) {
        val up = if (variance != CONTRAVARIANT) upper else !upper
        tvar.constr.inst = null
        val bound: Type = if (up) tparam.info.bounds.hi else tparam.info.bounds.lo
        //Console.println("solveOne0 "+tvar+" "+config+" "+bound);//DEBUG
        var cyclic = bound contains tparam
        for ((tvar2, (tparam2, variance2)) <- config) {
          if (tparam2 != tparam &&
              ((bound contains tparam2) ||
               up && (tparam2.info.bounds.lo =:= tparam.tpe) ||  //@M TODO: might be affected by change to tpe in Symbol
               !up && (tparam2.info.bounds.hi =:= tparam.tpe))) {  //@M TODO: might be affected by change to tpe in Symbol
            if (tvar2.constr.inst eq null) cyclic = true
            solveOne(tvar2, tparam2, variance2)
          }
        }
        if (!cyclic) {
          if (up) {
            if (bound.typeSymbol != AnyClass) {
              tvar.constr.hibounds =
                bound.instantiateTypeParams(tparams, tvars) :: tvar.constr.hibounds
            }
            for (tparam2 <- tparams)
              if (tparam2.info.bounds.lo =:= tparam.tpe)  //@M TODO: might be affected by change to tpe in Symbol
                tvar.constr.hibounds =
                  tparam2.tpe.instantiateTypeParams(tparams, tvars) :: tvar.constr.hibounds
          } else {
            if (bound.typeSymbol != NothingClass && bound.typeSymbol != tparam) {
              tvar.constr.lobounds =
                bound.instantiateTypeParams(tparams, tvars) :: tvar.constr.lobounds
            }
            for (tparam2 <- tparams)
              if (tparam2.info.bounds.hi =:= tparam.tpe)  //@M TODO: might be affected by change to tpe in Symbol
                tvar.constr.lobounds =
                  tparam2.tpe.instantiateTypeParams(tparams, tvars) :: tvar.constr.lobounds
          }
        }
        tvar.constr.inst = NoType // necessary because hibounds/lobounds may contain tvar
        tvar setInst (
          if (up) {
            if (depth != AnyDepth) glb(tvar.constr.hibounds, depth) else glb(tvar.constr.hibounds)
          } else {
            if (depth != AnyDepth) lub(tvar.constr.lobounds, depth) else lub(tvar.constr.lobounds)
          })
        //Console.println("solving "+tvar+" "+up+" "+(if (up) (tvar.constr.hibounds) else tvar.constr.lobounds)+((if (up) (tvar.constr.hibounds) else tvar.constr.lobounds) map (_.widen))+" = "+tvar.constr.inst)//DEBUG"
      }
    }
    for ((tvar, (tparam, variance)) <- config)
      solveOne(tvar, tparam, variance)

    var ok = true
    for (tvar <- tvars)
      if (!(tvar.constr.lobounds forall (_ <:< tvar.constr.inst)) ||
          !(tvar.constr.hibounds forall (tvar.constr.inst <:< _))) {
        ok = false
      }
    ok
  }

  /** Do type arguments `targs' conform to formal parameters
   *  `tparams'?
   *
   *  @param tparams ...
   *  @param targs   ...
   *  @return        ...
   */
  def isWithinBounds(pre: Type, owner: Symbol, tparams: List[Symbol], targs: List[Type]): Boolean = {
    val bounds = instantiatedBounds(pre, owner, tparams, targs)
    !(List.map2(bounds, targs)((bound, targ) => bound containsType targ) contains false)
  }

  def instantiatedBounds(pre: Type, owner: Symbol, tparams: List[Symbol], targs: List[Type]): List[TypeBounds] =
    tparams map (_.info.asSeenFrom(pre, owner).instantiateTypeParams(tparams, targs).bounds)

// Lubs and Glbs ---------------------------------------------------------

  /** The least sorted upwards closed upper bound of a non-empty list
   *  of lists of types relative to the following ordering &lt;= between lists of types:
   *
   *    xs &lt;= ys   iff   forall y in ys exists x in xs such that x &lt;: y
   *
   *  @See baseTypeSeq  for a definition of sorted and upwards closed.
   */
  private def lubList(tss: List[List[Type]], depth: Int): List[Type] =
    if (tss.tail.isEmpty) tss.head
    else if (tss exists (_.isEmpty)) List()
    else {
      val ts0 = tss map (_.head)
      val sym = minSym(ts0)
      if (ts0 forall (t => t.typeSymbol == sym))
        mergePrefixAndArgs(elimSub(ts0, depth), 1, depth).toList ::: lubList(tss map (_.tail), depth)
      else
        lubList(tss map (ts => if (ts.head.typeSymbol == sym) ts.tail else ts), depth)
    }

  private def lubBaseTypeSeq(tss: List[BaseTypeSeq], depth: Int): List[Type] =
    lubList(tss map (_.toList), depth)

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

  /** A collector that tests for existential types appearing at given variance in a type */
  class ContainsVariantExistentialCollector(v: Int) extends TypeCollector(false) {
    def traverse(tp: Type) = tp match {
      case ExistentialType(_, _) if (variance == v) => result = true
      case _ => mapOver(tp)
    }
    def init() = {
      variance = 1
      this
    }
  }

  val containsCovariantExistentialCollector = new ContainsVariantExistentialCollector(1)
  val containsContravariantExistentialCollector = new ContainsVariantExistentialCollector(-1)

  /** Eliminate from list of types all elements which are a subtype
   *  of some other element of the list. */
  private def elimSub(ts: List[Type], depth: Int): List[Type] = {
    def elimAnonymousClass(t: Type) = t match {
      case TypeRef(pre, clazz, List()) if clazz.isAnonymousClass =>
        clazz.classBound.asSeenFrom(pre, clazz.owner)
      case _ =>
        t
    }
    def elimSub0(ts: List[Type]): List[Type] = ts match {
      case List() => List()
      case t :: ts1 =>
        val rest = elimSub0(ts1 filter (t1 => !isSubType(t1, t, decr(depth))))
        if (rest exists (t1 => isSubType(t, t1, decr(depth)))) rest else t :: rest
    }
    val ts0 = elimSub0(ts)
    if (ts0.length <= 1) ts0
    else {
      val ts1 = List.mapConserve(ts0)(t => elimAnonymousClass(t.underlying))
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
      case TypeVar(_, constr) =>
        if ((constr.inst ne null) && (constr.inst ne NoType)) constr.inst
        else throw new Error("trying to do lub/glb of typevar "+tp)
      case t => t
    }
    val strippedTypes = List.mapConserve(ts)(stripType)
    (strippedTypes, quantified)
  }

  def lub(ts: List[Type]): Type = lub(ts, lubDepth(ts))

  /** The least upper bound wrt &lt;:&lt; of a list of types */
  def lub(ts: List[Type], depth: Int): Type = {
    def lub0(ts0: List[Type]): Type = elimSub(ts0, depth) match {
      case List() => NothingClass.tpe
      case List(t) => t
      case ts @ PolyType(tparams, _) :: _ =>
        PolyType(
          List.map2(tparams, List.transpose(matchingBounds(ts, tparams)))
            ((tparam, bounds) => tparam.cloneSymbol.setInfo(glb(bounds, depth))),
          lub0(matchingInstTypes(ts, tparams)))
      case ts @ MethodType(params, _) :: rest =>
        MethodType(params, lub0(matchingRestypes(ts, params map (_.tpe))))
      case ts @ TypeBounds(_, _) :: rest =>
        mkTypeBounds(glb(ts map (_.bounds.lo), depth), lub(ts map (_.bounds.hi), depth))
      case ts0 =>
        val (ts, tparams) = stripExistentialsAndTypeVars(ts0)
        val bts: List[BaseTypeSeq] = ts map (_.baseTypeSeq)
        val lubBaseTypes: List[Type] = lubBaseTypeSeq(bts, depth)
        val lubParents = spanningTypes(lubBaseTypes)
        val lubOwner = commonOwner(ts)
        val lubBase = intersectionType(lubParents, lubOwner)
        val lubType =
          if (phase.erasedTypes || depth == 0) lubBase
          else {
            val lubRefined = refinedType(lubParents, lubOwner)
            val lubThisType = lubRefined.typeSymbol.thisType
            val narrowts = ts map (_.narrow)
            def lubsym(proto: Symbol): Symbol = {
              val prototp = lubThisType.memberInfo(proto)
              val syms = narrowts map (t =>
                t.nonPrivateMember(proto.name).suchThat(sym =>
                  sym.tpe matches prototp.substThis(lubThisType.typeSymbol, t)))
              if (syms contains NoSymbol) NoSymbol
              else {
                val symtypes =
                  (List.map2(narrowts, syms)
                     ((t, sym) => t.memberInfo(sym).substThis(t.typeSymbol, lubThisType)));
                if (proto.isTerm) // possible problem: owner of info is still the old one, instead of new refinement class
                  proto.cloneSymbol(lubRefined.typeSymbol).setInfo(lub(symtypes, decr(depth)))
                else if (symtypes.tail forall (symtypes.head =:=))
                  proto.cloneSymbol(lubRefined.typeSymbol).setInfo(symtypes.head)
                else {
                  def lubBounds(bnds: List[TypeBounds]): TypeBounds =
                    mkTypeBounds(glb(bnds map (_.lo), decr(depth)), lub(bnds map (_.hi), decr(depth)))
                  recycle(lubRefined.typeSymbol.newAbstractType(proto.pos, proto.name))
                    .setInfo(lubBounds(symtypes map (_.bounds)))
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
            for (sym <- lubBase.nonPrivateMembers) {
              // add a refinement symbol for all non-class members of lubBase
              // which are refined by every type in ts.
              if (!sym.isClass && !sym.isConstructor && (narrowts forall (t => refines(t, sym))))
                try {
                  val lsym = lubsym(sym)
                  if (lsym != NoSymbol) addMember(lubThisType, lubRefined, lubsym(sym))
                } catch {
                  case ex: NoCommonType =>
                }
            }
            if (lubRefined.decls.isEmpty) lubBase
            else {
//            println("refined lub of "+ts+"/"+narrowts+" is "+lubRefined+", baseclasses = "+(ts map (_.baseTypeSeq) map (_.toList)))
              lubRefined
            }
          }
        existentialAbstraction(tparams, lubType)
    }
    if (printLubs) {
      println(indent + "lub of " + ts + " at depth "+depth)//debug
      indent = indent + "  "
      assert(indent.length <= 100)
    }
    val res = if (depth < 0) AnyClass.tpe else lub0(ts)
    if (printLubs) {
      indent = indent.substring(0, indent.length() - 2)
      println(indent + "lub of " + ts + " is " + res)//debug
    }
    if (ts forall (_.isNotNull)) res.notNull else res
  }

  val GlbFailure = new Throwable

  /** A global counter for glb calls in the `specializes' query connected to the `addMembers'
   *  call in `glb'. There's a possible inifinite recursion when `specializes' calls
   *  memberType, which calls baseTypeSeq, which calls mergePrefixAndArgs, which calls glb.
   *  The counter breaks this recursion after two calls.
   *  If the recursion is broken, no member is added to the glb.
   */
  private var globalGlbDepth = 0
  private final val globalGlbLimit = 2

  def glb(ts: List[Type]): Type = glb(ts, lubDepth(ts))

  /** The greatest lower bound wrt &lt;:&lt; of a list of types */
  private def glb(ts: List[Type], depth: Int): Type = {
    def glb0(ts0: List[Type]): Type = elimSuper(ts0 map (_.deconst)) match {// todo: deconst needed?
      case List() => AnyClass.tpe
      case List(t) => t
      case ts @ PolyType(tparams, _) :: _ =>
        PolyType(
          List.map2(tparams, List.transpose(matchingBounds(ts, tparams)))
          ((tparam, bounds) => tparam.cloneSymbol.setInfo(lub(bounds, depth))),
          glb0(matchingInstTypes(ts, tparams)))
      case ts @ MethodType(params, _) :: rest =>
        MethodType(params, glb0(matchingRestypes(ts, params map (_.tpe))))
      case ts @ TypeBounds(_, _) :: rest =>
        mkTypeBounds(lub(ts map (_.bounds.lo), depth), glb(ts map (_.bounds.hi), depth))
      case ts0 =>
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
                proto.cloneSymbol(glbRefined.typeSymbol).setInfo(
                  if (proto.isTerm) glb(symtypes, decr(depth))
                  else {
                    def isTypeBound(tp: Type) = tp match {
                      case TypeBounds(_, _) => true
                      case _ => false
                    }
                    def glbBounds(bnds: List[Type]): TypeBounds = {
                      val lo = lub(bnds map (_.bounds.lo), decr(depth))
                      val hi = glb(bnds map (_.bounds.hi), decr(depth))
                      if (lo <:< hi) mkTypeBounds(lo, hi)
                      else throw GlbFailure
                    }
                    val symbounds = symtypes filter isTypeBound
                    var result: Type =
                      if (symbounds.isEmpty)
                        mkTypeBounds(NothingClass.tpe, AnyClass.tpe)
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
                  for (ds <- dss; val sym <- ds.iterator)
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
    if (settings.debug.value) {
      println(indent + "glb of " + ts + " at depth "+depth)//debug
      indent = indent + "  "
    }
    val res = if (depth < 0) NothingClass.tpe else glb0(ts)
    if (settings.debug.value) {
      indent = indent.substring(0, indent.length() - 2)
      log(indent + "glb of " + ts + " is " + res)//debug
    }
    if (ts exists (_.isNotNull)) res.notNull else res
  }

  /** The most deeply nested owner that contains all the symbols
   *  of thistype or prefixless typerefs/singletype occurrences in given type.
   */
  private def commonOwner(t: Type): Symbol = {
    commonOwnerMap.init
    commonOwnerMap.apply(t)
    commonOwnerMap.result
  }

  /** The most deeply nested owner that contains all the symbols
   *  of thistype or prefixless typerefs/singletype occurrences in given list
   *  of types.
   */
  private def commonOwner(tps: List[Type]): Symbol = {
    if (settings.debug.value) log("computing common owner of types " + tps)//debug
    commonOwnerMap.init
    tps foreach { tp => commonOwnerMap.apply(tp); () }
    commonOwnerMap.result
  }

  /** Compute lub (if variance == 1) or glb (if variance == -1) of given list
   *  of types `tps'. All types in `tps' are typerefs or singletypes
   *  with the same symbol.
   *  Return `Some(x)' if the computation succeeds with result `x'.
   *  Return `None' if the computuation fails.
   */
  def mergePrefixAndArgs(tps: List[Type], variance: Int, depth: Int): Option[Type] = tps match {
    case List(tp) =>
      Some(tp)
    case TypeRef(_, sym, _) :: rest =>
      val pres = tps map (_.prefix)
      val pre = if (variance == 1) lub(pres, depth) else glb(pres, depth)
      val argss = tps map (_.typeArgs)
      val capturedParams = new ListBuffer[Symbol]
      val args = List.map2(sym.typeParams, List.transpose(argss)) {
        (tparam, as) =>
          if (depth == 0)
            if (tparam.variance == variance) AnyClass.tpe
            else if (tparam.variance == -variance) NothingClass.tpe
            else NoType
          else
            if (tparam.variance == variance) lub(as, decr(depth))
            else if (tparam.variance == -variance) glb(as, decr(depth))
            else {
              val l = lub(as, decr(depth))
              val g = glb(as, decr(depth))
              if (l <:< g) l
              else {
                val owner = commonOwner(as)
                val qvar = makeFreshExistential("", commonOwner(as), mkTypeBounds(g, l))
                capturedParams += qvar
                qvar.tpe
              }
            }
      }
      try {
        if (args contains NoType) None
        else Some(existentialAbstraction(capturedParams.toList, typeRef(pre, sym, args)))
      } catch {
        case ex: MalformedType => None
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

  /** Make symbol `sym' a member of scope `tp.decls'
   *  where `thistp' is the narrowed owner type of the scope.
   */
  def addMember(thistp: Type, tp: Type, sym: Symbol) {
    assert(sym != NoSymbol)
    if (settings.debug.value) log("add member " + sym+":"+sym.info+" to "+thistp)
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
  private def matchingBounds(tps: List[Type], tparams: List[Symbol]): List[List[Type]] =
    tps map {
      case PolyType(tparams1, _) if (tparams1.length == tparams.length) =>
        tparams1 map (tparam => tparam.info.substSym(tparams1, tparams))
      case _ =>
        throw new NoCommonType(tps)
    }

  /** All types in list must be polytypes with type parameter lists of
   *  same length as tparams.
   *  Returns list of instance types, where corresponding type
   *  parameters are renamed to tparams.
   */
  private def matchingInstTypes(tps: List[Type], tparams: List[Symbol]): List[Type] =
    tps map {
      case PolyType(tparams1, restpe) if (tparams1.length == tparams.length) =>
        restpe.substSym(tparams1, tparams)
      case _ =>
        throw new NoCommonType(tps)
    }

  /** All types in list must be method types with equal parameter types.
   *  Returns list of their result types.
   */
  private def matchingRestypes(tps: List[Type], pts: List[Type]): List[Type] =
    tps map {
      case MethodType(params1, res) if (isSameTypes(params1 map (_.tpe), pts)) =>
        res
      case _ =>
        throw new NoCommonType(tps)
    }

// Errors and Diagnostics -----------------------------------------------------

  /** An exception signalling a type error */
  class TypeError(val pos: Position, val msg: String) extends java.lang.Error(msg) {
    def this(msg: String) = this(NoPosition, msg)
  }

  class NoCommonType(tps: List[Type]) extends java.lang.Error(
    "lub/glb of incompatible types: " + tps.mkString("", " and ", ""))

  /** An exception signalling a malformed type */
  class MalformedType(msg: String) extends TypeError(msg) {
    def this(pre: Type, tp: String) = this("malformed type: " + pre + "#" + tp)
  }

  /** An exception signalling a variance annotation/usage conflict */
  class VarianceError(msg: String) extends TypeError(msg)

  /** The current indentation string for traces */
  private var indent: String = ""

  /** Perform operation `p' on arguments `tp1',
   *  `arg2' and print trace of computation.
   */
  private def explain[T](op: String, p: (Type, T) => Boolean, tp1: Type, arg2: T): Boolean = {
    Console.println(indent + tp1 + " " + op + " " + arg2 + "?" /* + "("+tp1.getClass+","+arg2.asInstanceOf[AnyRef].getClass+")"*/)
    indent = indent + "  "
    val result = p(tp1, arg2)
    indent = indent.substring(0, indent.length() - 2)
    Console.println(indent + result)
    result
  }

  /** If option `explaintypes' is set, print a subtype trace for
   *  `found <:< required'.
   */
  def explainTypes(found: Type, required: Type) {
    if (settings.explaintypes.value) withTypesExplained(found <:< required)
  }

  /** If option `explaintypes' is set, print a subtype trace for
   *  `op(found, required)'.
   */
  def explainTypes(op: (Type, Type) => Any, found: Type, required: Type) {
    if (settings.explaintypes.value) withTypesExplained(op(found, required))
  }

  /** Execute `op' while printing a trace of the operations on types executed.
   */
  def withTypesExplained[A](op: => A): A = {
    val s = explainSwitch
    try { explainSwitch = true; op } finally { explainSwitch = s }
  }

  def objToAny(tp: Type): Type =
    if (!phase.erasedTypes && tp.typeSymbol == ObjectClass) AnyClass.tpe
    else tp

  val shorthands = Set(
    "scala.collection.immutable.List",
    "scala.collection.immutable.Nil",
    "scala.collection.Sequence",
    "scala.collection.Traversable",
    "scala.collection.Iterable",
    "scala.collection.mutable.StringBuilder",
    "scala.collection.Vector",
    "scala.collection.Iterator")
}

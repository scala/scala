/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.symtab

import scala.collection.mutable.{ListBuffer, HashMap}
import scala.compat.Platform.currentTime
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
  case AnnotatedType(attribs, tp) =>
    // tp @attribs

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
  var singletonClosureCount = 0
  var compoundClosureCount = 0
  var typerefClosureCount = 0
  var findMemberCount = 0
  var noMemberCount = 0
  var multMemberCount = 0
  var findMemberMillis = 0l
  var subtypeCount = 0
  var subtypeMillis = 0l

  private var explainSwitch = false
  private var checkMalformedSwitch = true

  private final val LubGlbMargin = 0
  private final val LogPendingSubTypesThreshold = 50

  val emptyTypeArray = new Array[Type](0)

  /** A map from lists to compound types that have the given list as parents.
   *  This is used to avoid duplication in the computation of closure and baseClasses.
   *  It makes use of the fact that these two operations depend only on the parents,
   *  not on the refinement.
   */
  var intersectionWitness = new HashMap[List[Type], Type]

  /** A proxy for a type (identified by field `tp') that forwards all operations to it
   *  Every operation that is overridden for some kind of types should be forwarded.
   */
  trait TypeProxy extends Type {
    val tp: Type

    protected def maybeRewrap(newtp: Type) = if (newtp eq tp) this else rewrap(newtp)
    protected def rewrap(newtp: Type): Type = this

    // the following are all operations in class Type that are overridden in some subclass
    // Important to keep this up-to-date when new operations are added!
    override def isTrivial = tp.isTrivial
    override def isHigherKinded: Boolean = tp.isHigherKinded
    override def isNotNull = tp.isNotNull
    override def isError = tp.isError
    override def isErroneous = tp.isErroneous
    override def isStable: Boolean = tp.isStable
    override def symbol = tp.symbol
    override def singleDeref = maybeRewrap(tp.singleDeref)
    override def widen = maybeRewrap(tp.widen)
    override def deconst = maybeRewrap(tp.deconst)
    override def typeOfThis = tp.typeOfThis
    override def narrow = tp.narrow
    override def bounds = tp.bounds
    override def parents = tp.parents
    override def prefix = tp.prefix
    override def typeArgs = tp.typeArgs
    override def resultType = maybeRewrap(tp.resultType)
    override def resultType(actuals: List[Type]) = maybeRewrap(tp.resultType(actuals))
    override def finalResultType = maybeRewrap(tp.finalResultType)
    override def paramSectionCount = tp.paramSectionCount
    override def paramTypes = tp.paramTypes
    override def typeParams = tp.typeParams
    override def notNull = maybeRewrap(tp.notNull)
    override def instantiateTypeParams(formals: List[Symbol], actuals: List[Type]) =
      tp.instantiateTypeParams(formals, actuals)
    override def skolemizeExistential(owner: Symbol, origin: AnyRef) =
      tp.skolemizeExistential(owner, origin)
    override def normalize = maybeRewrap(tp.normalize)
    override def decls = tp.decls
    override def baseType(clazz: Symbol) = tp.baseType(clazz)
    override def closure = tp.closure
    override def closureDepth = tp.closureDepth
    override def baseClasses = tp.baseClasses
    override def cloneInfo(owner: Symbol) = maybeRewrap(tp.cloneInfo(owner))
    override def prefixString = tp.prefixString
    override def isComplete = tp.isComplete
    override def complete(sym: Symbol) = tp.complete(sym)
    override def load(sym: Symbol): Unit = tp.load(sym)
    override def withAttributes(attribs: List[AnnotationInfo]) = tp.withAttributes(attribs)
    override def withoutAttributes = tp.withoutAttributes
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

    /** Is this type guaranteed not to have `null' as a value? */
    def isNotNull: Boolean = false

    /** Does this depend on an enclosing method parameter? */
    def isDependent: Boolean = {
      IsDependentTraverser.result = false
      IsDependentTraverser.traverse(this)
      IsDependentTraverser.result
    }

    /** The symbol associated with the type
      * Note that the symbol of the normalized type is returned (@see normalize)
      */
    def symbol: Symbol = NoSymbol

    /** The base type underlying a singleton type,
     *  identity on all other types */
    def singleDeref: Type = this

    /** Widen from singleton type to its underlying non-singleton base type
     *  by applying one or more singleDeref steps,
     *  identity for all other types */
    def widen: Type = this

    /** Map a constant type or not-null-type to its underlying base type,
     *  identity for all other types */
    def deconst: Type = this

    /** The type of `this' of a class type or reference type
     */
    def typeOfThis: Type = symbol.typeOfThis

    /** Map to a this type which is a subtype of this type.
     */
    def narrow: Type =
      if (phase.erasedTypes) this
      else refinedType(List(this), commonOwner(this), EmptyScope).narrow

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

    /** For a method or poly type, the types of its first value parameter section,
     *  the empty list for all other types */
    def paramTypes: List[Type] = List()

    /** For a poly type, its type parameters,
     *  the empty list for all other types */
    def typeParams: List[Symbol] = List()

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
    def isError: Boolean = symbol.isError

    /** Is this type produced as a repair for an error? */
    def isErroneous: Boolean = {
      ErroneousTraverser.result = false
      ErroneousTraverser.traverse(this)
      ErroneousTraverser.result
    }

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
    def members: List[Symbol] = findMember(nme.ANYNAME, 0, 0, false).alternatives

    /** A list of all non-private members of this type (defined or inherited) */
    def nonPrivateMembers: List[Symbol] =
      findMember(nme.ANYNAME, PRIVATE | BRIDGE, 0, false).alternatives

    /** A list of all implicit symbols of this type  (defined or inherited) */
    def implicitMembers: List[Symbol] =
      findMember(nme.ANYNAME, BRIDGE, IMPLICIT, false).alternatives

    /** The member with given name,
     *  an OverloadedSymbol if several exist, NoSymbol if none exist */
    def member(name: Name): Symbol = findMember(name, BRIDGE, 0, false)

    /** The non-private member with given name,
     *  an OverloadedSymbol if several exist, NoSymbol if none exist */
    def nonPrivateMember(name: Name): Symbol =
      findMember(name, PRIVATE | BRIDGE, 0, false)

    /** The non-local member with given name,
     *  an OverloadedSymbol if several exist, NoSymbol if none exist */
    def nonLocalMember(name: Name): Symbol =
      findMember(name, LOCAL | BRIDGE, 0, false)

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
      if (!isTrivial && (!phase.erasedTypes || pre.symbol == ArrayClass)) {
        val m = new AsSeenFromMap(pre, clazz)
        val tp = m apply this
        existentialAbstraction(m.capturedParams, tp)
      } else this

    /** The info of `sym', seen as a member of this type.
     */
    def memberInfo(sym: Symbol): Type =
      sym.info.asSeenFrom(this, sym.owner)

    /** The type of `sym', seen as a member of this type. */
    def memberType(sym: Symbol): Type = {
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
          //Console.println("" + this + ".memberType(" + sym +":" + sym.tpe +")" + sym.ownerChain);//debug
          tp.asSeenFrom(this, sym.owner)
      }
    }

    /** Substitute types `to' for occurrences of references to
     *  symbols `from' in this type.
     */
    def subst(from: List[Symbol], to: List[Type]): Type =
      new SubstTypeMap(from, to) apply this

    /** Substitute symbols `to' for occurrences of symbols
     *  `from' in this type.
     */
    def substSym(from: List[Symbol], to: List[Symbol]): Type =
      new SubstSymMap(from, to) apply this

    /** Substitute all occurrences of `ThisType(from)' in this type
     *  by `to'.
     */
    def substThis(from: Symbol, to: Type): Type =
      new SubstThisMap(from, to) apply this

    def substSuper(from: Type, to: Type): Type =
      new SubstSuperMap(from, to) apply this

    /** Returns all parts of this type which satisfy predicate `p' */
    def filter(p: Type => Boolean): List[Type] = {
      new FilterTraverser(p).traverse(this).hits.toList
    }

    /** Returns optionally first type (in a preorder traversal) which satisfies predicate `p',
     *  or None if none exists.
     */
    def find(p: Type => Boolean): Option[Type] = {
      new FindTraverser(p).traverse(this).result
    }

    /** Apply `f' to each part of this type */
    def foreach(f: Type => Unit): Unit = new ForEachTraverser(f).traverse(this)

    /** Is there part of this type which satisfies predicate `p'? */
    def exists(p: Type => Boolean): Boolean = !find(p).isEmpty

    /** Does this type contain a reference to this symbol? */
    def contains(sym: Symbol): Boolean =
      new ContainsTraverser(sym).traverse(this).result

    /** Does this type contain a reference to this type */
    def containsTp(tp: Type): Boolean =
      new ContainsTypeTraverser(tp).traverse(this).result

    /** Is this type a subtype of that type? */
    def <:<(that: Type): Boolean = {
      if (util.Statistics.enabled) subtypeCount = subtypeCount + 1
      val startTime = if (util.Statistics.enabled) currentTime else 0l
      val result =
        ((this eq that) ||
         (if (explainSwitch) explain("<", isSubType, this, that)
          else isSubType(this, that)));
      if (util.Statistics.enabled)
        subtypeMillis = subtypeMillis + currentTime - startTime
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
    def closure: Array[Type] = Array(this)

    /** The maximum depth (@see maxDepth) of each type in the closure of this type. */
    def closureDepth: Int = 1

    def baseClasses: List[Symbol] = List()

    /**
     *  @param sym the class symbol
     *  @return    the index of given class symbol in the closure of this type,
     *             or -1 if no base type with given class symbol exists.
     */
    def closurePos(sym: Symbol): Int = {
      val cl = closure
      var lo = 0
      var hi = cl.length - 1
      while (lo <= hi) {
        val mid = (lo + hi) / 2
        val clsym = cl(mid).symbol
        if (sym == clsym) return mid
        else if (sym isLess clsym) hi = mid - 1
        else if (clsym isLess sym) lo = mid + 1
        else throw new Error()
      }
      -1
    }

    /** If this is a polytype, a copy with cloned type parameters owned
     *  by `owner'. Identity for all other types.
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

    /** Is this type completed (i.e. not a lazy type)?
     */
    def isComplete: Boolean = true

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
      else baseClasses.head.newOverloaded(this, alts)
    }

    /**
     *  @param name          ...
     *  @param excludedFlags ...
     *  @param requiredFlags ...
     *  @param stableOnly    ...
     *  @return              ...
     */
    //TODO: use narrow only for modules? (correct? efficiency gain?)
    def findMember(name: Name, excludedFlags: Int, requiredFlags: Long, stableOnly: Boolean): Symbol = {
      if (inIDE) trackTypeIDE(symbol)
      if (util.Statistics.enabled) findMemberCount = findMemberCount + 1
      val startTime = if (util.Statistics.enabled) currentTime else 0l

      //Console.println("find member " + name.decode + " in " + this + ":" + this.baseClasses)//DEBUG
      var members: Scope = null
      var member: Symbol = NoSymbol
      var excluded = excludedFlags | DEFERRED
      var self: Type = null
      var continue = true
      var savedCheckMalformedSwitch = checkMalformedSwitch
      checkMalformedSwitch = false
      while (continue) {
        continue = false
        var bcs = baseClasses
        while (!bcs.isEmpty) {
          val decls = bcs.head.info.decls
          bcs = if (name == nme.CONSTRUCTOR) Nil else bcs.tail
          var entry =
            if (name == nme.ANYNAME) decls.elems else decls lookupEntry name
          while (entry ne null) {
            val sym = entry.sym
            if (sym.getFlag(requiredFlags) == requiredFlags) {
              val excl = sym.getFlag(excluded)
              if (excl == 0) {
                if (name.isTypeName || stableOnly) {
                  checkMalformedSwitch = savedCheckMalformedSwitch
                  if (util.Statistics.enabled)
                    findMemberMillis = findMemberMillis + currentTime - startTime
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
                    members = newScope(List(member, sym))
                  }
                } else {
                  var prevEntry = members lookupEntry sym.name
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
        } // while (!bcs.isEmpty)
        excluded = excludedFlags
      } // while (continue)
      checkMalformedSwitch = savedCheckMalformedSwitch
      if (util.Statistics.enabled)
        findMemberMillis = findMemberMillis + currentTime - startTime
      if (members eq null) {
        if (util.Statistics.enabled) if (member == NoSymbol) noMemberCount = noMemberCount + 1;
        member
      } else {
        if (util.Statistics.enabled) multMemberCount = multMemberCount + 1;
        //val pre = if (this.symbol.isClass) this.symbol.thisType else this;
        baseClasses.head.newOverloaded(this, members.toList)
      }
    }

    /** Add an attribute to this type */
    def withAttribute(attrib: AnnotationInfo) = withAttributes(List(attrib))

    /** Add a number of attributes to this type */
    def withAttributes(attribs: List[AnnotationInfo]): Type =
      attribs match {
        case Nil => this
        case _ => AnnotatedType(attribs, this)
      }

    /** Remove any attributes from this type */
    def withoutAttributes = this
  }

// Subclasses ------------------------------------------------------------

  trait UniqueType {
    private val hashcode = { val h = super.hashCode(); if (h < 0) -h else h }
    override def hashCode() = hashcode
  }

  /** A base class for types that defer some operations
   *  to their immediate supertype.
   */
  abstract class SubType extends Type {
    def supertype: Type
    override def parents: List[Type] = supertype.parents
    override def decls: Scope = supertype.decls
    override def baseType(clazz: Symbol): Type = supertype.baseType(clazz)
    override def closure: Array[Type] = supertype.closure
    override def closureDepth: Int = supertype.closureDepth
    override def baseClasses: List[Symbol] = supertype.baseClasses
    override def isNotNull = supertype.isNotNull
  }

  case class NotNullType(tp: Type) extends SubType with TypeProxy {
    override def supertype = tp
    override def isNotNull: Boolean = true
    override def deconst: Type = tp
    override def toString: String = supertype.toString + " with NotNull"
  }

  /** A base class for types that represent a single value
   *  (single-types and this-types).
   */
  abstract class SingletonType extends SubType {
    override def singleDeref: Type
    def supertype: Type = singleDeref
    override def isStable: Boolean = true
    override def widen: Type = singleDeref.widen
    override def closure: Array[Type] = {
      if (util.Statistics.enabled) singletonClosureCount = singletonClosureCount + 1
      addClosure(this, supertype.closure)
    }
    override def toString: String = prefixString + "type"
  }

  /** An object representing an erroneous type */
  case object ErrorType extends Type {
    // todo see whether we can do without
    override def isError: Boolean = true
    override def decls: Scope = new ErrorScope(NoSymbol)
    override def findMember(name: Name, excludedFlags: Int,
                            requiredFlags: Long, stableOnly: Boolean): Symbol = {
      var sym = decls lookup name
      if (sym == NoSymbol) {
        sym = NoSymbol.newErrorSymbol(name)
        decls enter sym
      }
      sym
    }
    override def baseType(clazz: Symbol): Type = this
    override def toString: String = "<error>"
    override def narrow: Type = this
    // override def isNullable: Boolean = true
  }

  /** An object representing an unknown type */
  case object WildcardType extends Type {
    override def toString: String = "?"
    // override def isNullable: Boolean = true
  }

  case class BoundedWildcardType(override val bounds: TypeBounds) extends Type {
    override def toString: String = "?" + bounds
  }

  /** An object representing a non-existing type */
  case object NoType extends Type {
    override def isTrivial: Boolean = true
    override def toString: String = "<notype>"
    // override def isNullable: Boolean = true
  }

  /** An object representing a non-existing prefix */
  case object NoPrefix extends Type {
    override def isTrivial: Boolean = true
    override def isStable: Boolean = true
    override def prefixString = ""
    override def toString: String = "<noprefix>"
    // override def isNullable: Boolean = true
  }

  /** A class for this-types of the form <sym>.this.type
   */
  case class ThisType(sym: Symbol) extends SingletonType {
    //assert(sym.isClass && !sym.isModuleClass || sym.isRoot, sym)
    override def isTrivial: Boolean = sym.isPackageClass
    override def isNotNull = true
    override def symbol = sym
    override def singleDeref: Type = sym.typeOfThis
    override def prefixString =
      if (settings.debug.value) sym.nameString + ".this."
      else if (sym.isRoot || sym.isEmptyPackageClass || sym.isInterpreterWrapper || sym.isScalaPackageClass) ""
      else if (sym.isAnonymousClass || sym.isRefinementClass) "this."
      else if (sym.isPackageClass) sym.fullNameString + "."
      else sym.nameString + ".this."
    override def toString: String =
      if (sym.isRoot) "<root>"
      else if (sym.isEmptyPackageClass) "<empty>"
      else super.toString
    override def narrow: Type = this
  }

  case class DeBruijnIndex(level: Int, paramId: Int) extends Type {
    override def isTrivial = true
    override def isStable = true
    override def toString = "<param "+level+"."+paramId+">"
  }

  /** A class for singleton types of the form &lt;prefix&gt;.&lt;sym.name&gt;.type.
   *  Cannot be created directly; one should always use
   *  `singleType' for creation.
   */
  case class SingleType(pre: Type, sym: Symbol) extends SingletonType {
    override val isTrivial: Boolean = pre.isTrivial
    // override def isNullable = supertype.isNullable
    override def isNotNull = supertype.isNotNull
    private var singleDerefCache: Type = _
    private var singleDerefPeriod = NoPeriod
    override def singleDeref: Type = {
      if (inIDE) return pre.memberType(sym).resultType
      val period = singleDerefPeriod
      if (period != currentPeriod) {
        singleDerefPeriod = currentPeriod
        if (!isValid(period)) {
          singleDerefCache = pre.memberType(sym).resultType;
        }
      }
      singleDerefCache
    }

    override def narrow: Type = {
      if (phase.erasedTypes) this
      else {
        val thissym = refinedType(List(this), sym.owner, EmptyScope).symbol
        if (sym.owner != NoSymbol) {
          //Console.println("narrowing module " + sym + thissym.owner);
          thissym.typeOfThis = this
        }
        thissym.thisType
      }
    }

    override def symbol = sym
    override def prefix: Type = pre
    override def prefixString: String =
      if ((sym.isEmptyPackage || sym.isInterpreterWrapper || sym.isPredefModule || sym.isScalaPackage) && !settings.debug.value) ""
      else pre.prefixString + sym.nameString + "."
  }

  case class SuperType(thistpe: Type, supertp: Type) extends SingletonType {
    override val isTrivial: Boolean = thistpe.isTrivial && supertp.isTrivial
    override def isNotNull = true;
    override def symbol = thistpe.symbol
    override def singleDeref = supertp
    override def prefix: Type = supertp.prefix
    override def prefixString =
      if (thistpe.prefixString.endsWith("this."))
        thistpe.prefixString.substring(0, thistpe.prefixString.length() - 5) + "super."
      else thistpe.prefixString;
    override def narrow: Type = thistpe.narrow
  }

  /** A class for the bounds of abstract types and type parameters
   */
  case class TypeBounds(lo: Type, hi: Type) extends SubType {
    override val isTrivial: Boolean = lo.isTrivial && hi.isTrivial
    def supertype: Type = hi
    override def bounds: TypeBounds = this
    def containsType(that: Type) = that <:< this || lo <:< that && that <:< hi;
    // override def isNullable: Boolean = AllRefClass.tpe <:< lo;
    override def toString = ">: " + lo + " <: " + hi
  }

  /** A common base class for intersection types and class types
   */
  abstract class CompoundType extends Type {

    private var closureCache: Array[Type] = _
    private var closurePeriod = NoPeriod
    private var baseClassesCache: List[Symbol] = _
    private var baseClassesPeriod = NoPeriod
    private var closureDepthCache: Int = _

    override def closure: Array[Type] = {
      def computeClosure: Array[Type] =
        try {
          if (util.Statistics.enabled)
            compoundClosureCount = compoundClosureCount + 1
          //Console.println("computing closure of " + symbol.tpe + " " + parents)//DEBUG
          val buf = new ListBuffer[Type]
          buf += symbol.tpe
          var clSize = 1
          val nparents = parents.length
          if (nparents != 0) {
            val pclosure = new Array[Array[Type]](nparents)
            val index = new Array[Int](nparents)
            var i = 0
            for (p <- parents) {
              pclosure(i) = if (p.closure eq null) AnyClass.info.closure // cyclic reference
                            else p.closure
              index(i) = 0
              i += 1
            }
            def nextBaseType(i: Int): Type = {
              val j = index(i)
              val pci = pclosure(i)
              if (j < pci.length) pci(j) else AnyClass.tpe
            }
            var minSym: Symbol = NoSymbol
            while (minSym != AnyClass) {
              minSym = nextBaseType(0).symbol
              i = 1
              while (i < nparents) {
                if (nextBaseType(i).symbol isLess minSym)
                  minSym = nextBaseType(i).symbol
                i += 1
              }
              var minTypes: List[Type] = List()
              i = 0
              while (i < nparents) {
                val tp = nextBaseType(i)
                if (tp.symbol == minSym) {
                  if (!(minTypes exists (tp =:=))) minTypes = tp :: minTypes;
                  index(i) = index(i) + 1
                }
                i = i + 1
              }
              buf += intersectionType(minTypes)
              clSize = clSize + 1
            }
          }
          closureCache = new Array[Type](clSize)
          buf.copyToArray(closureCache, 0)
          //Console.println("closureCache of " + symbol.tpe + " = " + List.fromArray(closureCache))//DEBUG
          var j = 0
          while (j < clSize) {
            closureCache(j) match {
              case RefinedType(parents, decls) =>
                if (!decls.isEmpty) assert(false, "computing closure of "+this+":"+this.isInstanceOf[RefinedType])
                //Console.println("compute closure of "+this+" => glb("+parents+")")
                closureCache(j) = mergePrefixAndArgs(parents, -1, maxClosureDepth(parents) + LubGlbMargin) match {
                  case Some(tp0) => tp0
                  case None => throw new MalformedClosure(parents)
                }
                assert(!closureCache(j).isInstanceOf[RefinedType], closureCache(j))
              case _ =>
            }
            j = j + 1
          }
          //Console.println("closure of " + symbol.tpe + " = " + List.fromArray(closureCache))//DEBUG
          closureCache
        } catch {
          case ex: MalformedClosure =>
            throw new MalformedType(
              "the type intersection " + this + " is malformed" +
              "\n --- because ---\n" + ex.getMessage())
        }
      val period = closurePeriod;
      if (period != currentPeriod) {
        closurePeriod = currentPeriod
        if (!isValidForBaseClasses(period)) {
          closureCache = null
          closureCache = memo[Array[Type], Type](computeClosure, modifyClosure(symbol.tpe))
          closureDepthCache = maxDepth(closureCache)
        }
        //Console.println("closure(" + symbol + ") = " + List.fromArray(closureCache));//DEBUG
      }
      if (closureCache eq null)
        throw new TypeError("illegal cyclic reference involving " + symbol)
      closureCache
    }

    override def closureDepth: Int = { closure; closureDepthCache }

    override def baseClasses: List[Symbol] = {
      if (inIDE) trackTypeIDE(symbol)
      def computeBaseClasses: List[Symbol] =
        if (parents.isEmpty) List(symbol)
        else {
          //Console.println("computing base classes of " + symbol + " at phase " + phase);//DEBUG
          // optimized, since this seems to be performance critical
          val superclazz = parents.head
          var mixins = parents.tail
          val sbcs = superclazz.baseClasses
          var bcs = sbcs
          def isNew(clazz: Symbol): Boolean = (
            superclazz.closurePos(clazz) < 0 &&
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
          symbol :: bcs
        }
      val period = baseClassesPeriod
      if (period != currentPeriod) {
        baseClassesPeriod = currentPeriod
        if (!isValidForBaseClasses(period)) {
          baseClassesCache = null
          baseClassesCache = memo[List[Symbol], Symbol](computeBaseClasses, x => symbol :: x.baseClasses.tail)
        }
      }
      if (baseClassesCache eq null)
        throw new TypeError("illegal cyclic reference involving " + symbol)
      baseClassesCache
    }

    def memo[A <: Seq[B], B](op1: => A, op2: Type => A) = intersectionWitness get parents match {
      case Some(w) =>
        if (w eq this) op1 else op2(w)
      case None =>
        intersectionWitness(parents) = this
        op1
    }

    override def baseType(sym: Symbol): Type = {
      if (inIDE) { trackTypeIDE(sym); trackTypeIDE(symbol); }
      val index = closurePos(sym)
      if (index >= 0) closure(index) else NoType
    }

    override def narrow: Type = {
      if (inIDE) trackTypeIDE(symbol)
      symbol.thisType
    }

    override def isNotNull: Boolean = parents exists (_.isNotNull)

    // override def isNullable: Boolean =
    // parents forall (p => p.isNullable && !p.symbol.isAbstractType);

    override def toString: String =
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
                         override val decls: Scope) extends CompoundType

  /** A class representing a class info
   */
  case class ClassInfoType(
    override val parents: List[Type],
    override val decls: Scope,
    override val symbol: Symbol) extends CompoundType
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
      for (tparam <- symbol.typeParams) {
        val enterRefs = new TypeMap {
          def apply(tp: Type): Type = {
            tp match {
              case TypeRef(_, sym, args) =>
                for ((tparam1, arg) <- sym.info.typeParams zip args)
                  if (arg contains tparam) {
                    addRef(NonExpansive, tparam, tparam1)
                    if (arg.symbol != tparam) addRef(Expansive, tparam, tparam1)
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
      for ((from, targets) <- refs(NonExpansive).elements)
        for (target <- targets) {
          var thatInfo = classInfo(target)
          if (thatInfo.state != Initialized)
            change = change | thatInfo.propagate()
          addRefs(NonExpansive, from, thatInfo.getRefs(NonExpansive, target))
          addRefs(Expansive, from, thatInfo.getRefs(Expansive, target))
        }
      for ((from, targets) <- refs(Expansive).elements)
        for (target <- targets) {
          var thatInfo = classInfo(target)
          addRefs(Expansive, from, thatInfo.getRefs(NonExpansive, target))
        }
      change = change || refs(0) != lastRefs(0) || refs(1) != lastRefs(1)
      if (change) state = Initializing
      //else Console.println("Propagate "+symbol+", final expansive = "+refs(Expansive)+", nonexpansive = "+refs(NonExpansive))//DEBUG
      change
    }

    // override def isNullable: Boolean =
    // symbol == AnyClass ||
    // symbol != AllClass && (symbol isSubClass ObjectClass) && !(symbol isSubClass NonNullClass);

    // override def isNonNull: Boolean = symbol == NonNullClass || super.isNonNull;
  }

  class PackageClassInfoType(decls: Scope, clazz: Symbol)
  extends ClassInfoType(List(), decls, clazz)

  /** A class representing a constant type.
   *
   *  @param value ...
   */
  case class ConstantType(value: Constant) extends SingletonType {
    assert(value.tpe.symbol != UnitClass)
    override def isTrivial: Boolean = true
    override def isNotNull = value.value != null
    override def symbol: Symbol = value.tpe.symbol
    override def singleDeref: Type =
      if (value.value.isInstanceOf[String]) value.tpe
      else value.tpe
    override def deconst: Type = value.tpe
    override def toString: String =
      value.tpe.toString + "(" + value.escapedStringValue + ")"
    // override def isNullable: Boolean = value.value eq null
    // override def isNonNull: Boolean = value.value ne null
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
    assert(!checkMalformedSwitch || !sym.isAbstractType || pre.isStable || pre.isError)
    assert(!pre.isInstanceOf[ClassInfoType], this)
    assert(!(sym hasFlag (PARAM | EXISTENTIAL)) || pre == NoPrefix, this)

    private var parentsCache: List[Type] = _
    private var parentsPeriod = NoPeriod
    private var closureCache: Array[Type] = _
    private var closurePeriod = NoPeriod
    private var closureDepthCache: Int = _

    override def isStable: Boolean = {
      sym == SingletonClass ||
      sym.isAbstractType && (sym.info.bounds.hi.symbol isSubClass SingletonClass)
    }

    override val isTrivial: Boolean =
      pre.isTrivial && !sym.isTypeParameter && args.forall(_.isTrivial)

    override def isNotNull =
      sym.isModuleClass || sym == AllClass || isValueClass(sym) || super.isNotNull

    // @M: propagate actual type params (args) to `tp', by replacing formal type parameters with actual ones
    def transform(tp: Type): Type =
      tp.asSeenFrom(pre, sym.owner).instantiateTypeParams(sym.typeParams, argsMaybeDummy)
      // @M TODO maybe we shouldn't instantiate type params if isHigherKinded -- probably needed for partial type application though

    //@M! use appliedType on the polytype that represents the bounds (or if aliastype, the rhs)
    def transformInfo(tp: Type): Type =
      appliedType(tp.asSeenFrom(pre, sym.owner), argsMaybeDummy)
      // TODO: argsMaybeDummy --> ok? or don't instantiate type params if isHigherKinded

    def thisInfo     = if (sym.isTypeMember) transformInfo(sym.info) else sym.info
    def relativeInfo = if (sym.isTypeMember) transformInfo(pre.memberInfo(sym)) else pre.memberInfo(sym)

    def transform(cl: Array[Type]): Array[Type] = {
      val cl1 = new Array[Type](cl.length)
      var i = 0
      while (i < cl.length) {
        cl1(i) = transform(cl(i))
        i = i + 1
      }
      cl1
    }

    override def symbol = if (sym.isAliasType) normalize.symbol else sym
/* @MAT
whenever you see `tp.symbol.isXXXX' and then act on tp based on that predicate, you're on thin ice,
as `symbol' (and `prefix') automatically normalize, but the other inspectors don't.
In other words, even if `tp.normalize.sym.isXXX' is true, `tp.sym.isXXX' may be false (if sym were a public method to access the non-normalized symbol)...

In retrospect, I think `tp.symbol.isXXX' or (worse) `tp.symbol==XXX' should be replaced by `val tp = tp0.asXXX'.
A type's symbol should never be inspected directly.
*/

    override def bounds: TypeBounds =
      if (sym.isAbstractType) transform(thisInfo.bounds).asInstanceOf[TypeBounds]
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

    override def narrow =
      if (sym.isModuleClass) transform(sym.thisType)
      else if (sym.isAliasType) normalize.narrow
      else super.narrow

    override def prefix: Type = if (sym.isAliasType) normalize.prefix else pre

    override def typeArgs: List[Type] = args

    override def typeParams: List[Symbol] =
      if (args.isEmpty) sym.unsafeTypeParams else List()
         // @MAT was symbol.unsafeTypeParams, but symbol normalizes now

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

    override def normalize =
      if (sym.isAliasType) {
        if (sym.info.typeParams.length == args.length) // beta-reduce  -- check if the info has been loaded, if not, the arity check is meaningless
          // Martin to Adriaan: I believe sym.info.isComplete is redundant here
          // @M: correct: it was a remnant of a previous fix for the problem in the last else {} branch
          transform(sym.info.resultType).normalize // cycles have been checked in typeRef
        else if (isHigherKinded)
          PolyType(typeParams, transform(sym.info.resultType).normalize)
        else {
          //log("Error: normalizing "+sym.rawname+" with mismatch between type params "+sym.info.typeParams+" and args "+args)
          //this
          transform(sym.info.resultType).normalize // technically wrong, but returning `this' is even worse (cycle!)
          // only happens when compiling `val x: Class' with -Xgenerics,
          // when `type Class = java.lang.Class' has already been compiled (without -Xgenerics)
        }
      } else if (isHigherKinded) {
        PolyType(typeParams, typeRef(pre, sym, higherKindedArgs)) // @M TODO: transform?
      } else super.normalize // @M TODO: transform?

    override def decls: Scope = {
      sym.info match {
        case TypeRef(_, sym1, _) =>
          assert(sym1 != sym, this) // @MAT was != symbol
        case _ =>
      }
      thisInfo.decls
    }

    override def baseType(clazz: Symbol): Type =
      if (sym == clazz) this
      else if (sym.isClass) transform(sym.info.baseType(clazz))
      else relativeInfo.baseType(clazz) // @M! use relativeInfo instead of pre.memberInfo(sym)

    override def closure: Array[Type] = {
      val period = closurePeriod
      if (period != currentPeriod) {
        closurePeriod = currentPeriod
        if (!isValidForBaseClasses(period)) {
          if (util.Statistics.enabled)
            typerefClosureCount = typerefClosureCount + 1
          closureCache =
            if (sym.isAbstractType) addClosure(this, transform(bounds.hi).closure)
            else transform(sym.info.closure)
          closureDepthCache = maxDepth(closureCache)
        }
      }
      if (closureCache eq null)
        throw new TypeError("illegal cyclic reference involving " + sym)
      closureCache
    }

    override def closureDepth: Int = { closure; closureDepthCache }

    override def baseClasses: List[Symbol] = thisInfo.baseClasses

    // override def isNullable: Boolean = sym.info.isNullable

    override def toString: String = {
      if (!settings.debug.value) {
        if (sym == RepeatedParamClass && !args.isEmpty)
          return args(0).toString + "*"
        if (sym == ByNameParamClass && !args.isEmpty)
          return "=> " + args(0).toString
        if (isFunctionType(this))
          return normalize.typeArgs.init.mkString("(", ", ", ")") + " => " + normalize.typeArgs.last
        if (isTupleType(this))
          return args.mkString("(", ", ", if (args.length == 1) ",)" else ")")
      }
      val str = (pre.prefixString + sym.nameString +
                 (if (args.isEmpty) "" else args.mkString("[", ",", "]")))
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
  }

  /** A class representing a method type with parameters.
   */
  case class MethodType(override val paramTypes: List[Type],
                        override val resultType: Type) extends Type {
    override val isTrivial: Boolean =
      paramTypes.forall(_.isTrivial) && resultType.isTrivial

    //assert(paramTypes forall (pt => !pt.symbol.isImplClass))//DEBUG
    override def paramSectionCount: Int = resultType.paramSectionCount + 1

    override def resultType(actuals: List[Type]) =
      new InstantiateDeBruijnMap(actuals).apply(resultType)

    override def finalResultType: Type = resultType.finalResultType

    protected def paramPrefix = "("

    private def dependentToString(base: Int): String = {
      val params = for ((pt, n) <- paramTypes.zipWithIndex) yield "x$"+n+":"+pt
      val res = resultType match {
        case mt: MethodType => mt.dependentToString(base + params.length)
        case rt => rt.toString
      }
      params.mkString(paramPrefix, ",", ")")+res
    }

    override def toString: String =
      if (resultType.isDependent) dependentToString(0)
      else paramTypes.mkString(paramPrefix, ",", ")") + resultType
  }

  class ImplicitMethodType(pts: List[Type], rt: Type) extends MethodType(pts, rt) {
    override protected def paramPrefix = "(implicit "
  }

  class JavaMethodType(pts: List[Type], rt: Type) extends MethodType(pts, rt)

  /** A class representing a polymorphic type or, if tparams.length == 0,
   *  a parameterless method type.
   *  (@M: note that polymorphic nullary methods have non-empty tparams,
   *   e.g., isInstanceOf or def makeList[T] = new List[T].
   *   Ideally, there would be a NullaryMethodType, so that higher-kinded types
   *   could use PolyType instead of TypeRef with empty args)
   */
  case class PolyType(override val typeParams: List[Symbol], override val resultType: Type)
       extends Type {

    override def paramSectionCount: Int = resultType.paramSectionCount
    override def paramTypes: List[Type] = resultType.paramTypes
    override def parents: List[Type] = resultType.parents
    override def decls: Scope = resultType.decls
    override def symbol: Symbol = resultType.symbol
    override def prefix: Type = resultType.prefix
    override def closure: Array[Type] = resultType.closure
    override def closureDepth: Int = resultType.closureDepth
    override def baseClasses: List[Symbol] = resultType.baseClasses
    override def baseType(clazz: Symbol): Type = resultType.baseType(clazz)
    override def narrow: Type = resultType.narrow

    override def finalResultType: Type = resultType.finalResultType

    /** @M: abstractTypeSig now wraps a TypeBounds in a PolyType
     *  to represent a higher-kinded type parameter
     *  wrap lo&hi in polytypes to bind variables
     */
    override def bounds: TypeBounds =
      TypeBounds(PolyType(typeParams, resultType.bounds.lo),
                 PolyType(typeParams, resultType.bounds.hi))

    override def isHigherKinded = !typeParams.isEmpty

    override def toString: String =
      (if (typeParams.isEmpty) "=> "
       else (typeParams map (_.defString) mkString ("[", ",", "]")))+resultType

    override def cloneInfo(owner: Symbol) = {
      val tparams = cloneSymbols(typeParams, owner)
      PolyType(tparams, resultType.substSym(typeParams, tparams))
    }
  }

  case class ExistentialType(override val typeParams: List[Symbol],
                             val quantified: Type) extends TypeProxy
  {
    val tp = quantified
    override protected def rewrap(newtp: Type) = existentialAbstraction(typeParams, newtp)

    override def isStable: Boolean = false
    override def bounds = TypeBounds(maybeRewrap(tp.bounds.lo), maybeRewrap(tp.bounds.hi))
    override def parents = tp.parents map maybeRewrap
    override def prefix = maybeRewrap(tp.prefix)
    override def typeArgs = tp.typeArgs map maybeRewrap
    override def paramTypes = tp.paramTypes map maybeRewrap
    override def instantiateTypeParams(formals: List[Symbol], actuals: List[Type]) =
      maybeRewrap(tp.instantiateTypeParams(formals, actuals))
    override def baseType(clazz: Symbol) = maybeRewrap(tp.baseType(clazz))
    override def closure = tp.closure map maybeRewrap

    override def skolemizeExistential(owner: Symbol, origin: AnyRef) = {
      def mkSkolem(tparam: Symbol): Symbol = {
        val skolem = new TypeSkolem(
          if (owner == NoSymbol) tparam.owner else owner,
          tparam.pos, tparam.name, origin)
        skolem.setInfo(tparam.info.cloneInfo(skolem))
              .setFlag(tparam.flags | EXISTENTIAL)
              .resetFlag(PARAM)
      }
      val skolems = typeParams map mkSkolem
      for (skolem <- skolems)
        skolem setInfo skolem.info.substSym(typeParams, skolems)
      quantified.substSym(typeParams, skolems)
    }

    override def toString: String =
      quantified+(typeParams map tparamToString mkString(" forSome { ", "; ", " }"))

    private def tparamToString(tparam: Symbol) = {
      val tname = tparam.name.toString
      if ((tname endsWith ".type") && (tparam.info.bounds.hi.symbol isSubClass SingletonClass) &&
          !settings.debug.value)
        "val "+tname.substring(0, tname.length - 5)+": "+dropSingletonType(tparam.info.bounds.hi)
      else tparam.defString
    }

    override def cloneInfo(owner: Symbol) = {
      val tparams = cloneSymbols(typeParams, owner)
      ExistentialType(tparams, quantified.substSym(typeParams, tparams))
    }
  }

  /** A class containing the alternatives and type prefix of an overloaded symbol.
   *  Not used after phase `typer'.
   */
  case class OverloadedType(pre: Type, alternatives: List[Symbol]) extends Type {
    override def prefix: Type = pre
    override def toString =
      (alternatives map pre.memberType).mkString("", " <and> ", "")
  }

  /** A class remembering a type instantiation for some a set of overloaded
   *  polymorphic symbols.
   *  Not used after phase `typer'.
   */
  case class AntiPolyType(pre: Type, targs: List[Type]) extends Type {
    override def toString =
      pre.toString + targs.mkString("(with type arguments ", ",", ")");
    override def memberType(sym: Symbol) = pre.memberType(sym) match {
      case PolyType(tparams, restp) => restp.subst(tparams, targs)
      case ErrorType => ErrorType
    }
  }

  /** A class representing a type variable
   *  Not used after phase `typer'.
   */
  case class TypeVar(origin: Type, constr: TypeConstraint) extends Type {
    //constr.self = this //DEBUG
    override def symbol = origin.symbol
    override def toString: String =
      if (constr.inst eq null) "<null " + origin + ">"
      else if (constr.inst eq NoType) "?*" + origin
      else constr.inst.toString;
    override def isStable = origin.isStable
  }

  /** A type carrying some attributes.  The attributes have no significance
    * to the core compiler, but can be observed by type-system plugins.  The
    * core compiler does take care to propagate attributes and to save them
    * in the symbol tables of object files. */
  case class AnnotatedType(attributes: List[AnnotationInfo], tp: Type) extends TypeProxy {
    override def toString: String = {
      val attString =
        if (attributes.isEmpty)
          ""
        else
          attributes.mkString("@", " @", " ")

      attString + tp
    }


    /** Add a number of attributes to this type */
    override def withAttributes(attribs: List[AnnotationInfo]): Type =
      AnnotatedType(attribs:::this.attributes, this)

    /** Remove any attributes from this type */
    override def withoutAttributes = tp.withoutAttributes

    /** Martin to Lex: I don't understand what the following 2 method do? */
    override def bounds: TypeBounds = {
       val oftp = tp.bounds
       oftp match {
         case TypeBounds(lo, hi) if ((lo eq this) && (hi eq this)) => mkTypeBounds(this,this)
         case _ => oftp
       }
    }
    override def closure: Array[Type] = {
       val oftp = tp.closure
       if ((oftp.length == 1 &&) (oftp(0) eq this))
         Array(this)
       else
         oftp
     }
  }

  /** A class representing an as-yet unevaluated type.
   */
  abstract class LazyType extends Type {
    override def isComplete: Boolean = false
    override def complete(sym: Symbol)
    override def toString = "<?>"
  }

// Creators ---------------------------------------------------------------

  /** Rebind symbol `sym' to an overriding member in type
   *  `pre'.
   */
  private def rebind(pre: Type, sym: Symbol): Symbol = {
    val owner = sym.owner
    if (owner.isClass && owner != pre.symbol && !sym.isFinal && !sym.isClass) {
      //Console.println("rebind "+pre+" "+sym)//DEBUG
      val rebind = pre.nonPrivateMember(sym.name).suchThat(sym => sym.isType || sym.isStable)
      if (rebind == NoSymbol) sym else rebind
    } else sym
  }

  /** Convert a `super' prefix to a this-type if `sym'
   *  is abstract or final.
   */
  private def removeSuper(tp: Type, sym: Symbol): Type = tp match {
    case SuperType(thistp, _) =>
      if (sym.isFinal || sym.hasFlag(DEFERRED)) thistp
      else tp
    case _ =>
      tp
  }

  /** The canonical creator for this-types */
  def mkThisType(sym: Symbol): Type =
    if (phase.erasedTypes) sym.tpe else unique(new ThisType(sym) with UniqueType)

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
      if (checkMalformedSwitch && !pre1.isStable && !pre1.isError)
        throw new MalformedType(pre, sym.nameString)
      else
        unique(new SingleType(pre1, sym1) with UniqueType)
    }
  }

  /** The canonical creator for super-types */
  def mkSuperType(thistp: Type, supertp: Type): Type =
    if (phase.erasedTypes) supertp
    else unique(new SuperType(thistp, supertp) with UniqueType)

  /** The canonical creator for type bounds */
  def mkTypeBounds(lo: Type, hi: Type): TypeBounds =
    unique(new TypeBounds(lo, hi) with UniqueType)

  def refinementOfClass(clazz: Symbol, parents: List[Type], decls: Scope) =
    new RefinedType(parents, decls) { override def symbol: Symbol = clazz }

  /** the canonical creator for a refined type with a given scope */
  def refinedType(parents: List[Type], owner: Symbol, decls: Scope): Type = {
    if (phase.erasedTypes)
      if (parents.isEmpty) ObjectClass.tpe else parents.head
    else {
      val clazz = owner.newRefinementClass(NoPosition)
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
    refinedType(parents, owner, newScope)

  def copyRefinedType(original: RefinedType, parents: List[Type], decls: Scope) =
    if ((parents eq original.parents) && (decls eq original.decls)) original
    else {
      val result = refinedType(parents, original.symbol.owner)
      val syms1 = decls.toList
      for (sym <- syms1)
        result.decls.enter(sym.cloneSymbol(result.symbol))
      val syms2 = result.decls.toList
      val resultThis = result.symbol.thisType
      for (sym <- syms2)
        sym.setInfo(sym.info.substSym(syms1, syms2).substThis(original.symbol, resultThis))
      result
    }

  /** the canonical creator for a constant type */
  def mkConstantType(value: Constant): ConstantType =
    unique(new ConstantType(value) with UniqueType)

  /** The canonical creator for typerefs */
  def typeRef(pre: Type, sym: Symbol, args: List[Type]): Type = {
    var sym1 = if (sym.isAbstractType) rebind(pre, sym) else sym
    def transform(tp: Type): Type =
      tp.resultType.asSeenFrom(pre, sym1.owner).instantiateTypeParams(sym1.typeParams, args)

    if (sym1.isAliasType && sym1.info.typeParams.length == args.length) {
      // note: we require that object is initialized,
      // that's why we use info.typeParams instead of typeParams.
      if (sym1.hasFlag(LOCKED))
        throw new TypeError("illegal cyclic reference involving " + sym1)
      sym1.setFlag(LOCKED)
      val result = transform(sym1.info)
      sym1.resetFlag(LOCKED)
      //result // @M: original version -- this would expand the type alias immediately
      rawTypeRef(pre, sym1, args) //@MAT -- don't expand type alias, but still check there are no cycles
    } else {
      val pre1 = removeSuper(pre, sym1)
      if (pre1 ne pre) {
        if (sym1.isAbstractType) sym1 = rebind(pre1, sym1)
        typeRef(pre1, sym1, args)
      } else if (checkMalformedSwitch && !pre.isStable && !pre.isError &&
                 (sym1.isAbstractType /* || !pre.widen.symbol.isStableClass*/)) {
        throw new MalformedType(pre, sym1.nameString)
      } else if (sym1.isClass && pre.isInstanceOf[CompoundType]) {
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
  def rawTypeRef(pre: Type, sym: Symbol, args: List[Type]): Type =
    unique(new TypeRef(pre, sym, args) with UniqueType)

  /** The canonical creator for implicit method types */
  def ImplicitMethodType(paramTypes: List[Type], resultType: Type): ImplicitMethodType =
    new ImplicitMethodType(paramTypes, resultType) // don't unique this!

  /** The canonical creator for implicit method types */
  def JavaMethodType(paramTypes: List[Type], resultType: Type): JavaMethodType =
    new JavaMethodType(paramTypes, resultType) // don't unique this!

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
          val tps1a = tps1 filter (_.symbol.==(tp.symbol))
          val tps1b = tps1 filter (_.symbol.!=(tp.symbol))
          mergePrefixAndArgs(tps1a, -1) match {
            case Some(tp1) => tp1 :: merge(tps1b)
            case None => throw new MalformedType(
              "malformed type: "+refinedType(tps, owner)+" has repeated parent class "+
              tp.symbol+" with incompatible prefixes or type arguments")
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
      case ErrorType => tycon
      case st: SingletonType => appliedType(st.widen, args) // @M TODO: what to do? see bug1
      case _ =>
        Console.println(tycon.getClass())
        Console.println(tycon.$tag())
        throw new Error()
    }

  /** A creator for type parameterizations
   *  If tparams is empty, simply returns result type
   */
  def parameterizedType(tparams: List[Symbol], tpe: Type): Type =
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
   *  If there are no such type parameters, simply returns result type `tpe'.
   */
  def existentialAbstraction(tparams: List[Symbol], tpe: Type): Type =
    if (tparams.isEmpty) tpe
    else {
      val extrapolate = new TypeMap {
        variance = 1
        stableNeeded = false
        def apply(tp: Type): Type = {
          val tp1 = mapOver(tp)
          tp1 match {
            case TypeRef(pre, sym, args) if (tparams contains sym) && (variance != 0) =>
              val repl = if (variance == 1) dropSingletonType(tp1.bounds.hi) else tp1.bounds.lo
              if ((!stableNeeded || repl.isStable) && !(tparams exists (repl.contains))) repl
              else tp1
            case _ =>
              tp1
          }
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

  /** Remove any occurrence of type <singleton> from this type and its parents */
  private object dropSingletonType extends TypeMap {
    def apply(tp: Type): Type = {
      tp match {
        case TypeRef(_, sym, _) if (sym == SingletonClass) =>
          AnyClass.tpe
        case tp1 @ RefinedType(parents, decls) =>
          var parents1 = parents filter (_.symbol != SingletonClass)
          if (parents1.isEmpty) parents1 = List(AnyClass.tpe)
          if (parents1.tail.isEmpty && decls.isEmpty) mapOver(parents1.head)
          else mapOver(copyRefinedType(tp1, parents1, decls))
        case tp1 =>
          mapOver(tp1)
      }
    }
  }

// Hash consing --------------------------------------------------------------

  private var uniques: HashSet[AnyRef] = _
  private var uniqueRunId = NoRunId

  def uniqueTypeCount = uniques.size // for statistics

  private def unique[T <: AnyRef](tp: T): T = {
    if (uniqueRunId != currentRunId) {
      uniques = new HashSet(20000)
      uniqueRunId = currentRunId
    }
    val tp1 = uniques.findEntry(tp)
    if (tp1 eq null) {
      uniques.addEntry(tp); tp
    } else {
      tp1.asInstanceOf[T]
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
/* debug
    private var _inst: Type = NoType
    def inst = _inst
    def inst_=(tp: Type) {
      assert(tp == null || !(tp containsTp self), tp)
      _inst = tp
    }
*/

    def instantiate(tp: Type): Boolean =
      if (lobounds.forall(_ <:< tp) && hibounds.forall(tp <:< _)) {
        inst = tp; true
      } else false

    override def toString =
      lobounds.mkString("[ _>:(", ",", ") ") +
      hibounds.mkString("| _<:(", ",", ") | _= ") + inst
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

    /** Is a stable type needed here?
     */
    var stableNeeded = false

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
          val v = variance; variance = 0
          val s = stableNeeded; stableNeeded = true
          val pre1 = this(pre)
          variance = v
          stableNeeded = s
          if (pre1 eq pre) tp
          else singleType(pre1, sym)
        }
      case SuperType(thistp, supertp) =>
        val thistp1 = this(thistp)
        val supertp1 = this(supertp)
        if ((thistp1 eq thistp) && (supertp1 eq supertp)) tp
        else mkSuperType(thistp1, supertp1)
      case TypeRef(pre, sym, args) =>
        val s = stableNeeded; stableNeeded = true
        val pre1 = this(pre)
        stableNeeded = s
        //val args1 = List.mapConserve(args)(this)
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
        //else refinementOfClass(tp.symbol, parents1, decls1)
        copyRefinedType(rtp, parents1, decls1)
/*
      case ClassInfoType(parents, decls, clazz) =>
        val parents1 = List.mapConserve(parents)(this);
        val decls1 = mapOver(decls);
        if ((parents1 eq parents) && (decls1 eq decls)) tp
        else cloneDecls(ClassInfoType(parents1, new Scope(), clazz), tp, decls1)
*/
      case MethodType(paramtypes, result) =>
        variance = -variance
        val paramtypes1 = List.mapConserve(paramtypes)(this)
        variance = -variance
        val result1 = this(result)
        if ((paramtypes1 eq paramtypes) && (result1 eq result)) tp
        else copyMethodType(tp, paramtypes1, result1)
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
      case AnnotatedType(attribs, atp) =>
        val atp1 = this(atp)
        if (atp1 eq atp) tp
        else AnnotatedType(attribs, atp1)
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
    def mapOver(syms: List[Symbol]): List[Symbol] = {
      val infos = syms map (_.info)
      val infos1 = List.mapConserve(infos)(this)
      if (infos1 eq infos) syms
      else {
        val syms1 = syms map (_.cloneSymbol)
        List.map2(syms1, infos1) {
          ((sym1, info1) => sym1.setInfo(info1.substSym(syms, syms1)))
        }
      }
    }

    protected def copyMethodType(tp: Type, formals: List[Type], restpe: Type): Type = tp match {
      case _: ImplicitMethodType => ImplicitMethodType(formals, restpe)
      case _: JavaMethodType => JavaMethodType(formals, restpe)
      case _ => MethodType(formals, restpe)
    }
  }

  abstract class TypeTraverser extends TypeMap {
    def traverse(tp: Type): TypeTraverser //todo: return Unit instead?
    def apply(tp: Type): Type = { traverse(tp); tp }
  }

  /** A map to compute the asSeenFrom method  */
  class AsSeenFromMap(pre: Type, clazz: Symbol) extends TypeMap {
    var capturedParams: List[Symbol] = List()
    /** Return pre.baseType(clazz), or if that's NoType and clazz is a refinement, pre itself.
     *  See bug397.scala for an example where the second alternative is needed.
     *  The problem is that when forming the closure of an abstract type,
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
                     (pre.widen.symbol isNonBottomSubClass sym))
              pre match {
                case SuperType(thistp, _) => thistp
                case _ => pre
              }
            else toPrefix(base(pre, clazz).prefix, clazz.owner);
          toPrefix(pre, clazz)
        case SingleType(pre, sym) =>
          if (sym.isPackageClass) tp // short path
          else {
            val v = variance; variance = 0
            val pre1 = this(pre)
            variance = v
            if (pre1 eq pre) tp
            else if (pre1.isStable) singleType(pre1, sym)
            else pre1.memberType(sym).resultType
          }
        case TypeRef(prefix, sym, args) if (sym.isTypeParameter) =>
          def toInstance(pre: Type, clazz: Symbol): Type =
            if ((pre eq NoType) || (pre eq NoPrefix) || !clazz.isClass) mapOver(tp) //@M! see test pos/tcpoly_return_overriding.scala why mapOver is necessary
            else {
              val symclazz = sym.owner;
              def throwError =
                throw new Error("" + tp + " in " + symclazz +
                                " cannot be instantiated from " + pre.widen);
              def instParam(ps: List[Symbol], as: List[Type]): Type =
                if (ps.isEmpty) throwError
                else if (sym eq ps.head)  // @M! don't just replace the whole thing, might be followed by type application
                  appliedType(as.head, List.mapConserve(args)(this)) // @M: was as.head
                else instParam(ps.tail, as.tail);
              if (symclazz == clazz && (pre.widen.symbol isNonBottomSubClass symclazz))
                pre.baseType(symclazz) match {
                  case TypeRef(_, basesym, baseargs) =>
                    //Console.println("instantiating " + sym + " from " + basesym + " with " + basesym.typeParams + " and " + baseargs+", pre = "+pre+", symclazz = "+symclazz);//DEBUG
                    if (basesym.typeParams.length != baseargs.length)
                      throw new TypeError(
                        "something is wrong (wrong class file?): "+basesym+
                        " with type parameters "+
                        basesym.typeParams.map(_.name).mkString("[",",","]")+
                        " gets applied to arguments "+baseargs.mkString("(",",",")")+", phase = "+phase)
                    instParam(basesym.typeParams, baseargs);
                  case ExistentialType(tparams, qtpe) =>
                    capturedParams = capturedParams union tparams
                    toInstance(qtpe, clazz)
                  case _ =>
                    throwError
                }
              else toInstance(base(pre, clazz).prefix, clazz.owner)
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

    def apply(tp0: Type): Type = if (from.isEmpty) tp0 else {
      val tp = mapOver(tp0)

      def subst(sym: Symbol, from: List[Symbol], to: List[T]): Type =
        if (from.isEmpty) tp
        else if (to.isEmpty && inIDE) throw new TypeError(NoPosition, "type parameter list problem");
        else if (matches(from.head, sym)) toType(tp, to.head)
        else subst(sym, from.tail, to.tail)

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
          subst(sym, from, to) match {
            case r @ TypeRef(pre1, sym1, args1) =>
              if (args.isEmpty) r
              else rawTypeRef(pre1, sym1, args)
            case r =>
              r
          }
        case SingleType(NoPrefix, sym) =>
          subst(sym, from, to)
        case PolyType(tparams, restp) =>
          assert(!(tparams exists (from contains)))
          tp
        case ExistentialType(tparams, restp) =>
          if (tparams exists (from contains))
            assert(false, "["+from.mkString(",")+":="+to.mkString(",")+"]"+tp)
          tp
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
  }

  /** A map to implement the `subst' method. */
  class SubstTypeMap(from: List[Symbol], to: List[Type])
  extends SubstMap(from, to) {
    protected def toType(fromtp: Type, tp: Type) = tp
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

  class InstantiateDeBruijnMap(actuals: List[Type]) extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case DeBruijnIndex(level, pid) =>
        if (level == 1)
          if (pid < actuals.length) actuals(pid) else tp
        else DeBruijnIndex(level - 1, pid)
      case _ =>
        mapOver(tp)
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

  /** A map to implement the `contains' method */
  class ContainsTraverser(sym: Symbol) extends TypeTraverser {
    var result = false
    def traverse(tp: Type): ContainsTraverser = {
      if (!result) {
        tp.normalize match {
          case TypeRef(_, sym1, _) if (sym == sym1) => result = true
          case SingleType(_, sym1) if (sym == sym1) => result = true
          case _ => mapOver(tp)
        }
      }
      this
    }
  }

  /** A map to implement the `contains' method */
  class ContainsTypeTraverser(t: Type) extends TypeTraverser {
    var result = false
    def traverse(tp: Type): ContainsTypeTraverser = {
      if (!result) {
        if (tp eq t) result = true
        else mapOver(tp)
      }
      this
    }
  }

  /** A map to implement the `filter' method */
  class FilterTraverser(p: Type => Boolean) extends TypeTraverser {
    val hits = new ListBuffer[Type]
    def traverse(tp: Type): FilterTraverser = {
      if (p(tp)) hits += tp
      mapOver(tp)
      this
    }
  }

  class ForEachTraverser(f: Type => Unit) extends TypeTraverser {
    def traverse(tp: Type): TypeTraverser = {
      f(tp)
      mapOver(tp)
      this
    }
  }

  /** A map to implement the `filter' method */
  class FindTraverser(p: Type => Boolean) extends TypeTraverser {
    var result: Option[Type] = None
    def traverse(tp: Type): FindTraverser = {
      if (result.isEmpty) {
        if (p(tp)) result = Some(tp)
        mapOver(tp)
      }
      this
    }
  }

  /** A map to implement the `contains' method */
  object ErroneousTraverser extends TypeTraverser {
    var result: Boolean = _
    def traverse(tp: Type): TypeTraverser = {
      if (!result) {
        result = tp.isError
        mapOver(tp)
      }
      this
    }
  }

  object IsDependentTraverser extends TypeTraverser {
    var result: Boolean = _
    def traverse(tp: Type): TypeTraverser = {
      tp match {
        case DeBruijnIndex(_, _) => result = true
        case _ => if (!result) mapOver(tp)
      }
      this
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
        var rebind0 = pre.findMember(sym.name, BRIDGE, 0, true)
/*
        if (rebind0 == NoSymbol && (sym hasFlag EXPANDEDNAME)) {
          // problem is that symbols with expanded names might be in the wrong hash bucket
          // in a previous scope. We account for that by re-creating the hash as a last attempt.
          sym.owner.info.decls.createHash()
          rebind0 = pre.findMember(sym.name, BRIDGE, 0, true)
        }
*/
        if (rebind0 == NoSymbol) { assert(false, ""+pre+"."+sym+" does no longer exist, phase = "+phase) }
        /** The two symbols have the same fully qualified name */
        def corresponds(sym1: Symbol, sym2: Symbol): Boolean =
          sym1.name == sym2.name && (sym1.isPackageClass || corresponds(sym1.owner, sym2.owner))
        if (!corresponds(sym.owner, rebind0.owner)) {
          if (settings.debug.value) Console.println("ADAPT1 pre = "+pre+", sym = "+sym+sym.locationString+", rebind = "+rebind0+rebind0.locationString)
          val bcs = pre.baseClasses.dropWhile(bc => !corresponds(bc, sym.owner));
          assert(!bcs.isEmpty)
          rebind0 = pre.baseType(bcs.head).member(sym.name)
          if (settings.debug.value) Console.println("ADAPT2 pre = "+pre+", bcs.head = "+bcs.head+", sym = "+sym+sym.locationString+", rebind = "+rebind0+(if (rebind0 == NoSymbol) "" else rebind0.locationString))
        }
        val rebind = rebind0.suchThat(sym => sym.isType || sym.isStable)
        if (rebind == NoSymbol) {
          if (settings.debug.value) Console.println("" + phase + " " + phase.flatClasses+sym.owner+sym.name)
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
      case PolyType(tparams, restp) =>
        val restp1 = this(restp)
        if (restp1 eq restp) tp
        else PolyType(tparams, restp1)
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
        else refinedType(parents1, tp.symbol.owner, decls)
      case SuperType(_, _) => mapOver(tp)
      case TypeBounds(_, _) => mapOver(tp)
      case MethodType(_, _) => mapOver(tp)
      case TypeVar(_, _) => mapOver(tp)
      case AnnotatedType(_,_) => mapOver(tp)
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

  import Math.max

  private var nextid = 0
  private def freshTypeName() = {
    nextid += 1
    newTypeName("_"+nextid)
  }

  /** The maximum depth of all types in the closures of each of the types `tps' */
  final def maxClosureDepth(tps: Seq[Type]): Int = {
    var d = 0
    for (tp <- tps) d = max(d, tp.closureDepth)
    d
  }

  /** The maximum depth of all types `tps' */
  final def maxDepth(tps: Seq[Type]): Int = {
    var d = 0
    for (tp <- tps) d = max(d, maxDepth(tp))
    d
  }

  /** The maximum depth of type `tp' */
  final def maxDepth(tp: Type): Int = tp match {
    case TypeRef(pre, sym, args) =>
      max(maxDepth(pre), maxDepth(args) + 1)
    case RefinedType(parents, decls) =>
      max(maxDepth(parents), maxDepth(decls.toList.map(_.info)) + 1)
    case TypeBounds(lo, hi) =>
      max(maxDepth(lo), maxDepth(hi))
    case MethodType(paramtypes, result) =>
      maxDepth(result)
    case PolyType(tparams, result) =>
      max(maxDepth(result), maxDepth(tparams map (_.info)) + 1)
    case ExistentialType(tparams, result) =>
      max(maxDepth(result), maxDepth(tparams map (_.info)) + 1)
    case _ =>
      1
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

  /** Do `tp1' and `tp2' denote equivalent types?
   *
   *  @param tp1 ...
   *  @param tp2 ...
   *  @return    true, iff `tp1' and `tp2' denote
   *             equivalent types.
   */
  def isSameType(tp1: Type, tp2: Type): Boolean = {
    (tp1, tp2) match {
      case (ErrorType, _) => true
      case (WildcardType, _) => true
      case (_, ErrorType) => true
      case (_, WildcardType) => true

      case (NoType, _) => false
      case (NoPrefix, _) => tp2.symbol.isPackageClass
      case (_, NoType) => false
      case (_, NoPrefix) => tp1.symbol.isPackageClass

      case (ThisType(sym1), ThisType(sym2))
      if (sym1 == sym2) =>
        true
      case (SingleType(pre1, sym1), SingleType(pre2, sym2))
      if ((sym1 == sym2) && (pre1 =:= pre2)) =>
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
        sym1 == sym2 && (phase.erasedTypes || pre1 =:= pre2) &&
          // @M! normalize reduces higher-kinded case to PolyType's
        ((tp1.isHigherKinded && tp2.isHigherKinded && tp1.normalize =:= tp2.normalize)
         || isSameTypes(args1, args2))
      case (RefinedType(parents1, ref1), RefinedType(parents2, ref2)) =>
        def isSubScope(s1: Scope, s2: Scope): Boolean = s2.toList.forall {
          sym2 =>
            val sym1 = s1.lookup(sym2.name)
            sym1 != NoSymbol &&
            sym1.info =:= sym2.info.substThis(sym2.owner, sym1.owner.thisType)
        }
        //Console.println("is same? " + tp1 + " " + tp2 + " " + tp1.symbol.owner + " " + tp2.symbol.owner)//DEBUG
        isSameTypes(parents1, parents2) && isSubScope(ref1, ref2) && isSubScope(ref2, ref1)
      case (MethodType(pts1, res1), MethodType(pts2, res2)) =>
        (pts1.length == pts2.length &&
         isSameTypes(pts1, pts2) &&
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
      case (TypeVar(_, constr1), _) =>
        if (constr1.inst != NoType) constr1.inst =:= tp2
        else constr1 instantiate (wildcardToTypeVarMap(tp2))
      case (_, TypeVar(_, constr2)) =>
        if (constr2.inst != NoType) tp1 =:= constr2.inst
        else constr2 instantiate (wildcardToTypeVarMap(tp1))
      case (AnnotatedType(_,atp), _) =>
        isSameType(atp, tp2)
      case (_, AnnotatedType(_,atp)) =>
        isSameType(tp1, atp)
      case _ =>
        if (tp1.isStable && tp2.isStable) {
          var origin1 = tp1
          while (origin1.singleDeref.isStable) origin1 = origin1.singleDeref
          var origin2 = tp2
          while (origin2.singleDeref.isStable) origin2 = origin2.singleDeref
          ((origin1 ne tp1) || (origin2 ne tp2)) && (origin1 =:= origin2)
        } else false
    }
  } || {
    val tp1n = tp1.normalize
    val tp2n = tp2.normalize
    ((tp1n ne tp1) || (tp2n ne tp2)) && isSameType(tp1n, tp2n)
  }

  /** Are `tps1' and `tps2' lists of pairwise equivalent
   *  types?
   */
  def isSameTypes(tps1: List[Type], tps2: List[Type]): Boolean =
    tps1.length == tps2.length &&
    List.forall2(tps1, tps2)((tp1, tp2) => tp1 =:= tp2)

  var stc: Int = 0
  private var pendingSubTypes = new collection.mutable.HashSet[SubTypePair]

  def isSubType(tp1: Type, tp2: Type): Boolean = try {
    stc = stc + 1
    if (stc >= LogPendingSubTypesThreshold) {
      val p = new SubTypePair(tp1, tp2)
      if (pendingSubTypes contains p)
        false
      else
        try {
          pendingSubTypes += p
          isSubType0(tp1, tp2)
        } finally {
          pendingSubTypes -= p
        }
    } else {
      isSubType0(tp1, tp2)
    }
  } finally {
    stc = stc - 1
  }

  /** hook for IDE */
  protected def trackTypeIDE(sym : Symbol) : Boolean = true;

  /** Does type `tp1' conform to `tp2'?
   *
   *  @param tp1 ...
   *  @param tp2 ...
   *  @return    ...
   */
  def isSubType0(tp1: Type, tp2: Type): Boolean = {
    (tp1, tp2) match {
      case (ErrorType, _)    => true
      case (WildcardType, _) => true
      case (_, ErrorType)    => true
      case (_, WildcardType) => true

      case (NoType, _)   => false
      case (NoPrefix, _) => tp2.symbol.isPackageClass
      case (_, NoType)   => false
      case (_, NoPrefix) => tp1.symbol.isPackageClass

      case (ThisType(_), ThisType(_))           => tp1 =:= tp2
      case (ThisType(_), SingleType(_, _))      => tp1 =:= tp2
      case (SingleType(_, _), ThisType(_))      => tp1 =:= tp2
      case (SingleType(_, _), SingleType(_, _)) => tp1 =:= tp2
      case (ConstantType(_), ConstantType(_))   => tp1 =:= tp2

      case (TypeRef(pre1, sym1, args1), TypeRef(pre2, sym2, args2))
      if !(tp1.isHigherKinded || tp2.isHigherKinded) =>
        //Console.println("isSubType " + tp1 + " " + tp2);//DEBUG
        if (inIDE) { trackTypeIDE(sym1); trackTypeIDE(sym2); }

        def isSubArgs(tps1: List[Type], tps2: List[Type],
                      tparams: List[Symbol]): Boolean = (
          tps1.isEmpty && tps2.isEmpty
          ||
          !tps1.isEmpty && !tps2.isEmpty &&
          (tparams.head.isCovariant || (tps2.head <:< tps1.head)) &&
          (tparams.head.isContravariant || (tps1.head <:< tps2.head)) &&
          isSubArgs(tps1.tail, tps2.tail, tparams.tail)
        );
        (sym1 == sym2 &&
          (phase.erasedTypes || pre1 <:< pre2) &&
          (sym2 == AnyClass || isSubArgs(args1, args2, sym1.typeParams)) //@M: Any is kind-polymorphic
         ||
          sym1.isAbstractType && !(tp1 =:= tp1.bounds.hi) && (tp1.bounds.hi <:< tp2)
         ||
          sym2.isAbstractType && !(tp2 =:= tp2.bounds.lo) && (tp1 <:< tp2.bounds.lo)
         ||
         sym2.isClass &&
         ({ val base = tp1 baseType sym2; !(base eq tp1) && (base <:< tp2) })
         ||
         sym1 == AllClass
         ||
         // Console.println("last chance " + sym1 + " " + sym2 + " " + sym2.isClass + " " (sym2 isSubClass ObjectClass))
         sym1 == AllRefClass &&
         sym2.isClass && (sym2 isNonBottomSubClass ObjectClass) && (!(tp2.normalize.symbol isNonBottomSubClass NotNullClass)))
      case (MethodType(pts1, res1), MethodType(pts2, res2)) =>
        (pts1.length == pts2.length &&
         matchingParams(pts1, pts2, tp2.isInstanceOf[JavaMethodType]) &&
         (res1 <:< res2) &&
         tp1.isInstanceOf[ImplicitMethodType] == tp2.isInstanceOf[ImplicitMethodType])
      case (PolyType(tparams1, res1), PolyType(tparams2, res2)) =>
        (tparams1.length == tparams2.length &&
         List.forall2(tparams1, tparams2)
           ((p1, p2) => p2.info.substSym(tparams2, tparams1) <:< p1.info) &&
         res1 <:< res2.substSym(tparams2, tparams1))
      case (TypeBounds(lo1, hi1), TypeBounds(lo2, hi2)) =>
        lo2 <:< lo1 && hi1 <:< hi2
      case (BoundedWildcardType(bounds), _) =>
        bounds.lo <:< tp2
      case (_, BoundedWildcardType(bounds)) =>
        tp1 <:< bounds.hi
      case (_, TypeVar(_, constr2)) =>
        if (constr2.inst != NoType) tp1 <:< constr2.inst
        else { constr2.lobounds = tp1 :: constr2.lobounds; true }
      case (TypeVar(_, constr1), _) =>
        if (constr1.inst != NoType) constr1.inst <:< tp2
        else { constr1.hibounds = tp2 :: constr1.hibounds; true }
      case (AnnotatedType(_,atp1), _) =>
        atp1 <:< tp2
      case (_, AnnotatedType(_,atp2)) =>
        tp1 <:< atp2
      case (_, _)  if (tp1.isHigherKinded || tp2.isHigherKinded) =>
        (tp1.symbol == AllClass
         ||
         tp2.symbol == AnyClass // @M Any and Nothing are super-type resp. subtype of every well-kinded type
         || // @M! normalize reduces higher-kinded case to PolyType's
         (tp1.isHigherKinded && tp2.isHigherKinded) && isSubType0(tp1.normalize, tp2.normalize))
      case (_, TypeRef(pre2, sym2, args2))
      if (sym2.isAbstractType && !(tp2 =:= tp2.bounds.lo) && (tp1 <:< tp2.bounds.lo) &&
            (if (!inIDE) true else trackTypeIDE(sym2)) ||
          sym2 == NotNullClass && tp1.isNotNull) =>
        true
      case (_, TypeRef(pre2, sym2, args2))
      if (sym2 == SingletonClass && tp1.isStable) =>
        true
      case (_, RefinedType(parents2, ref2)) =>
        (parents2 forall (tp2 => tp1 <:< tp2 || tp2.symbol == NotNullClass && tp1.isNotNull)) &&
        (ref2.toList forall tp1.specializes) &&
        (!parents2.exists(_.symbol.isAbstractType) || tp1.symbol != AllRefClass)
      case (_, ExistentialType(tparams2, res2)) =>
        val tvars = tparams2 map (tparam => new TypeVar(tparam.tpe, new TypeConstraint))
        val ires2 = res2.instantiateTypeParams(tparams2, tvars)
        (tp1 <:< ires2) && {
//          println("solve: "+tparams2)
          solve(tvars, tparams2, tparams2 map (x => 0), false)
//          println("check bounds: "+tparams2+" aginst "+(tvars map (_.constr.inst)))
          isWithinBounds(NoPrefix, NoSymbol, tparams2, tvars map (_.constr.inst))
        }
      case (RefinedType(parents1, ref1), _) =>
        parents1 exists (_ <:< tp2)
      case (ExistentialType(_, _), _) =>
        tp1.skolemizeExistential(NoSymbol, null) <:< tp2

      /* todo: replace following with
      case (ThisType(_), _)
         | {SingleType(_, _), _}
         | {ConstantType(_), _} =>
         once patern matching bug is fixed */
      case (_, NotNullType(ntp2)) =>
        tp1.isNotNull && tp1 <:< ntp2
      case (NotNullType(ntp1), _) =>
        ntp1 <:< tp2
      case (ThisType(_), _) => tp1.singleDeref <:< tp2
      case (SingleType(_, _), _) => tp1.singleDeref <:< tp2
      case (ConstantType(_), _) =>
        tp1.singleDeref <:< tp2

      case (TypeRef(pre1, sym1, args1), _) =>
        if (inIDE) trackTypeIDE(sym1)
        (sym1 == AllClass && tp2 <:< AnyClass.tpe
         ||
         sym1 == AllRefClass && tp2.isInstanceOf[SingletonType] && (tp1 <:< tp2.widen))
      case _ =>
        false
    }
  } || {
    val tp1n = tp1.normalize
    val tp2n = tp2.normalize
    ((tp1n ne tp1) || (tp2n ne tp2)) && isSubType0(tp1n, tp2n)
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
    tp.symbol == AllClass ||
    tp.symbol == AllRefClass && (sym.owner isSubClass ObjectClass) ||
    (tp.nonPrivateMember(sym.name).alternatives exists
      (alt => sym == alt || specializesSym(tp.narrow, alt, sym.owner.thisType, sym)))

  /** Does member `sym1' of `tp1' have a stronger type
   *  than member `sym2' of `tp2'?
   */
  private def specializesSym(tp1: Type, sym1: Symbol, tp2: Type, sym2: Symbol): Boolean = {
    val info1 = tp1.memberInfo(sym1)
    val info2 = tp2.memberInfo(sym2).substThis(tp2.symbol, tp1)
    //System.out.println("specializes "+tp1+"."+sym1+":"+info1+sym1.locationString+" AND "+tp2+"."+sym2+":"+info2)//DEBUG
    sym2.isTerm && (info1 <:< info2) ||
    sym2.isAbstractType && info2.bounds.containsType(tp1.memberType(sym1)) ||
    sym2.isAliasType && tp2.memberType(sym2).substThis(tp2.symbol, tp1) =:= tp1.memberType(sym1) //@MAT ok
  }

  /** A function implementing `tp1' matches `tp2' */
  private def matchesType(tp1: Type, tp2: Type, alwaysMatchSimple: Boolean): Boolean = (tp1, tp2) match {
    case (MethodType(pts1, res1), MethodType(pts2, res2)) =>
      matchingParams(pts1, pts2, tp2.isInstanceOf[JavaMethodType]) &&
      matchesType(res1, res2, alwaysMatchSimple) &&
      tp1.isInstanceOf[ImplicitMethodType] == tp2.isInstanceOf[ImplicitMethodType]
    case (PolyType(tparams1, res1), PolyType(tparams2, res2)) =>
      tparams1.length == tparams2.length &&
      matchesType(res1, res2.substSym(tparams2, tparams1), alwaysMatchSimple)
    case (PolyType(List(), rtp1), MethodType(List(), rtp2)) =>
      matchesType(rtp1, rtp2, alwaysMatchSimple)
    case (MethodType(List(), rtp1), PolyType(List(), rtp2)) =>
      matchesType(rtp1, rtp2, alwaysMatchSimple)
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

  /** Are `tps1' and `tps2' lists of pairwise equivalent types? */
  private def matchingParams(tps1: List[Type], tps2: List[Type], tps2isJava: Boolean): Boolean = (
    tps1.length == tps2.length &&
    List.forall2(tps1, tps2)((tp1, tp2) =>
      (tp1 =:= tp2) || tps2isJava & tp1.symbol == ObjectClass && tp2.symbol == AnyClass)
  );

  /** Prepend type `tp' to closure `cl'.
   *
   *  @param tp ...
   *  @param cl ...
   *  @return   ...
   */
  private def addClosure(tp: Type, cl: Array[Type]): Array[Type] = {
    val cl1 = new Array[Type](cl.length + 1)
    cl1(0) = tp
    Array.copy(cl, 0, cl1, 1, cl.length)
    cl1
  }

  private def modifyClosure(tp: Type)(other: Type): Array[Type] = {
    val cl = other.closure
    val cl1 = new Array[Type](cl.length)
    cl1(0) = tp
    Array.copy(cl, 1, cl1, 1, cl.length-1)
    cl1
  }

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
   *  @throws NoInstance
   */
  def solve(tvars: List[TypeVar], tparams: List[Symbol],
            variances: List[Int], upper: Boolean) {
    val config = tvars zip (tparams zip variances)

    def solveOne(tvar: TypeVar, tparam: Symbol, variance: Int) {
      if (tvar.constr.inst == NoType) {
        val up = if (variance != CONTRAVARIANT) upper else !upper
        tvar.constr.inst = null
        val bound: Type = if (up) tparam.info.bounds.hi else tparam.info.bounds.lo
        // Console.println("solveOne0 "+tvar+" "+config+" "+bound);//DEBUG
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
            if (bound.symbol != AnyClass) {
              tvar.constr.hibounds =
                bound.instantiateTypeParams(tparams, tvars) :: tvar.constr.hibounds
            }
            for (tparam2 <- tparams)
              if (tparam2.info.bounds.lo =:= tparam.tpe)  //@M TODO: might be affected by change to tpe in Symbol
                tvar.constr.hibounds =
                  tparam2.tpe.instantiateTypeParams(tparams, tvars) :: tvar.constr.hibounds
          } else {
            if (bound.symbol != AllClass && bound.symbol != tparam) {
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
        //Console.println("solving "+tvar+" "+up+" "+(if (up) (tvar.constr.hibounds) else tvar.constr.lobounds))//DEBUG
        tvar.constr.inst = if (up) glb(tvar.constr.hibounds) else lub(tvar.constr.lobounds)
      }
    }
    for ((tvar, (tparam, variance)) <- config)
      solveOne(tvar, tparam, variance)
  }

  /** Do type arguments `targs' conform to formal parameters
   *  `tparams'?
   *
   *  @param tparams ...
   *  @param targs   ...
   *  @return        ...
   */
  def isWithinBounds(pre: Type, owner: Symbol, tparams: List[Symbol], targs: List[Type]): Boolean = {
    val bounds = tparams map { tparam =>
      tparam.info.asSeenFrom(pre, owner).instantiateTypeParams(tparams, targs).bounds
    }
    !(List.map2(bounds, targs)((bound, targ) => bound containsType targ) contains false)
  }

// Lubs and Glbs ---------------------------------------------------------

  /** The greatest sorted upwards closed lower bound of a list of lists of
   *  types relative to the following ordering &lt;= between lists of types:
   *
   *    xs &lt;= ys   iff   forall y in ys exists x in xs such that x &lt;: y
   *
   *  @See closure  for a definition of sorted and upwards closed.
   */
  private def glbList(tss: List[List[Type]], depth: Int): List[Type] = {
    val tss1 = tss filter (ts => !ts.isEmpty)
    if (tss1.isEmpty) List()
    else if (tss1.tail.isEmpty) tss.head
    else {
      val ts0 = tss1 map (_.head)
      val sym = minSym(ts0)
      val ts1 = elimSuper(ts0 filter (_.symbol == sym))
      mergePrefixAndArgs(ts1, -1, depth) match {
        case Some(tp0) =>
          tp0 :: glbList(tss1 map (ts => if (ts.head.symbol == sym) ts.tail else ts), depth)
        case None =>
          throw new MalformedClosure(ts1)
      }
    }
  }

  /** The greatest sorted upwards closed lower bound of a list of closures.
   *
   *  @See glbList for more explanations.
   */
  private def glbArray(tss: List[Array[Type]], depth: Int): Array[Type] = {
    val tss1 = tss map { ts: Array[Type] => List.fromArray(ts) }
    val glbs = glbList(tss1, depth)
    val result = new Array[Type](glbs.length)
    var i = 0
    for (x <- glbs.elements) { result(i) = x; i += 1 }
    result
    // Array(glbs: _*);
  }

  /** The least sorted upwards closed upper bound of a non-empty list
   *  of lists of types.
   *
   *  @See glbList for more explanations.
   */
  private def lubList(tss: List[List[Type]], depth: Int): List[Type] =
    if (tss.tail.isEmpty) tss.head
    else if (tss exists (_.isEmpty)) List()
    else {
      val ts0 = tss map (_.head)
      val sym = minSym(ts0)
      if (ts0 forall (t => t.symbol == sym))
        mergePrefixAndArgs(elimSub(ts0), 1, depth).toList ::: lubList(tss map (_.tail), depth)
      else
        lubList(tss map (ts => if (ts.head.symbol == sym) ts.tail else ts), depth)
    }

  /** The least sorted upwards closed upper bound of a non-empty list
   *  of closures.
   *
   *  @See lubList for more explanations.
   */
  private def lubArray(tss: List[Array[Type]], depth: Int): Array[Type] = {
    var lubs = lubList(tss map { ts: Array[Type] => List.fromArray(ts) }, depth)
    var arr = new Array[Type](lubs.length)
    var i = 0
    while (i < arr.length) {
      arr(i) = lubs.head
      i = i + 1
      lubs = lubs.tail
    }
    arr
    // todo: replace by Array(lubs: _* )
  }

  /** The minimal symbol (wrt Symbol.isLess) of a list of types */
  private def minSym(tps: List[Type]): Symbol =
    (tps.head.symbol /: tps.tail) {
      (sym1, tp2) => if (tp2.symbol isLess sym1) tp2.symbol else sym1
    }

  /** A minimal type list which has a given array of types as its closure */
  def spanningTypes(ts: List[Type]): List[Type] = ts match {
    case List() => List()
    case first :: rest =>
      first :: spanningTypes(
        rest filter (t => !first.symbol.isSubClass(t.symbol)))
  }

  /** Eliminate from list of types all elements which are a supertype
   *  of some other element of the list. */
  private def elimSuper(ts: List[Type]): List[Type] = ts match {
    case List() => List()
    case t :: ts1 =>
      val rest = ts1 filter (t1 => !(t <:< t1));
      if (rest exists (t1 => t1 <:< t)) rest else t :: rest
  }

  /** Eliminate from list of types all elements which are a subtype
   *  of some other element of the list. */
  private def elimSub(ts: List[Type]): List[Type] = {
    def elimSub0(ts: List[Type]): List[Type] = ts match {
      case List() => List()
      case t :: ts1 =>
        val rest = elimSub0(ts1 filter (t1 => !(t1 <:< t)))
        if (rest exists (t1 => t <:< t1)) rest else t :: rest
    }
    val ts0 = elimSub0(ts)
    if (ts0.length <= 1) ts0
    else {
      val ts1 = List.mapConserve(ts0)(_.singleDeref)
      if (ts1 eq ts0) ts0
      else elimSub(ts1)
    }
  }

  private def stripExistentials(ts: List[Type]): (List[Type], List[Symbol]) = {
    val typeParams = ts flatMap {
      case ExistentialType(tparams, res) => tparams
      case t => List()
    }
    val strippedTypes = List.mapConserve(ts) {
      case ExistentialType(tparams, res) => res
      case t => t
    }
    (strippedTypes, typeParams)
  }

  def lub(ts: List[Type]): Type = lub(ts, maxClosureDepth(ts) + LubGlbMargin)

  /** The least upper bound wrt &lt;:&lt; of a list of types */
  def lub(ts: List[Type], depth: Int): Type = {
    def lub0(ts0: List[Type]): Type = {
      if (elimSub(ts0 map (_.deconst)) != elimSub(ts0))
        println("DIFF for lub of "+ts+", with deconst = "+elimSub(ts0 map (_.deconst))+", without = "+elimSub(ts0))

      elimSub(ts0/* map (_.deconst) */) match {
      case List() => AllClass.tpe
      case List(t) => t
      case ts @ PolyType(tparams, _) :: _ =>
        PolyType(
          List.map2(tparams, List.transpose(matchingBounds(ts, tparams)))
            ((tparam, bounds) => tparam.cloneSymbol.setInfo(glb(bounds, depth))),
          lub0(matchingInstTypes(ts, tparams)))
      case ts @ MethodType(pts, _) :: rest =>
        MethodType(pts, lub0(matchingRestypes(ts, pts)))
      case ts @ TypeBounds(_, _) :: rest =>
        assert(false)
        mkTypeBounds(glb(ts map (_.bounds.lo), depth), lub(ts map (_.bounds.hi), depth))
      case ts0 =>
        val (ts, tparams) = stripExistentials(ts0)
        val closures: List[Array[Type]] = ts map (_.closure)
        val lubBaseTypes: Array[Type] = lubArray(closures, depth)
        val lubParents = spanningTypes(List.fromArray(lubBaseTypes))
        val lubOwner = commonOwner(ts)
        val lubBase = intersectionType(lubParents, lubOwner)
        val lubType =
          if (phase.erasedTypes || depth == 0) lubBase
          else {
            val lubRefined = refinedType(lubParents, lubOwner)
            val lubThisType = lubRefined.symbol.thisType
            val narrowts = ts map (_.narrow)
            def lubsym(proto: Symbol): Symbol = {
              val prototp = lubThisType.memberInfo(proto)
              val syms = narrowts map (t =>
                t.nonPrivateMember(proto.name).suchThat(sym =>
                  sym.tpe matches prototp.substThis(lubThisType.symbol, t)))
              if (syms contains NoSymbol) NoSymbol
              else {
                val symtypes =
                  (List.map2(narrowts, syms)
                     ((t, sym) => t.memberInfo(sym).substThis(t.symbol, lubThisType)));
                if (proto.isTerm)
                  proto.cloneSymbol(lubRefined.symbol).setInfo(lub(symtypes, depth-1))
                else if (symtypes.tail forall (symtypes.head =:=))
                  proto.cloneSymbol(lubRefined.symbol).setInfo(symtypes.head)
                else {
                  def lubBounds(bnds: List[TypeBounds]): TypeBounds =
                    mkTypeBounds(glb(bnds map (_.lo), depth-1), lub(bnds map (_.hi), depth-1))
                  proto.owner.newAbstractType(proto.pos, proto.name)
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
            if (lubRefined.decls.isEmpty) lubBase else lubRefined
          }
        existentialAbstraction(tparams, lubType)
    }}
//    if (settings.debug.value) {
//      log(indent + "lub of " + ts + " at depth "+depth)//debug
//      indent = indent + "  "
//    }
    val res = lub0(ts)
//    if (settings.debug.value) {
//      indent = indent.substring(0, indent.length() - 2)
//      log(indent + "lub of " + ts + " is " + res)//debug
//    }
    if (ts forall (_.isNotNull)) res.notNull else res
  }

  def glb(ts: List[Type]): Type = glb(ts, maxClosureDepth(ts) + LubGlbMargin)

  /** The greatest lower bound wrt &lt;:&lt; of a list of types */
  private def glb(ts: List[Type], depth: Int): Type = {
    def glb0(ts0: List[Type]): Type = elimSuper(ts0 map (_.deconst)) match {
      case List() => AnyClass.tpe
      case List(t) => t
      case ts @ PolyType(tparams, _) :: _ =>
        PolyType(
          List.map2(tparams, List.transpose(matchingBounds(ts, tparams)))
          ((tparam, bounds) => tparam.cloneSymbol.setInfo(lub(bounds, depth))),
          glb0(matchingInstTypes(ts, tparams)))
      case ts @ MethodType(pts, _) :: rest =>
        MethodType(pts, glb0(matchingRestypes(ts, pts)))
      case ts @ TypeBounds(_, _) :: rest =>
        mkTypeBounds(lub(ts map (_.bounds.lo), depth), glb(ts map (_.bounds.hi), depth))
      case ts0 =>
        try {
          val (ts, tparams) = stripExistentials(ts0)
          val glbOwner = commonOwner(ts)
          def refinedToParents(t: Type): List[Type] = t match {
            case RefinedType(ps, _) => ps flatMap refinedToParents
            case _ => List(t)
          }
          val ts1 = ts flatMap refinedToParents
          val glbBase = intersectionType(ts1, glbOwner)
          val glbType =
            if (phase.erasedTypes || depth == 0) glbBase
            else {
              val glbRefined = refinedType(ts1, glbOwner)
              val glbThisType = glbRefined.symbol.thisType
              def glbsym(proto: Symbol): Symbol = {
                val prototp = glbThisType.memberInfo(proto)
                val syms = for {
                  val t <- ts
                  val alt <- t.nonPrivateMember(proto.name).alternatives
                  glbThisType.memberInfo(alt) matches prototp
                } yield alt
                val symtypes = syms map glbThisType.memberInfo
                assert(!symtypes.isEmpty)
                proto.cloneSymbol(glbRefined.symbol).setInfo(
                  if (proto.isTerm) glb(symtypes, depth-1)
                  else {
                    def isTypeBound(tp: Type) = tp match {
                      case TypeBounds(_, _) => true
                      case _ => false
                    }
                    def glbBounds(bnds: List[Type]): TypeBounds = {
                      val lo = lub(bnds map (_.bounds.lo), depth-1)
                      val hi = glb(bnds map (_.bounds.hi), depth-1)
                      if (lo <:< hi) mkTypeBounds(lo, hi)
                      else throw new MalformedClosure(bnds)
                    }
                    val symbounds = symtypes filter isTypeBound
                    var result: Type =
                      if (symbounds.isEmpty)
                        mkTypeBounds(AllClass.tpe, AnyClass.tpe)
                      else glbBounds(symbounds)
                    for (t <- symtypes if !isTypeBound(t))
                      if (result.bounds containsType t) result = t
                      else throw new MalformedClosure(symtypes);
                    result
                  })
              }
              for (t <- ts; val sym <- t.nonPrivateMembers)
                if (!sym.isClass && !sym.isConstructor && !(glbThisType specializes sym))
                  try {
                    addMember(glbThisType, glbRefined, glbsym(sym))
                  } catch {
                    case ex: NoCommonType =>
                  }
              if (glbRefined.decls.isEmpty) glbBase else glbRefined
            }
          existentialAbstraction(tparams, glbType)
        } catch {
          case _: MalformedClosure =>
            if (ts forall (t => AllRefClass.tpe <:< t)) AllRefClass.tpe
            else AllClass.tpe
        }
    }
    if (settings.debug.value) {
      log(indent + "glb of " + ts + " at depth "+depth)//debug
      indent = indent + "  "
    }
    val res = glb0(ts)
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
  private def mergePrefixAndArgs(tps: List[Type], variance: Int, depth: Int): Option[Type] = tps match {
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
            else if (tparam.variance == -variance) AllClass.tpe
            else NoType
          else
            if (tparam.variance == variance) lub(as, depth-1)
            else if (tparam.variance == -variance) glb(as, depth-1)
            else {
              val l = lub(as, depth-1)
              val g = glb(as, depth-1)
              if (l <:< g) l
              else {
                val qvar =
                  commonOwner(as).newAbstractType(NoPosition, freshTypeName())
                  .setInfo(TypeBounds(g, l))
                  .setFlag(EXISTENTIAL)
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
  }

  /** Make symbol `sym' a member of scope `tp.decls'
   *  where `thistp' is the narrowed owner type of the scope.
   */
  def addMember(thistp: Type, tp: Type, sym: Symbol) {
    assert(sym != NoSymbol)
    if (settings.debug.value) log("add member " + sym)//debug
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
      case MethodType(pts1, res) if (isSameTypes(pts1, pts)) =>
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

  /** An exception signalling a malformed closure */
  class MalformedClosure(ts: List[Type])
       extends TypeError("no common type instance of base types " +
                         ts.mkString("", " and ", "") + " exists")

  /** An exception signalling a variance annotation/usage conflict */
  class VarianceError(msg: String) extends TypeError(msg)

  /** The current indentation string for traces */
  private var indent: String = ""

  /** Perform operation `p' on arguments `tp1',
   *  `arg2' and print trace of computation.
   */
  private def explain[T](op: String, p: (Type, T) => Boolean, tp1: Type, arg2: T): Boolean = {
    Console.println(indent + tp1 + " " + op + " " + arg2 + "?")
    indent = indent + "  "
    val result = p(tp1, arg2)
    indent = indent.substring(0, indent.length() - 2)
    Console.println(indent + result)
    result
  }

  /** If option `explaintypes' is set, print a subtype trace for
   *  `found <:< required'.
   *
   *  @param found    ...
   *  @param required ...
   */
  def explainTypes(found: Type, required: Type) {
    if (settings.explaintypes.value) withTypesExplained(found <:< required)
  }

  def withTypesExplained[A](op: => A): A = {
    val s = explainSwitch
    try { explainSwitch = true; op } finally { explainSwitch = s }
  }

  def withoutMalformedChecks[T](op: => T): T = {
    val s = checkMalformedSwitch
    try { checkMalformedSwitch = false; op } finally { checkMalformedSwitch = s }
  }
}

/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.symtab;

import scala.tools.util.Position;
import Flags._;

/* A standard type pattern match:
  case ErrorType =>
  case WildcardType =>
  case NoType =>
  case NoPrefix =>
  case ThisType(_) =>
  case SingleType(pre, sym) =>
  case ConstantType(base, value) =>
  case TypeRef(pre, sym, args) =>
  case TypeBounds(lo, hi) =>
  case RefinedType(parents, defs) =>
  case ClassInfoType(parents, defs, clazz) =>
  case MethodType(paramtypes, result) =>
  case PolyType(tparams, result) =>
  case OverloadedType(pre, tparams, alts) =>
  case TypeVar(_, _) =>
*/

abstract class Types: SymbolTable {
  import definitions._;

  private var explainSwitch = false;

  val emptyTypeArray = new Array[Type](0);

  /** The base class for all types */
  trait Type {

    /** The symbol associated with the type */
    def symbol: Symbol = NoSymbol;

    /** The base type underlying a singleton type,
     *  identity on all other types */
    def singleDeref: Type = this;

    /** Widen from singleton type to its underlying non-singleton base type
     *  by applying one or more singleDeref steps,
     *  identity for all other types */
    def widen: Type = this;

    /** The type of `this' of a class type or reference type
     */
    def typeOfThis = symbol.typeOfThis;

    /** Map to a this type which is a subtype of this type.
     */
    def narrow: Type =
      refinedType(List(this), commonOwner(this), EmptyScope).narrow;

    /** Map a constant type to its underlying base type,
     *  identity for all other types */
    def deconst: Type = this;

    /** Map a parameterless method type to its result type
     *  identity for all other types */
    def derefDef: Type = match {
      case PolyType(List(), restp) => restp
      case _ => this
    }

    /** For a TypeBounds type, itself;
     *  for a reference denoting an abstract type, its bounds,
     *  for all other types, a TypeBounds type all of whose bounds are this type.
     *  error for all other types */
    def bounds: TypeBounds = TypeBounds(this, this);

    /** For a class or intersection type, its parents.
     *  For a TypeBounds type, the parents of its hi bound.
     *  inherited by typerefs, singleton types, and refinement types,
     *  The empty list for all other types */
    def parents: List[Type] = List();

    /** For a typeref or single-type, its prefix. NoType for all other types. */
    def prefix: Type = NoType;

    /** For a typeref, its arguments. The empty list for all other types */
    def typeArgs: List[Type] = List();

    /** For a method or poly type, its result type,
     *  the type itself for all other types */
    def resultType: Type = this;

    /** For a method or poly type, the number of its value parameter sections,
     *  0 for all other types */
    def paramSectionCount: int = 0;

    /** For a method or poly type, the types of its first value parameter section,
     *  the empty list for all other types */
    def paramTypes: List[Type] = List();

    /** For a poly type, its type parameters,
     *  the empty list for all other types */
    def typeParams: List[Symbol] = List();

    /** Is this type produced as a repair for an error? */
    def isError: boolean = symbol.isError;

    /** Does this type denote a stable reference (i.e. singleton type)? */
    def isStable: boolean = false;

    /** For a classtype or refined type, its defined or declared members;
     *  inherited by subtypes and typerefs.
     *  The empty scope for all other types */
    def decls: Scope = EmptyScope;

    /** The defined or declared members with name `name' in this type;
     *  an OverloadedSymbol if several exist, NoSymbol if none exist.
     *  Alternatives of overloaded symbol appear in the order they are declared.
     */
    def decl(name: Name): Symbol = findDecl(name, 0);

    /** The non-orivate defined or declared members with name `name' in this type;
     *  an OverloadedSymbol if several exist, NoSymbol if none exist.
     *  Alternatives of overloaded symbol appear in the order they are declared.
     */
    def nonPrivateDecl(name: Name): Symbol = findDecl(name, PRIVATE);

    /** A list of all members of this type (defined or inherited)
     *  Members appear in linearization order of their owners.
     *  Members with the same owner appear in reverse order of their declarations.
     */
    def members: List[Symbol] = findMember(nme.ANYNAME, 0).alternatives;

    /** A list of all non-private members of this type (defined or inherited) */
    def nonPrivateMembers: List[Symbol] = findMember(nme.ANYNAME, PRIVATE).alternatives;

    /** The member with given name,
     *  an OverloadedSymbol if several exist, NoSymbol if none exist */
    def member(name: Name): Symbol = findMember(name, 0);

    /** The non-private member with given name,
     *  an OverloadedSymbol if several exist, NoSymbol if none exist */
    def nonPrivateMember(name: Name): Symbol = findMember(name, PRIVATE);

    /** The least type instance of given class which is a supertype
     *  of this type */
    def baseType(clazz: Symbol): Type = NoType;

    /** This type as seen from prefix `
    pre' and class `clazz'. This means:
     *  Replace all thistypes of `clazz' or one of its subclasses by `pre'
     *  and instantiate all parameters by arguments of `pre'.
     *  Proceed analogously for thistypes referring to outer classes. */
    def asSeenFrom(pre: Type, clazz: Symbol): Type =
      new AsSeenFromMap(pre, clazz) apply this;

    /** The info of `sym', seen as a member of this type. */
    def memberInfo(sym: Symbol): Type =
      sym.info.asSeenFrom(this, sym.owner);

    /** The type of `sym', seen as a member of this type. */
    def memberType(sym: Symbol): Type =
      sym.tpe.asSeenFrom(this, sym.owner);

    /** Substitute types `to' for occurrences of references to symbols `from'
     *  in this type. */
    def subst(from: List[Symbol], to: List[Type]): Type =
      new SubstTypeMap(from, to) apply this;

    /** Substitute symbols `to' for occurrences of symbols `from' in this type. */
    def substSym(from: List[Symbol], to: List[Symbol]): Type =
      new SubstSymMap(from, to) apply this;

    /** Substitute all occurrences of ThisType(from) in this type by `to' */
    def substThis(from: Symbol, to: Type): Type =
      new SubstThisMap(from, to) apply this;

    /** Does this type contain a reference to this symbol? */
    def contains(sym: Symbol): boolean = {
      val m = new ContainsMap(sym);
      m(this);
      m.result
    }

    /** Is this type a subtype of that type? */
    def <:<(that: Type): boolean =
      if (explainSwitch) explain("<", isSubType, this, that)
      else this == that || isSubType(this, that);

    /** Is this type equivalent to that type? */
    def =:=(that: Type): boolean =
      if (explainSwitch) explain("<", isSameType, this, that)
      else this == that || isSameType(this, that);

    /** Does this type implement symbol `sym' with same or stronger type? */
    def specializes(sym: Symbol): boolean =
      if (explainSwitch) explain("specializes", specializesSym, this, sym)
      else specializesSym(this, sym);

    /** Is this type close enough to that type so that
     *  members with the two type would override each other?
     *  This means:
     *    - Either both types are polytypes with the same number of
     *      type parameters and their result types match after renaming
     *      corresponding type parameters
     *    - Or both types are method types with equivalent type parameter types
     *      and matching result types
     *    - Or both types are equivalent
     *    - Or phase.exactMatch is false and both types are neither method nor
     *      poly types.
     */
    def matches(that: Type): boolean =
      matchesType(this, that);

    /** The shortest sorted upwards closed array of types that contains
     *  this type as first element.
     *
     *  A list or array of types ts is upwards closed if
     *
     *    for all t in ts:
     *      for all typerefs p.s[args] such that t <: p.s[args]
     *      there exists a typeref p'.s[args'] in ts such that
     *      t <: p'.s['args] <: p.s[args],
     *      and
     *      for all singleton types p.s such that t <: p.s
     *      there exists a singleton type p'.s in ts such that
     *      t <: p'.s <: p.s
     *
     *  Sorting is with respect to Symbol.isLess() on type symbols.
     */
    def closure: Array[Type] = Predef.Array(this);

    def baseClasses: List[Symbol] = List();

    /** The index of given class symbol in the closure of this type,
     *  or -1 if no base type with given class symbol exists */
    def closurePos(sym: Symbol): int = {
      val cl = closure;
      var lo = 0;
      var hi = cl.length - 1;
      while (lo <= hi) {
	val mid = (lo + hi) / 2;
	val clsym = cl(mid).symbol;
	if (sym == clsym) return mid
	else if (sym isLess clsym) hi = mid - 1
	else if (clsym isLess sym) lo = mid + 1
	else throw new Error()
      }
      -1
    }

    /** The erasure of this type. This is:
     *   - For a singleton or constant type,
     *     the erasure of its underlying base type
     *   - For a non-empty type intersection,
     *     the erasure of its first parent
     *   - For a polymorphic type, the erasure of its result type
     *   - for types Object, All, Any: AnyRef
     *   - for an abstract type or TypeBounds type,
     *     the erasure of its upper bound
     *   - for all other typerefs, the type without prefix or arguments
     *   - for a method type, the method type consistsing of
     *     erased parameter and result types.
     *   - for all other types, the type itself.
     */
    def erasure: Type = this;

    /** If this is a polytype, a copy with cloned type parameters owned
     *  by `owner'. Identity for all other types. */
    def cloneInfo(owner: Symbol) = this;

    /** The string representation of this type used as a prefix */
    def prefixString = toString() + "#";

    /** The string representation of this type, with singletypes explained */
    def toLongString = {
      val str = toString();
      if (str.endsWith(".type")) str + " (with underlying type " + widen + ")";
      else str
    }

    /** Is this type completed (i.e. not a lazy type)?
     */
    def isComplete: boolean = true;

    /** If this is a lazy type, assign a new type to `sym'. */
    def complete(sym: Symbol): unit = {}

    private def findDecl(name: Name, excludedFlags: int): Symbol = {
      var alts: List[Symbol] = List();
      var sym: Symbol = NoSymbol;
      var e: ScopeEntry = decls.lookupEntry(name);
      while (e != null) {
	if ((e.sym.rawflags & excludedFlags) == 0) {
	  if (sym == NoSymbol) sym = e.sym
	  else {
	    if (alts.isEmpty) alts = List(sym);
	    alts = e.sym :: alts
	  }
	}
	e = decls.lookupNextEntry(e)
      }
      if (alts.isEmpty) sym
      else baseClasses.head.newOverloaded(this, alts)
    }

    protected def findMember(name: Name, excludedFlags: int): Symbol = {
      //System.out.println("find member " + name.decode + " in " + this.baseClasses);//DEBUG
      var members: Scope = null;
      var member: Symbol = NoSymbol;
      var excluded = excludedFlags | DEFERRED;
      var continue = true;
      while (continue) {
	continue = false;
	var bcs = baseClasses;
	while (!bcs.isEmpty) {
	  val decls = bcs.head.info.decls;
	  bcs = bcs.tail;
	  var entry = if (name == nme.ANYNAME) decls.elems else decls lookupEntry name;
	  while (entry != null) {
	    val sym = entry.sym;
	    val excl = sym.rawflags & excluded;
	    if (excl == 0) {
	      if (name.isTypeName)
		return sym
	      else if (member == NoSymbol)
		member = sym
	      else if (members == null &&
		       !(member.name == sym.name &&
			 (memberType(member) matches memberType(sym))))
		members = new Scope(List(member, sym))
	      else {
		var prevEntry = members lookupEntry sym.name;
		while (prevEntry != null &&
		       !(memberType(prevEntry.sym) matches memberType(sym)))
		  prevEntry = members lookupNextEntry prevEntry;
		if (prevEntry == null)
		  members enter sym
	      }
	    } else if (excl == DEFERRED) {
	      continue = true;
	    }
	    entry = if (name == nme.ANYNAME) entry.next else decls lookupNextEntry entry
	  } // while (entry != null)
	  excluded = excluded | PRIVATE
	} // while (!bcs.isEmpty)
	excluded = excludedFlags
      } // while (continue)
      if (members == null) member
      else baseClasses.head.newOverloaded(this, members.toList)
    }
  }

// Subclasses ------------------------------------------------------------

  /** A base class for types that defer some operations
   *  to their immediate supertype
   */
  abstract trait SubType extends Type {
    protected def supertype: Type;
    override def parents: List[Type] = supertype.parents;
    override def decls: Scope = supertype.decls;
    override def baseType(clazz: Symbol): Type = supertype.baseType(clazz);
    override def closure: Array[Type] = supertype.closure;
    override def baseClasses: List[Symbol] = supertype.baseClasses;
    override def erasure: Type = supertype.erasure;
  }

  /** A base class for types that represent a single value
   *  (single-types and this-types)
   */
  abstract trait SingletonType extends SubType {
    override def singleDeref: Type;
    protected def supertype: Type = singleDeref;
    override def isStable: boolean = true;
    override def widen: Type = singleDeref.widen;
    override def closure: Array[Type] = addClosure(this, supertype.closure);
    override def toString(): String = prefixString + "type";
  }

  /** An object representing an erroneous type */
  case object ErrorType extends Type {
    // todo see whether we can do without
    override def isError: boolean = true;
    override def decls: Scope = new ErrorScope(NoSymbol);
    override def findMember(name: Name, excludedFlags: int): Symbol = decls lookup name;
    override def baseType(clazz: Symbol): Type = this;
    override def toString(): String = "<error>";
    override def narrow: Type = this;
  }

  /** An object representing an unknown type */
  case object WildcardType extends Type {
    override def toString(): String = "?"
  }

  /** An object representing a non-existing type */
  case object NoType extends Type {
    override def toString(): String = "<notype>"
  }

  /** An object representing a non-existing prefix */
  case object NoPrefix extends Type {
    override def isStable: boolean = true;
    override def prefixString = "";
  }

  /** A class for this-types of the form <sym>.this.type
   */
  case class ThisType(sym: Symbol) extends SingletonType {
    override def symbol = sym;
    override def singleDeref: Type = sym.typeOfThis;
    override def prefixString =
      if (settings.debug.value) sym.nameString + ".this.";
      else if (sym.isRoot) ""
      else if (sym.isAnonymousClass || sym.isRefinementClass) "this."
      else if (sym.isPackageClass) sym.fullNameString('.') + "."
      else sym.nameString + ".this.";
    override def narrow: Type = this;
  }

  /** A class for singleton types of the form <prefix>.<sym.name>.type.
   *  Cannot be created directly; one should always use
   *  `singleType' for creation.
   */
  abstract case class SingleType(pre: Type, sym: Symbol) extends SingletonType {
    private var singleDerefCache: Type = _;
    private var valid: Phase = null;
    override def singleDeref: Type = {
      if (valid != phase) {
        valid = phase;
        singleDerefCache = pre.memberType(sym).resultType;
      }
      singleDerefCache
    }
    override def symbol = sym;
    override def prefix: Type = pre;
    override def prefixString: String = pre.prefixString + sym.nameString + ".";
  }

  /** A class for the bounds of abstract types and type parameters
   */
  case class TypeBounds(lo: Type, hi: Type) extends SubType {
    protected def supertype: Type = hi;
    override def bounds: TypeBounds = this;
    def containsType(that: Type) = lo <:< that && that <:< hi;
  }

  /** A common base class for intersection types and class types
   */
  abstract class CompoundType extends Type {
    override val parents: List[Type];
    override val decls: Scope;

    private var closureCache: Array[Type] = _;
    private var baseClassCache: List[Symbol] = _;
    private var validClosure: Phase = null;
    private var validBaseClasses: Phase = null;

    override def closure: Array[Type] = {
      def computeClosure: Array[Type] =
	try {
          addClosure(symbol.tpe, glbArray(parents map (.closure)));
	} catch {
          case ex: MalformedClosure =>
            throw new MalformedType(
	      "the type intersection " + this + " is malformed" +
              "\n --- because ---\n" + ex.getMessage())
	}
      if (validClosure != phase) {
        validClosure = phase;
        closureCache = null;
        closureCache = computeClosure;
	System.out.println("closure(" + symbol + ") = " + List.fromArray(closureCache));//debug
      }
      if (closureCache == null)
        throw new TypeError("illegal cyclic reference involving " + symbol);
      closureCache;
    }

    override def baseClasses: List[Symbol] = {
      def computeBaseClasses: List[Symbol] =
	if (parents.isEmpty) List(symbol)
	else {
	  var bcs: List[Symbol] = parents.head.baseClasses;
	  val mixins = parents.tail;
	  def isNew(limit: List[Type])(clazz: Symbol): boolean = {
	    var ms = mixins;
	    while (!(ms eq limit) && ms.head.closurePos(clazz) < 0) ms = ms.tail;
	    ms eq limit
	  }
	  var ms = mixins;
	  while (!ms.isEmpty) {
	    bcs = ms.head.baseClasses.filter(isNew(ms)) ::: bcs;
	    ms = ms.tail
	  }
	  symbol :: bcs
	}
      if (validBaseClasses != phase) {
	validBaseClasses = phase;
	baseClassCache = null;
	baseClassCache = computeBaseClasses;
      }
      if (baseClassCache == null)
        throw new TypeError("illegal cyclic reference involving " + symbol);
      baseClassCache
    }

    override def baseType(sym: Symbol): Type = {
      val index = closurePos(sym);
      if (index >= 0) closure(index) else NoType;
    }

    override def narrow: Type = symbol.thisType;

    override def erasure: Type =
      if (parents.isEmpty) this else parents.head.erasure;

    override def toString(): String =
      parents.mkString("", " with ", "") + decls.toString()
  }

  /** A class representing intersection types with refinements of the form
   *    <parents_0> with ... with <parents_n> { decls }
   *  Cannot be created directly;
   *  one should always use `refinedType' for creation.
   */
  abstract case class RefinedType(override val parents: List[Type],
				  override val decls: Scope) extends CompoundType;

  /** A class representing a class info
   */
  case class ClassInfoType(override val parents: List[Type],
			   override val decls: Scope,
			   override val symbol: Symbol) extends CompoundType;

  class PackageClassInfoType(decls: Scope, clazz: Symbol) extends ClassInfoType(List(), decls, clazz);

  /** A class representing a constant type */
  case class ConstantType(base: Type, value: Any) extends SingletonType {
    override def symbol: Symbol = base.symbol;
    override def singleDeref: Type = base;
    override def deconst: Type = base;
    override def toString(): String = base.toString() + "(" + value + ")";
  }

  class ExtTypeRef(pre: Type, sym: Symbol, args: List[Type]) extends TypeRef(pre, sym, args);

  /** A class for named types of the form <prefix>.<sym.name>[args]
   *  Cannot be created directly; one should always use `typeRef' for creation.
   */
  case class TypeRef(pre: Type, sym: Symbol, args: List[Type]) extends Type {
    assert(!sym.isAbstractType || pre.isStable || pre.isError);

    def transform(tp: Type): Type =
      tp.asSeenFrom(pre, sym.owner).subst(sym.typeParams, args);

    def transform(cl: Array[Type]): Array[Type] = {
      val cl1 = new Array[Type](cl.length);
      var i = 0;
      while (i < cl.length) { cl1(i) = transform(cl(i)); i = i + 1 }
      cl1
    }

    override def symbol = sym;

    override def bounds: TypeBounds =
      if (sym.isAbstractType) transform(sym.info.bounds).asInstanceOf[TypeBounds]
      else super.bounds;

    override def parents: List[Type] = sym.info.parents map transform;

    override def typeOfThis = transform(sym.typeOfThis);

    override def prefix: Type = pre;

    override def typeArgs: List[Type] = args;

    override def typeParams: List[Symbol] =
      if (args.isEmpty) symbol.typeParams else List();

    override def decls: Scope = sym.info.decls;

    override def baseType(clazz: Symbol): Type =
      if (sym == clazz) this
      else if (sym.isClass) transform(sym.info.baseType(clazz))
      else pre.memberInfo(sym).baseType(clazz);

    override def closure: Array[Type] =
      if (sym.isAbstractType) addClosure(this, transform(bounds.hi).closure)
      else transform(sym.info.closure);

    override def baseClasses: List[Symbol] = sym.info.baseClasses;

    override def erasure: Type =
      if (sym.isAbstractType || sym.isAliasType) sym.info.erasure
      else if (sym == ObjectClass ||
	       sym == AllClass ||
	       sym == AllRefClass ||
	       sym == AnyRefClass) AnyClass.tpe
      else typeRef(NoPrefix, sym, List());

    override def toString(): String =
      if (!settings.debug.value && sym == RepeatedParamClass && !args.isEmpty)
	args(0).toString() + "*"
      else pre.prefixString + sym.nameString +
	(if (args.isEmpty) "" else args.mkString("[", ",", "]"));
  }

  /** A class representing a method type with parameters.
   */
  case class MethodType(override val paramTypes: List[Type],
                        override val resultType: Type) extends Type {

    override def paramSectionCount: int = resultType.paramSectionCount + 1;

    override def erasure = {
      val pts = paramTypes mapConserve (.erasure);
      val res = resultType.erasure;
      if ((pts eq paramTypes) && (res eq resultType)) this
      else MethodType(pts, res)
    }

    override def toString(): String = paramTypes.mkString("(", ",", ")") + resultType;
  }

  class ImplicitMethodType(pts: List[Type], rt: Type) extends MethodType(pts, rt);

  /** A class representing a polymorphic type or, if tparams.length == 0,
   *  a parameterless method type.
   */
  case class PolyType(override val typeParams: List[Symbol], override val resultType: Type)
       extends Type {

    override def paramSectionCount: int = resultType.paramSectionCount;

    override def paramTypes: List[Type] = resultType.paramTypes;

    override def erasure = resultType.erasure;

    override def toString(): String =
      (if (typeParams.isEmpty) "=> " else typeParams.mkString("[", ",", "]")) + resultType;

    override def cloneInfo(owner: Symbol) = {
      val tparams = typeParams map (.cloneSymbol(owner));
      for (val tparam <- tparams)
        tparam.setInfo(tparam.info.substSym(typeParams, tparams));
      PolyType(tparams, resultType.substSym(typeParams, tparams))
    }
  }

  /** A class representing a type variable
   */
  case class TypeVar(origin: Type, constr: TypeConstraint) extends Type {

    override def symbol = origin.symbol;

    override def toString(): String =
      if (constr.inst == NoType) "?" + origin else constr.inst.toString();
  }

  /** A class containing the alternatives and type prefix of an overloaded symbol
   */
  case class OverloadedType(pre: Type, alternatives: List[Symbol]) extends Type {
    override def prefix: Type = pre;
    override def toString() = (alternatives map pre.memberType).mkString("", " <and> ", "")
  }

  /** A class representing an as-yet unevaluated type.
   */
  abstract class LazyType extends Type {
    override def isComplete: boolean = false;
    override def complete(sym: Symbol): unit;
  }

  /** A class representing a lazy type with known type parameters
   */
  class LazyPolyType(override val typeParams: List[Symbol], restp: Type) extends LazyType {
    override def complete(sym: Symbol): unit = {
      restp.complete(sym);
    }
  }

// Creators ---------------------------------------------------------------

  /** Rebind symbol `sym' to an overriding member in type `pre' */
  private def rebind(pre: Type, sym: Symbol): Symbol = {
    val owner = sym.owner;
    if (owner.isClass && owner != pre.symbol && !sym.isFinal) {
      val rebind = pre.nonPrivateMember(sym.name).suchThat(.isStable);
      if (rebind == NoSymbol) sym else rebind
    } else sym
  }

  /** The canonical creator for single-types */
  def singleType(pre: Type, sym: Symbol): SingleType = {
    if (!pre.isStable && !pre.isError)
      throw new MalformedType(pre, sym.name.toString());
    new SingleType(pre, rebind(pre, sym)) {}
  }

  /** The canonical creator for typerefs */
  def typeRef(pre: Type, sym: Symbol, args: List[Type]): Type = {
    val sym1 = if (sym.isAbstractType) rebind(pre, sym) else sym;
    if (sym1.isAbstractType && !pre.isStable && !pre.isError)
      throw new MalformedType(pre, sym.nameString);
    if (sym1.isAliasType && sym1.typeParams.length == args.length) {
      if (sym1.hasFlag(LOCKED))
        throw new TypeError("illegal cyclic reference involving " + sym1);
      sym1.setFlag(LOCKED);
      val result = sym1.info.asSeenFrom(pre, sym1.owner).subst(sym1.typeParams, args);
      sym1.resetFlag(LOCKED);
      result
    } else {
      new ExtTypeRef(pre, sym1, args) {}
    }
  }

  /** the canonical creator for a refined type with a given scope */
  def refinedType(parents: List[Type], owner: Symbol, decls: Scope): RefinedType = {
    val clazz = owner.newRefinementClass(Position.NOPOS);
    val result = new RefinedType(parents, decls) {
      override def symbol: Symbol = clazz
    }
    clazz.setInfo(result);
    result
  }

  /** the canonical creator for a refined type with an initially empty scope */
  def refinedType(parents: List[Type], owner: Symbol): RefinedType =
    refinedType(parents, owner, new Scope);

  /** A creator for intersection type where intersections of a single type are
   *  replaced by the type itself. */
  def intersectionType(tps: List[Type]): Type = tps match {
    case List(tp) => tp
    case _ => refinedType(tps, commonOwner(tps))
  }

  /** A creator for type applications */
  def appliedType(tycon: Type, args: List[Type]): Type = tycon match {
    case TypeRef(pre, sym, _) => typeRef(pre, sym, args)
    case PolyType(tparams, restpe) => restpe.subst(tparams, args)
    case ErrorType => tycon
  }

// Helper Classes ---------------------------------------------------------

  /** A class expressing upper and lower bounds constraints
   *  for type variables, as well as their instantiations */
  class TypeConstraint {
    var lobounds: List[Type] = List();
    var hibounds: List[Type] = List();
    var inst: Type = NoType;

    def instantiate(tp: Type): boolean =
      if (lobounds.forall(.<:<(tp)) && hibounds.forall(tp.<:<)) {
        inst = tp; true
      } else false;
  }

  /** A prototype for mapping a function over all possible types
   */
  trait TypeMap extends Function1[Type, Type] {
    // deferred inherited: def apply(tp: Type): Type

    /** Map this function over given type */
    def mapOver(tp: Type): Type = tp match {
      case ErrorType | WildcardType | NoType | NoPrefix | ThisType(_) =>
        tp
      case SingleType(pre, sym) =>
        val pre1 = this(pre);
        if (pre1 eq pre) tp
        else singleType(pre1, sym)
      case ConstantType(base, value) =>
        val base1 = this(base);
        if (base1 eq base) tp
        else ConstantType(base1, value)
      case TypeRef(pre, sym, args) =>
        val pre1 = this(pre);
	val args1 = args mapConserve this;
        if ((pre1 eq pre) && (args1 eq args)) tp
        else typeRef(pre1, sym, args1)
      case TypeBounds(lo, hi) =>
        val lo1 = this(lo);
        val hi1 = this(hi);
        if ((lo1 eq lo) && (hi1 eq hi)) tp
        else TypeBounds(lo1, hi1)
      case RefinedType(parents, refinement) =>
        val parents1 = parents mapConserve this;
        val refinement1 = mapOver(refinement);
        if ((parents1 eq parents) && (refinement1 eq refinement)) tp
        else {
          val result = refinedType(parents1, tp.symbol.owner);
          val syms1 = refinement1.toList;
          for (val sym <- syms1)
            result.decls.enter(sym.cloneSymbol(result.symbol));
          val syms2 = result.decls.toList;
          val resultThis = ThisType(result.symbol);
          for (val sym <- syms2)
            sym.setInfo(sym.info.substSym(syms1, syms2).substThis(tp.symbol, resultThis));
          result
        }
      case MethodType(paramtypes, result) =>
        val paramtypes1 = paramtypes mapConserve this;
        val result1 = this(result);
        if ((paramtypes1 eq paramtypes) && (result1 eq result)) tp
        else MethodType(paramtypes1, result1)
      case PolyType(tparams, result) =>
        val tparams1 = mapOver(tparams);
        var result1 = this(result);
        if ((tparams1 eq tparams) && (result1 eq result)) tp
        else PolyType(tparams1, result1.substSym(tparams, tparams1))
      case OverloadedType(pre, alts) =>
        val pre1 = this(pre);
        if (pre1 eq pre) tp
        else OverloadedType(pre1, alts)
      case TypeVar(_, constr) =>
	if (constr.inst != NoType) this(constr.inst)
	else tp
      case _ =>
        throw new Error("mapOver inapplicable for " + tp);
    }

    /** Map this function over given scope */
    private def mapOver(scope: Scope): Scope = {
      val elems = scope.toList;
      val elems1 = mapOver(elems);
      if (elems1 eq elems) scope
      else new Scope(elems1)
    }

    /** Map this function over given list of symbols */
    private def mapOver(syms: List[Symbol]): List[Symbol] = {
      val infos = syms map (.info);
      val infos1 = infos mapConserve this;
      if (infos1 eq infos) syms
      else {
        val syms1 = syms map (.cloneSymbol);
        List.map2(syms1, infos1)
          ((sym1, info1) => sym1.setInfo(info1.substSym(syms, syms1)))
      }
    }
  }

  /** A map to compute the asSeenFrom method  */
  class AsSeenFromMap(pre: Type, clazz: Symbol) extends TypeMap {
    def apply(tp: Type): Type =
      if (pre == NoType || pre == NoPrefix || !clazz.isClass) tp
      else tp match {
        case ThisType(sym) =>
          def toPrefix(pre: Type, clazz: Symbol): Type =
            if (pre == NoType || pre == NoPrefix || !clazz.isClass) tp
            else if ((sym isSubClass clazz) && (pre.widen.symbol isSubClass sym)) pre
            else toPrefix(pre.baseType(clazz).prefix, clazz.owner);
          toPrefix(pre, clazz)
        case SingleType(pre, sym) =>
          try {
            mapOver(tp)
          } catch {
            case ex: MalformedType => apply(tp.singleDeref) // todo: try needed?
          }
	case TypeRef(prefix, sym, args) if (sym.isTypeParameter) =>
	  def toInstance(pre: Type, clazz: Symbol): Type =
	    if (pre == NoType || pre == NoPrefix || !clazz.isClass) tp
	    else {
	      val symclazz = sym.owner;
	      def throwError =
		throw new Error("" + tp + " in " + symclazz +
				"cannot be instantiated from " + pre.widen);
	      def instParam(ps: List[Symbol], as: List[Type]): Type =
		if (ps.isEmpty) throwError
		else if (sym eq ps.head) as.head
		else instParam(ps.tail, as.tail);
	      if (symclazz == clazz && (pre.widen.symbol isSubClass symclazz))
		pre.baseType(symclazz) match {
		  case TypeRef(_, basesym, baseargs) =>
		    instParam(basesym.typeParams, baseargs);
		  case _ =>
                    throwError
		}
	      else toInstance(pre.baseType(clazz).prefix, clazz.owner)
	    }
	  toInstance(pre, clazz)
        case _ =>
          mapOver(tp)
      }
  }

  /** A base class to compute all substitutions */
  abstract class SubstMap[T](from: List[Symbol], to: List[T]) extends TypeMap {

    /** Are sym1, sym1 the same. Can be tunded by subclasses */
    protected def matches(sym: Symbol, sym1: Symbol): boolean = sym eq sym1;

    /** Map target to type, can be tuned by subclasses */
    protected def toType(fromtp: Type, t: T): Type;

    def apply(tp: Type): Type = {
      def subst(sym: Symbol, from: List[Symbol], to: List[T]): Type =
        if (from.isEmpty) tp
        else if (matches(from.head, sym)) toType(tp, to.head)
        else subst(sym, from.tail, to.tail);
      tp match {
        case TypeRef(NoPrefix, sym, _) =>
          subst(sym, from, to)
        case SingleType(NoPrefix, sym) =>
          subst(sym, from, to)
	case PolyType(tparams, restp) =>
	  assert(!(tparams exists (from contains)));
	  mapOver(tp)
        case _ =>
          mapOver(tp)
      }
    }
  }

  /** A map to implement the substSym method */
  class SubstSymMap(from: List[Symbol], to: List[Symbol])
  extends SubstMap(from, to) {
    protected def toType(fromtp: Type, sym: Symbol) = fromtp match {
      case TypeRef(pre, _, args) => typeRef(pre, sym, args)
      case SingleType(pre, _) => singleType(pre, sym)
    }
  }

  /** A map to implement the subst method */
  class SubstTypeMap(from: List[Symbol], to: List[Type])
  extends SubstMap(from, to) {
    protected def toType(fromtp: Type, tp: Type) = tp;
  }

  /** A map to implement the substThis method */
  class SubstThisMap(from: Symbol, to: Type) extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case ThisType(sym) if (sym == from) => to
      case _ => mapOver(tp)
    }
  }

  /** A map to convert every occurrence of a wildcard type to a fresh
   *  type variable */
  object wildcardToTypeVarMap extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case WildcardType => TypeVar(tp, new TypeConstraint)
      case _ => mapOver(tp)
    }
  }

  /** A map to implement the contains method */
  class ContainsMap(sym: Symbol) extends TypeMap {
    var result = false;
    def apply(tp: Type): Type = {
      if (!result) {
        tp match {
          case TypeRef(_, sym1, _) if (sym == sym1) => result = true
          case SingleType(_, sym1) if (sym == sym1) => result = true
          case _ => mapOver(tp)
        }
      }
      tp
    }
  }

  /** A map to compute the most deeply nested owner that contains all the symbols
   *  of thistype or prefixless typerefs/singletype occurrences in given type */
  object commonOwnerMap extends TypeMap {
    var result: Symbol = _;
    def init = { result = NoSymbol }
    def apply(tp: Type): Type = tp match {
      case ThisType(sym) =>
        register(sym);
        tp
      case TypeRef(NoPrefix, sym, args) =>
        register(sym.owner);
        for (val arg <- args) apply(arg);
        tp
      case SingleType(NoPrefix, sym) =>
        register(sym.owner);
        tp
      case _ =>
        mapOver(tp)
    }
    private def register(sym: Symbol) =
      if (result == NoSymbol || (result isNestedIn sym)) result = sym
      else assert(result == sym || (sym isNestedIn result));
  }

// Helper Methods  -------------------------------------------------------------

  /** Do tp1 and tp2 denote equivalent types? */
  def isSameType(tp1: Type, tp2: Type): boolean = (tp1 eq tp2) || {
    Pair(tp1, tp2) match {
      case Pair(ErrorType, _)
	 | Pair(WildcardType, _)
         | Pair(_, ErrorType)
         | Pair(_, WildcardType) =>
	true
      case Pair(NoType, _)
	 | Pair(NoPrefix, _)
         | Pair(_, NoType)
         | Pair(_, NoPrefix) =>
	false
      case Pair(ThisType(sym1), ThisType(sym2)) =>
        sym1 == sym2
      case Pair(SingleType(pre1, sym1), SingleType(pre2, sym2))
      if (sym1 == sym2 && pre1 =:= pre2) =>
        true
      case Pair(SingleType(pre1, sym1), ThisType(sym2))
      if (sym1.isModule &&
	  sym1.moduleClass == sym2 &&
	  pre1 =:= sym2.owner.thisType) =>
        true
      case Pair(ThisType(sym1), SingleType(pre2, sym2))
      if (sym2.isModule &&
	  sym2.moduleClass == sym1 &&
	  pre2 =:= sym1.owner.thisType) =>
        true
      case Pair(ConstantType(base1, value1), ConstantType(base2, value2)) =>
	base1 =:= base2 && value1 == value2
      case Pair(TypeRef(pre1, sym1, args1), TypeRef(pre2, sym2, args2)) =>
	sym1 == sym2 && pre1 =:= pre2 && isSameTypes(args1, args2)
      case Pair(RefinedType(parents1, ref1), RefinedType(parents2, ref2)) =>
	def isSubScope(s1: Scope, s2: Scope): boolean = s2.toList.forall {
	  sym2 =>
            val sym1 = s1.lookup(sym2.name);
            sym1.info =:= sym2.info.substThis(sym2.owner, sym1.owner.thisType)
	}
	isSameTypes(parents1, parents2) && isSubScope(ref1, ref2) && isSubScope(ref2, ref1)
      case Pair(MethodType(pts1, res1), MethodType(pts2, res2)) =>
        pts1.length == pts2.length &&
        isSameTypes(pts1, pts2) &&
        res1 =:= res2
      case Pair(PolyType(tparams1, res1), PolyType(tparams2, res2)) =>
        tparams1.length == tparams2.length &&
        List.forall2(tparams1, tparams2)
          ((p1, p2) => p1.info =:= p2.info.substSym(tparams2, tparams1)) &&
        res1 =:= res2.substSym(tparams2, tparams1)
      case Pair(TypeBounds(lo1, hi1), TypeBounds(lo2, hi2)) =>
	lo1 =:= lo2 && hi1 =:= hi2
      case Pair(TypeVar(_, constr1), _) =>
	if (constr1.inst != NoType) constr1.inst =:= tp2
	else constr1 instantiate (wildcardToTypeVarMap(tp2))
      case Pair(_, TypeVar(_, constr2)) =>
	if (constr2.inst != NoType) tp1 =:= constr2.inst
	else constr2 instantiate (wildcardToTypeVarMap(tp1))
      case Pair(SingleType(_, _), _)
      if (tp2.isStable && tp1.singleDeref =:= tp2) =>
        true
      case Pair(_, SingleType(_, _))
      if (tp1.isStable && tp1 =:= tp2.singleDeref) =>
        true
      case _ =>
        false
    }
  }

  /** Are tps1 and tps2 lists of pairwise equivalent types? */
  def isSameTypes(tps1: List[Type], tps2: List[Type]): boolean =
    tps1.length == tps2.length &&
    List.forall2(tps1, tps2)((tp1, tp2) => tp1 =:= tp2);

  /** Does tp1 conform to tp2? */
  def isSubType(tp1: Type, tp2: Type): boolean = (tp1 eq tp2) || {
    Pair(tp1, tp2) match {
      case Pair(ErrorType, _)
         | Pair(WildcardType, _)
         | Pair(_, ErrorType)
         | Pair(_, WildcardType) =>
        true
      case Pair(NoType, _)
         | Pair(NoPrefix, _)
         | Pair(_, NoType)
         | Pair(_, NoPrefix) =>
        false
      case Pair(ThisType(_), ThisType(_))
         | Pair(ThisType(_), SingleType(_, _))
         | Pair(SingleType(_, _), ThisType(_))
         | Pair(SingleType(_, _), SingleType(_, _))
         | Pair(ConstantType(_, _), ConstantType(_, _)) =>
        tp1 =:= tp2
      case Pair(TypeRef(pre1, sym1, args1), TypeRef(pre2, sym2, args2)) =>
        def isSubArgs(tps1: List[Type], tps2: List[Type],
                      tparams: List[Symbol]): boolean = {
          tps1.isEmpty && tps2.isEmpty
          ||
          !tps1.isEmpty && !tps2.isEmpty &&
          (if (tparams.head.hasFlag(COVARIANT)) tps1.head <:< tps2.head
           else if (tparams.head.hasFlag(CONTRAVARIANT)) tps2.head <:< tps1.head
           else tps1.head =:= tps2.head) &&
           isSubArgs(tps1.tail, tps2.tail, tparams.tail)
        }
        sym1 == sym2 && (pre1 <:< pre2) &&
        isSubArgs(args1, args2, sym1.typeParams)
        ||
        sym1.isAbstractType && (tp1.bounds.hi <:< tp2)
        ||
        sym2.isAbstractType && (tp1 <:< tp2.bounds.lo)
        ||
        sym2.isClass &&
          ({ val base = tp1 baseType sym2; !(base eq tp1) && (base <:< tp2) })//debug
        ||
        sym1 == AllClass
        ||
        sym1 == AllRefClass && sym2 != AllClass && tp2 <:< AnyRefClass.tpe
      case Pair(MethodType(pts1, res1), MethodType(pts2, res2)) =>
        pts1.length == pts2.length &&
        isSameTypes(pts1, pts2) && (res1 <:< res2)
      case Pair(PolyType(tparams1, res1), PolyType(tparams2, res2)) =>
        tparams1.length == tparams2.length &&
        List.forall2(tparams1, tparams2)
          ((p1, p2) => p2.info.substSym(tparams2, tparams1) <:< p1.info) &&
        res1 <:< res2.substSym(tparams2, tparams1)
      case Pair(TypeBounds(lo1, hi1), TypeBounds(lo2, hi2)) =>
        lo2 <:< lo1 && hi1 <:< hi2
      case Pair(_, TypeVar(_, constr2)) =>
        if (constr2.inst != NoType) tp1 <:< constr2.inst
        else { constr2.lobounds = tp1 :: constr2.lobounds; true }
      case Pair(TypeVar(_, constr1), _) =>
        if (constr1.inst != NoType) constr1.inst <:< tp2
        else { constr1.hibounds = tp2 :: constr1.hibounds; true }
      case Pair(_, RefinedType(parents2, ref2)) =>
        (parents2 forall tp1.<:<) && (ref2.toList forall tp1.specializes)
      case Pair(RefinedType(parents1, ref1), _) =>
        parents1 exists (.<:<(tp2))
      /* todo: replace following with
      case Pair(ThisType(_), _)
         | Pair(SingleType(_, _), _)
         | Pair(ConstantType(_, _), _) =>
	 once patern matching bug is fixed */
      case Pair(ThisType(_), _) => tp1.singleDeref <:< tp2
      case Pair(SingleType(_, _), _) => tp1.singleDeref <:< tp2
      case Pair(ConstantType(_, _), _) => tp1.singleDeref <:< tp2

      case Pair(TypeRef(pre1, sym1, args1), _) =>
        sym1 == AllClass && tp2 <:< AnyClass.tpe
        ||
        sym1 == AllRefClass && tp2.symbol != AllClass && tp2 <:< AnyRefClass.tpe
      case _ =>
        false
    }
  }

  /** Are tps1 and tps2 lists of equal length such that all elements
   *  of tps1 conform to corresponding elements of tps2? */
  def isSubTypes(tps1: List[Type], tps2: List[Type]): boolean =
    tps1.length == tps2.length &&
    List.forall2(tps1, tps2)((tp1, tp2) => tp1 <:< tp2);

  /** Does type `tp' implement symbol `sym' with same or stronger type?
   *  Exact only if `sym' is a member of some refinement type, otherwise
   *  we might return false negatives */
  def specializesSym(tp: Type, sym: Symbol): boolean =
    tp.symbol == AllClass ||
    tp.symbol == AllRefClass && (sym.owner isSubClass AnyRefClass) ||
    (tp.nonPrivateMember(sym.name).alternatives exists
      (alt => sym == alt || specializesSym(tp.narrow, alt, ThisType(sym.owner), sym)));

  /** Does member `sym1' of `tp1' have a stronger type than member `sym2' of `tp2'? */
  private def specializesSym(tp1: Type, sym1: Symbol, tp2: Type, sym2: Symbol): boolean = {
    val info1 = tp1.memberInfo(sym1);
    val info2 = tp2.memberInfo(sym2).substThis(tp2.symbol, tp1);
    sym2.isTerm &&
      info1 <:< info2 ||
    sym2.isAbstractType &&
      (info2.bounds containsType info1) ||
    sym2.isAliasType &&
      tp2.memberType(sym2) =:= tp1.memberType(sym1)
  }

  /** A function implementing tp1 matches tp2 */
  private def matchesType(tp1: Type, tp2: Type): boolean = Pair(tp1, tp2) match {
    case Pair(MethodType(pts1, res1), MethodType(pts2, res2)) =>
      isSameTypes(pts1, pts2) && (res1 matches res2)
    case Pair(PolyType(tparams1, res1), PolyType(tparams2, res2)) =>
      tparams1.length == tparams2.length &&
      (res1 matches res2.substSym(tparams2, tparams1))
    case Pair(MethodType(_, _), _) | Pair(PolyType(_, _), _) =>
      false
    case _ =>
      !phase.exactMatch || tp1 =:= tp2
  }

  /** Prepend type `tp' to closure `cl' */
  private def addClosure(tp: Type, cl: Array[Type]): Array[Type] = {
    val cl1 = new Array[Type](cl.length + 1);
    cl1(0) = tp;
    System.arraycopy(cl, 0, cl1, 1, cl.length);
    cl1
  }

// Lubs and Glbs ---------------------------------------------------------

  private val recLimit = 10;
  private var recCount = 0;
  private var giveUp: boolean = _;

  /** Return op(tps), but give up if level of recursion is greater than
   *  recLimit */
  private def limitRecursion(tps: List[Type], boundkind: String,
                             op: List[Type] => Type): Type =
    if (recCount == recLimit) {
      giveUp = true;
      AnyClass.tpe
    } else {
      if (recCount == 0) giveUp = false;
      val result = try {
	recCount = recCount + 1;
	op(tps)
      } finally {
	recCount = recCount - 1
      }
      if (recCount == 0 && giveUp) {
	throw new TypeError("failure to compute " + boundkind +
			    " bound of types " +
			    tps.mkString("", " and ", ";\n") +
			    "an approximation is: " + result + ";\n" +
			    "additional type annotations are needed");
      }
      result
    }

  /** The greatest sorted upwards closed lower bound of a list of lists of
   *  types relative to the following ordering <= between lists of types:
   *
   *    xs <= ys   iff   forall y in ys exists x in xs such that x <: y
   *
   *  @See closure  for a definition of sorted and upwards closed.
   */
  private def glbList(tss: List[List[Type]]): List[Type] = {
    val tss1 = tss filter (ts => !ts.isEmpty);
    if (tss1.isEmpty) List()
    else if (tss1.tail.isEmpty) tss.head
    else {
      val ts0 = tss1 map (.head);
      val sym = minSym(ts0);
      val ts1 = elimSuper(ts0 filter (.symbol.==(sym)));
      mergePrefixAndArgs(ts1, -1) match {
        case Some(tp0) =>
          tp0 :: glbList(tss1 map (ts => if (ts.head.symbol == sym) ts.tail else ts))
        case None =>
          throw new MalformedClosure(ts1)
      }
    }
  }

  /** The greatest sorted upwards closed lower bound of a list of closures.
   *  @See glbList for more explanations.
   */
  private def glbArray(tss: List[Array[Type]]): Array[Type] = {
    val tss1 = tss map (ts: Array[Type] => List.fromArray(ts));
    val glbs = glbList(tss1);
    val result = new Array[Type](glbs.length);
    var i = 0;
    for (val x <- glbs.elements) { result(i) = x; i = i + 1; }
    result;
    // Predef.Array(glbs: _*);
  }

  /** The least sorted upwards closed upper bound of a non-empty list
   *  of lists of types.
   *  @See glbList for more explanations. */
  private def lubList(tss: List[List[Type]]): List[Type] =
    if (tss.tail.isEmpty) tss.head
    else if (tss exists (.isEmpty)) List()
    else {
      val ts0 = tss map (.head);
      val sym = minSym(ts0);
      if (ts0 forall (t => t.symbol == sym))
	mergePrefixAndArgs(elimSub(ts0), 1).toList ::: lubList(tss map (.tail))
      else
	lubList(tss map (ts => if (ts.head.symbol == sym) ts.tail else ts))
    }

  /** The least sorted upwards closed upper bound of a non-empty list
   *  of closures.
   *  @See lubList for more explanations. */
  private def lubArray(tss: List[Array[Type]]): Array[Type] =
    Predef.Array(lubList(tss map (ts: Array[Type] => List.fromArray(ts))): _* );

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
  private def elimSuper(ts: List[Type]): List[Type] =
    ts filter (t => !(ts exists (t1 => t1 <:< t)));

  /** Eliminate from list of types all elements which are a subtype
   *  of some other element of the list. */
  private def elimSub(ts: List[Type]): List[Type] =
    ts filter (t => !(ts exists (t1 => t <:< t1)));

  /** The least upper bound wrt <:< of a list of types */
  def lub(ts: List[Type]): Type = {
    def lub0(ts0: List[Type]): Type = elimSub(ts0) match {
      case List() => AllClass.tpe
      case List(t) => t
      case ts @ PolyType(tparams, _) :: _ =>
	PolyType(
	  List.map2(tparams, List.transpose(matchingBounds(ts, tparams)))
	  ((tparam, bounds) => tparam.cloneSymbol.setInfo(glb(bounds))),
	  lub0(ts map (.resultType)))
      case ts @ MethodType(pts, _) :: rest =>
        MethodType(pts, lub0(matchingRestypes(ts, pts)))
      case ts =>
        val closures: List[Array[Type]] = ts map (.closure);
        val lubBaseTypes: Array[Type] = lubArray(closures);
        val lubParents = spanningTypes(List.fromArray(lubBaseTypes));
	val lubBase = refinedType(lubParents, commonOwner(ts));
        val lubType = refinedType(lubParents, lubBase.symbol);
        val lubThisType = lubType.symbol.thisType;
        val narrowts = ts map (.narrow);
        def lubsym(proto: Symbol): Symbol = {
          val prototp = lubThisType.memberInfo(proto);
          val syms = narrowts map (t =>
	    t.nonPrivateMember(proto.name).suchThat(sym =>
	      sym.tpe matches prototp.substThis(lubThisType.symbol, t)));
          val symtypes = List.map2(narrowts, syms)
            ((t, sym) => t.memberInfo(sym).substThis(t.symbol, lubThisType));
          if (syms contains NoSymbol)
            NoSymbol
          else if (proto.isTerm)
            proto.cloneSymbol.setInfo(lub(symtypes))
          else if (symtypes.tail forall (symtypes.head =:=))
            proto.cloneSymbol.setInfo(symtypes.head)
          else {
            def lubBounds(bnds: List[TypeBounds]): TypeBounds =
              TypeBounds(glb(bnds map (.lo)), lub(bnds map (.hi)));
            proto.owner.newAbstractType(proto.pos, proto.name)
              .setInfo(lubBounds(symtypes map (.bounds)))
          }
        }
        def refines(tp: Type, sym: Symbol): boolean = {
	  val syms = tp.nonPrivateMember(sym.name).alternatives;
	  !syms.isEmpty && (syms forall (alt => !specializesSym(lubThisType, sym, tp, alt)))
        }
        for (val sym <- lubBase.nonPrivateMembers)
          // add a refinement symbol for all non-class members of lubBase
          // which are refined by every type in ts.
          if (!sym.isClass && (narrowts forall (t => refines(t, sym))))
            addMember(lubThisType, lubType, lubsym(sym));
        if (lubType.decls.isEmpty) lubBase else lubType;
    }
    limitRecursion(ts, "least upper", lub0);
  }

  /** The greatest lower bound wrt <:< of a list of types */
  def glb(ts: List[Type]): Type = {
    def glb0(ts0: List[Type]): Type = elimSuper(ts0) match {
      case List() => AnyClass.tpe
      case List(t) => t
      case ts @ PolyType(tparams, _) :: _ =>
        PolyType(
          List.map2(tparams, List.transpose(matchingBounds(ts, tparams)))
          ((tparam, bounds) => tparam.cloneSymbol.setInfo(lub(bounds))),
          glb0(ts map (.resultType)))
      case ts @ MethodType(pts, _) :: rest =>
        MethodType(pts, glb0(matchingRestypes(ts, pts)))
      case ts =>
	try {
          val glbBase = refinedType(ts, commonOwner(ts));
          val glbType = refinedType(ts, glbBase.symbol);
          val glbThisType = glbType.symbol.thisType;
          def glbsym(proto: Symbol): Symbol = {
            val prototp = glbThisType.memberInfo(proto);
            val syms = for (
	      val t <- ts;
              val alt <- t.nonPrivateMember(proto.name).alternatives;
              glbThisType.memberInfo(alt) matches prototp) yield alt;
            val symtypes = syms map glbThisType.memberInfo;
            assert(!symtypes.isEmpty);
            proto.cloneSymbol.setInfo(
              if (proto.isTerm) glb(symtypes)
              else {
                def isTypeBound(tp: Type) = tp match {
                  case TypeBounds(_, _) => true
                  case _ => false
                }
                def glbBounds(bnds: List[Type]): TypeBounds = {
                  val lo = lub(bnds map (.bounds.lo));
                  val hi = glb(bnds map (.bounds.hi));
                  if (lo <:< hi) TypeBounds(lo, hi)
                  else throw new MalformedClosure(bnds)
                }
                val symbounds = symtypes filter isTypeBound;
                var result: Type =
                  if (symbounds.isEmpty)
                    TypeBounds(AllClass.tpe, AnyClass.tpe)
                  else glbBounds(symbounds);
                for (val t <- symtypes; !isTypeBound(t))
                  if (result.bounds containsType t) result = t
                  else throw new MalformedClosure(symtypes);
                result
              })
          }
          for (val t <- ts; val sym <- t.nonPrivateMembers)
            if (!(sym.isClass || (glbThisType specializes sym)))
	      addMember(glbThisType, glbType, glbsym(sym));
	  if (glbType.decls.isEmpty) glbBase else glbType;
	} catch {
          case _: MalformedClosure =>
            if (ts forall (t => t <:< AnyRefClass.tpe)) AllRefClass.tpe
            else AllClass.tpe
        }
    }
    limitRecursion(ts, "greatest lower", glb0)
  }

  /** The most deeply nested owner that contains all the symbols
   *  of thistype or prefixless typerefs/singletype occurrences in given type */
  private def commonOwner(t: Type): Symbol = {
    commonOwnerMap.init;
    commonOwnerMap.apply(t);
    commonOwnerMap.result
  }

  /** The most deeply nested owner that contains all the symbols
   *  of thistype or prefixless typerefs/singletype occurrences in given list of types */
  private def commonOwner(tps: List[Type]): Symbol = {
    commonOwnerMap.init;
    tps mapConserve commonOwnerMap;
    commonOwnerMap.result
  }

  /** Compute lub (if variance == 1) or glb (if variance == 0) of given list of types
   *  `tps'. All types in `tps' are typerefs or singletypes with the same symbol.
   *  Return Some(x) if the computation succeeds with result `x'.
   *  Return None if the computuation fails.
   */
  private def mergePrefixAndArgs(tps: List[Type], variance: int): Option[Type] = tps match {
    case List(tp) =>
      Some(tp)
    case TypeRef(_, sym, _) :: rest =>
      val pres = tps map (.prefix);
      val pre = if (variance == 1) lub(pres) else glb(pres);
      val argss = tps map (.typeArgs);
      val args =
	List.map2(sym.typeParams, List.transpose(argss))
        ((tparam, as) =>
	  if (tparam.variance == variance) lub(as)
	  else if (tparam.variance == -variance) glb(as)
          else NoType);
      try {
	if (args contains NoType) None
	else Some(typeRef(pre, sym, args))
      } catch {
	case ex: MalformedType => None
      }
    case SingleType(_, sym) :: rest =>
      val pres = tps map (.prefix);
      val pre = if (variance == 1) lub(pres) else glb(pres);
      try {
	Some(singleType(pre, sym))
      } catch {
	case ex: MalformedType => None
      }
  }

  /** Make symbol `sym' a member of scope `tp.decls' where `thistp' is the narrowed
   *  owner type of the scope */
  private def addMember(thistp: Type, tp: Type, sym: Symbol): unit = {
    if (!(thistp specializes sym)) {
      if (sym.isTerm)
        for (val alt <- tp.nonPrivateDecl(sym.name).alternatives)
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
	throw new Error("lub/glb of incompatible types: " + tps.mkString("", " and ", ""))
    }

  /** All types in list must be method types with equal parameter types.
   *  Returns list of their result types.
   */
  private def matchingRestypes(tps: List[Type], pts: List[Type]): List[Type] =
    tps map {
      case MethodType(pts1, res) if (isSameTypes(pts1, pts)) =>
	res
      case _ =>
	throw new Error("lub/glb of incompatible types: " + tps.mkString("", " and ", ""))
    }

// Errors and Diagnostics ---------------------------------------------------------

  /** An exception signalling a type error */
  class TypeError(msg: String) extends java.lang.Error(msg);

  /** An exception signalling a malformed type */
  class MalformedType(msg: String) extends TypeError(msg) {
    def this(pre: Type, tp: String) = this("malformed type: " + pre + "#" + tp);
  }

  /** An exception signalling a malformed closure */
  class MalformedClosure(ts: List[Type])
       extends TypeError("no common type instance of base types " +
                         ts.mkString("", " and ", "") + " exists");

  /** An exception signalling a variance annotation/usage conflict */
  class VarianceError(msg: String) extends TypeError(msg);

  /** The current indentation string for traces */
  private var indent: String = "";

  /** Perform operation `p' on arguments `tp1', `arg2' and print trace of computation */
  private def explain[T](op: String, p: (Type, T) => boolean, tp1: Type, arg2: T): boolean = {
    System.out.println(indent + tp1 + " " + op + " " + arg2 + "?");
    indent = indent + "  ";
    val result = p(tp1, arg2);
    indent = indent.substring(0, indent.length() - 2);
    System.out.println(indent + result);
    result
  }

  /** If option `explaintypes' is set, print a subtype trace for `found' <: `required' */
  def explainTypes(found: Type, required: Type): unit =
    if (settings.explaintypes.value) {
      val s = explainSwitch;
      explainSwitch = true;
      found <:< required;
      explainSwitch = s
    }
}

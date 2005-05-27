/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.symtab;

import scala.tools.util.{AbstractFile, Position}
import Flags._;

abstract class Symbols: SymbolTable {
  import definitions._;

  private var ids = 0;

  val emptySymbolArray = new Array[Symbol](0);

  /** The class for all symbols */
  abstract class Symbol(initOwner: Symbol, initPos: int, initName: Name) {

    var owner = initOwner;
    var name = initName;
    var pos = initPos;
    val id = { ids = ids + 1; ids }
    private var rawflags: long = 0;

// Creators -------------------------------------------------------------------

    final def newValue(pos: int, name: Name) =
      new TermSymbol(this, pos, name);
    final def newVariable(pos: int, name: Name) =
      newValue(pos, name).setFlag(MUTABLE);
    final def newValueParameter(pos: int, name: Name) =
      newValue(pos, name).setFlag(PARAM);
    final def newLocalDummy(pos: int) =
      newValue(pos, nme.LOCAL(owner)).setInfo(NoType);
    final def newMethod(pos: int, name: Name) =
      newValue(pos, name).setFlag(METHOD);
    final def newLabel(pos: int, name: Name) =
      newMethod(pos, name).setFlag(LABEL);
    final def newConstructor(pos: int) =
      newMethod(pos, nme.CONSTRUCTOR);
    final def newModule(pos: int, name: Name, clazz: ClassSymbol) =
      new ModuleSymbol(this, pos, name, clazz);
    final def newModule(pos: int, name: Name) =
      new ModuleSymbol(this, pos, name, null);
    final def newPackage(pos: int, name: Name) = {
      assert(isPackageClass);
      val m = newModule(pos, name);
      m.setFlag(JAVA | PACKAGE);
      m.moduleClass.setFlag(JAVA | PACKAGE);
      m
    }
    final def newThisSym(pos: int) = {
      newValue(pos, nme.this_).setFlag(SYNTHETIC);
    }
    final def newImport(pos: int) =
      newValue(pos, nme.IMPORT).setFlag(SYNTHETIC);
    final def newOverloaded(pre: Type, alternatives: List[Symbol]): Symbol =
      newValue(alternatives.head.pos, alternatives.head.name)
      .setFlag(OVERLOADED)
      .setInfo(OverloadedType(pre, alternatives));

    final def newErrorValue(name: Name) =
      newValue(pos, name).setFlag(SYNTHETIC | IS_ERROR).setInfo(ErrorType);
    final def newAliasType(pos: int, name: Name) =
      new TypeSymbol(this, pos, name);
    final def newAbstractType(pos: int, name: Name) =
      new TypeSymbol(this, pos, name).setFlag(DEFERRED);
    final def newTypeParameter(pos: int, name: Name) =
      newAbstractType(pos, name).setFlag(PARAM);
    final def newClass(pos: int, name: Name) =
      new ClassSymbol(this, pos, name);
    final def newAnonymousClass(pos: int) =
      newClass(pos, nme.ANON_CLASS_NAME.toTypeName);
    final def newRefinementClass(pos: int) =
      newClass(pos, nme.REFINE_CLASS_NAME.toTypeName);
    final def newErrorClass(name: Name) = {
      val clazz = newClass(pos, name).setFlag(SYNTHETIC | IS_ERROR);
      clazz.setInfo(ClassInfoType(List(), new ErrorScope(this), clazz));
      clazz
    }
    final def newErrorSymbol(name: Name): Symbol =
      if (name.isTypeName) newErrorClass(name) else newErrorValue(name);

// Tests ----------------------------------------------------------------------

    def isTerm = false;        //to be overridden
    def isType = false;        //to be overridden
    def isClass = false;       //to be overridden

    final def isValue = isTerm && !(isModule && hasFlag(PACKAGE | JAVA));
    final def isVariable = isTerm && hasFlag(MUTABLE);
    final def isGetter = isTerm && hasFlag(ACCESSOR) && !name.endsWith(nme._EQ);
    final def isSetter = isTerm && hasFlag(ACCESSOR) && name.endsWith(nme._EQ);
    final def isValueParameter = isTerm && hasFlag(PARAM);
    final def isLocalDummy = isTerm && (name startsWith nme.LOCAL_PREFIX);
    final def isMethod = isTerm && (flags & (METHOD | STABLE)) == METHOD;
    final def isLabel = isTerm && hasFlag(LABEL);
    final def isConstructor = isTerm && name == nme.CONSTRUCTOR;
    final def isModule = isTerm && hasFlag(MODULE);
    final def isPackage = isModule && hasFlag(PACKAGE);
    final def isThisSym = isTerm && name == nme.this_;
    final def isError = hasFlag(IS_ERROR);
    final def isTrait = isClass & hasFlag(TRAIT);
    final def isAliasType = isType && !isClass && !hasFlag(DEFERRED);
    final def isAbstractType = isType && !isClass && hasFlag(DEFERRED);
    final def isTypeParameter = isType && hasFlag(PARAM);
    final def isAnonymousClass = isClass && (name startsWith nme.ANON_CLASS_NAME); // startsWith necessary because name may grow when lifted
    final def isRefinementClass = isClass && name == nme.REFINE_CLASS_NAME.toTypeName; // no lifting for refinement classes
    final def isModuleClass = isClass && hasFlag(MODULE);
    final def isPackageClass = isClass && hasFlag(PACKAGE);
    final def isRoot = isPackageClass && name == nme.ROOT.toTypeName;
    final def isEmptyPackage = isPackage && name == nme.EMPTY_PACKAGE_NAME;
    final def isEmptyPackageClass = isPackageClass && name == nme.EMPTY_PACKAGE_NAME.toTypeName;

    /** Does this symbol denote a stable value? */
    final def isStable =
      isTerm && !hasFlag(MUTABLE) && (!hasFlag(METHOD) || hasFlag(STABLE));

    /** Does this symbol denote the primary constructor
     * of its enclosing class? */
    final def isPrimaryConstructor =
      isConstructor && owner.primaryConstructor == this;

    /** Is this symbol static (i.e. with no outer instance)? */
    final def isStatic: boolean = hasFlag(STATIC) || isRoot || owner.isStaticOwner;

    /** Does this symbol denote a class that defines static symbols? */
    final def isStaticOwner: boolean = isPackageClass || isStatic && isModuleClass;

    /** Is this symbol final?*/
    final def isFinal: boolean =
      hasFlag(FINAL) ||
      isTerm && (
        hasFlag(PRIVATE) || isLocal || owner.isClass && owner.hasFlag(FINAL | MODULE));

    /** Is this symbol a sealed class?*/
    final def isSealed: boolean =
      isClass && (hasFlag(SEALED) || isSubClass(AnyValClass) || isSubClass(ArrayClass));

    /** Is this symbol locally defined? I.e. not a member of a class or module */
    final def isLocal: boolean = owner.isTerm || hasFlag(LOCAL);

    /** Is this class locally defined?
     *  A class is local, if
     *   - it is anonymous, or
     *   - its owner is a value
     *   - it is defined within a local class
     */
    final def isLocalClass: boolean =
      isClass && (isAnonymousClass || isRefinementClass || isLocal ||
                  !owner.isPackageClass && owner.isLocalClass);

    /** Symbol was preloaded from package */
    final def isExternal: boolean = pos == Position.NOPOS;

    /** A a member of class `base' is incomplete if (1) it is declared deferred or
     *  (2) it is abstract override and its super symbol in `base' is nonexistent or inclomplete.
     */
    final def isIncompleteIn(base: Symbol): boolean =
      (this == NoSymbol) ||
      (this hasFlag DEFERRED) ||
      (this hasFlag ABSOVERRIDE) && isIncompleteIn(superSymbol(base));

    /** The variance of this symbol as an integer */
    final def variance: int =
      if (hasFlag(COVARIANT)) 1
      else if (hasFlag(CONTRAVARIANT)) -1
      else 0;

// Flags ----------------------------------------------------------------------------

    final def flags = rawflags;
    final def flags_=(fs: long) = rawflags = fs;
    final def setFlag(mask: long): this.type = { rawflags = rawflags | mask; this }
    final def resetFlag(mask: long): this.type = { rawflags = rawflags & ~mask; this }
    final def getFlag(mask: long): long = rawflags & mask;
    final def hasFlag(mask: long): boolean = (rawflags & mask) != 0;
    final def resetFlags: unit = { rawflags = rawflags & SourceFlags }

// Info and Type -------------------------------------------------------------------

    private var infos: TypeHistory = null;
    private var limit: Phase = NoPhase;

    /** Get type. The type of a symbol is:
     *  for a type symbol, the type corresponding to the symbol itself
     *  for a term symbol, its usual type
     */
    def tpe: Type = info;

    /** Get type info associated with symbol at current phase, after
     *  ensuring that symbol is initialized (i.e. type is completed).
     */
    final def info: Type = {
      var cnt = 0;
      while ((rawflags & INITIALIZED) == 0) {
        //if (settings.debug.value) System.out.println("completing " + this + " in " + this.owner);//DEBUG
        assert(infos != null, this.name);
	val tp = infos.info;
        if ((rawflags & LOCKED) != 0) {
          setInfo(ErrorType);
          throw CyclicReference(this, tp);
        }
        rawflags = rawflags | LOCKED;
	val current = phase;
	phase = infos.start;
        tp.complete(this);
	// if (settings.debug.value && (rawflags & INITIALIZED) != 0) System.out.println("completed " + this/* + ":" + info*/);//DEBUG

        rawflags = rawflags & ~LOCKED;
	phase = current;
	cnt = cnt + 1;
	// allow for two completions:
	//   one: sourceCompleter to LazyType, two: LazyType to completed type
	if (cnt == 3) throw new Error("no progress in completing " + this + ":" + tp);
      }
      rawInfo
    }

    /** Set initial info. */
    def setInfo(info: Type): this.type = {
      infos = new TypeHistory(phase, info, null);
      limit = phase;
      assert(info != null);
      rawflags = if (info.isComplete) rawflags | INITIALIZED & ~LOCKED;
		 else rawflags & ~INITIALIZED & ~LOCKED;
      this
    }

    /** Set new info valid from start of next phase. */
    final def updateInfo(info: Type): Symbol = {
      val current = phase;
      phase = phase.next;
      assert(infos.start.id <= phase.id);
      if (infos.start == phase) infos = infos.prev;
      infos = new TypeHistory(phase, info, infos);
      phase = current;
      this
    }

    /** Return info without checking for initialization or completing */
    final def rawInfo: Type = {
      if (limit.id < phase.id) {
	if ((rawflags & INITIALIZED) != 0) {
	  val current = phase;
          var itr = infoTransformers.nextFrom(limit);
          infoTransformers = itr; // caching optimization
          while (itr.phase != NoPhase && itr.phase.id < current.id) {
            phase = itr.phase;
            val info1 = itr.transform(this, infos.info);
	    if (!(info1 eq infos.info))
	      infos = new TypeHistory(phase.next, info1, infos);
            itr = itr.nextFrom(phase.next)
          }
          phase = current;
          limit = current;
	}
	assert(infos != null/*, name.toString() + " " + limit + " " + phase*/);
	infos.info
      } else {
	var infos = this.infos;
	while (phase.id < infos.start.id && infos.prev != null) infos = infos.prev;
	infos.info
      }
    }

    /** Initialize the symbol */
    final def initialize: Symbol = {
      if ((rawflags & INITIALIZED) == 0) info;
      this
    }

    /** Was symbol's type updated during given phase? */
    final def isUpdatedAt(phase: Phase): boolean = {
      var infos = this.infos;
      while (infos != null && infos.start != phase.next) infos = infos.prev;
      infos != null
    }

    /** The type constructor of a symbol is:
     *  For a type symbol, the type corresponding to the symbol itself,
     *  excluding parameters.
     *  Not applicable for term symbols.
     */
    def typeConstructor: Type = throw new Error("typeConstructor inapplicable for " + this);

    /** The type parameters of this symbol */
    def unsafeTypeParams: List[Symbol] = rawInfo.typeParams;

    def typeParams: List[Symbol] = {
      rawInfo.load(this); rawInfo.typeParams
    }

    /** Reset symbol to initial state
     */
    def reset(completer: Type): unit = {
      resetFlags;
      pos = Position.NOPOS;
      infos = null;
      setInfo(completer)
    }

// Comparisons ----------------------------------------------------------------

    /** A total ordering between symbols that refines the class
     *  inheritance graph (i.e. subclass.isLess(superclass) always holds).
     *  the ordering is given by: (isType, -|closure| for type symbols, id)
     */
    final def isLess(that: Symbol): boolean =
      if (this.isType)
	that.isType &&
	{val diff = this.info.closure.length - that.info.closure.length;
	 diff > 0 || diff == 0 && this.id < that.id}
      else
	that.isType || this.id < that.id;

    /** A partial ordering between symbols.
     *  (this isNestedIn that) holds iff this symbol is defined within
     *  a class or method defining that symbol
     */
    final def isNestedIn(that: Symbol): boolean =
      owner == that || owner != NoSymbol && (owner isNestedIn that);

    /** Is this class symbol a subclass of that symbol? */
    final def isSubClass(that: Symbol): boolean =
      this == that || this.isError || that.isError ||
      info.closurePos(that) >= 0 ||
      this == AllClass ||
      this == AllRefClass &&
	(that == AnyClass ||
         that != AllClass && (that isSubClass AnyRefClass));

// Overloaded Alternatives ---------------------------------------------------------

    def alternatives: List[Symbol] =
      if (hasFlag(OVERLOADED)) info.asInstanceOf[OverloadedType].alternatives
      else List(this);

    def filter(cond: Symbol => boolean): Symbol =
      if (hasFlag(OVERLOADED)) {
	val alts = alternatives;
	val alts1 = alts filter cond;
	if (alts1 eq alts) this
	else if (alts1.isEmpty) NoSymbol
	else if (alts1.tail.isEmpty) alts1.head
	else owner.newOverloaded(info.prefix, alts1)
      } else if (cond(this)) this
      else NoSymbol;

    def suchThat(cond: Symbol => boolean): Symbol = {
      val result = filter(cond);
      assert(!result.hasFlag(OVERLOADED)/*, result.alternatives*/);
      result
    }

// Cloneing -------------------------------------------------------------------

    /** A clone of this symbol */
    final def cloneSymbol: Symbol =
      cloneSymbol(owner);

    /** A clone of this symbol, but with given owner */
    final def cloneSymbol(owner: Symbol): Symbol =
      cloneSymbolImpl(owner).setInfo(info.cloneInfo(owner)).setFlag(flags);

    /** Internal method to clone a symbol's implementation without flags or type
     */
    def cloneSymbolImpl(owner: Symbol): Symbol;

// Access to related symbols --------------------------------------------------

    /** The next enclosing class */
    def enclClass: Symbol = if (isClass) this else owner.enclClass;

    /** The next enclosing method */
    def enclMethod: Symbol = if (isMethod) this else owner.enclMethod;

    /** The primary constructor of a class */
    def primaryConstructor: Symbol = info.decl(nme.CONSTRUCTOR).alternatives.head;

    /** The self symbol of a class with explicit self type, or else the symbol itself.
     */
    def thisSym: Symbol = this;

    /** The type of `this' in a class, or else the type of the symbol itself. */
    final def typeOfThis = thisSym.tpe;

    /** Sets the type of `this' in a class */
    def typeOfThis_=(tp: Type): unit = throw new Error("typeOfThis cannot be set for " + this);

    /** If symbol is a class, the type this.type in this class, otherwise NoPrefix */
    def thisType: Type = NoPrefix;

    /** Return every accessor of a primary constructor parameter in this case class */
    final def caseFieldAccessors: List[Symbol] = {
      assert(isClass && hasFlag(CASE));
      info.decls.toList.filter(sym => !sym.hasFlag(PARAM) && sym.hasFlag(PARAMACCESSOR))
    }

    /** The symbol accessed by this accessor function.
     */
    final def accessed: Symbol = {
      assert(hasFlag(ACCESSOR));
      val name1 = if (name.endsWith(nme._EQ)) name.subName(0, name.length - nme._EQ.length)
	          else name;
      owner.info.decl(name1.toString() + "$")
    }

    /** The class with the same name in the same package as this module or
     *  case class factory
     */
    final def linkedClass: Symbol = {
      if (owner.isPackageClass)
	owner.info.decl(name.toTypeName).suchThat(sym => sym.rawInfo != NoType)
      else NoSymbol;
    }

    /** The module or case class factory with the same name in the same
     *  package as this class.
     */
    final def linkedModule: Symbol =
      if (owner.isPackageClass)
	owner.info.decl(name.toTermName).suchThat(
          sym => (sym hasFlag MODULE) && (sym.rawInfo != NoType));
      else NoSymbol;

    /** The top-level class containing this symbol */
    def toplevelClass: Symbol =
      if (isClass && owner.isPackageClass) this else owner.toplevelClass;

    /** For a module its linked class, for a class its linked module or case factory otherwise */
    final def linkedSym: Symbol =
      if (isTerm) linkedClass
      else if (isClass && owner.isPackageClass)
        owner.info.decl(name.toTermName).suchThat(sym => sym.rawInfo != NoType)
      else NoSymbol;

    /** The module corresponding to this module class (note that this
     *  is not updated when a module is cloned).
     */
    def sourceModule: Symbol = NoSymbol;

    /** The module class corresponding to this module.
     */
    def moduleClass: Symbol = NoSymbol;

    /** The symbol overridden by this symbol in given base class */
    final def overriddenSymbol(base: Symbol): Symbol =
      base.info.nonPrivateDecl(name).suchThat(sym =>
        sym.isType || (owner.thisType.memberType(sym) matches tpe));

    /** The symbol overriding this symbol in given subclass */
    final def overridingSymbol(base: Symbol): Symbol =
      base.info.nonPrivateDecl(name).suchThat(sym =>
        sym.isType || (base.thisType.memberType(sym) matches base.thisType.memberType(this)));

    /** The symbol accessed by a super in the definition of `this' when seen from
     *  class `base'. This symbol is always concrete.
     *  pre: `this.owner' is in the base class sequence of `base'.
     */
     final def superSymbol(base: Symbol): Symbol = {
      var bcs = base.info.baseClasses.dropWhile(.!=(owner)).tail;
      var sym: Symbol = NoSymbol;
      while (!bcs.isEmpty && sym == NoSymbol) {
	sym = overriddenSymbol(bcs.head).suchThat(sym => !(sym hasFlag DEFERRED));;
	bcs = bcs.tail
      }
      sym
    }

// ToString -------------------------------------------------------------------

    /** A tag which (in the ideal case) uniquely identifies class symbols */
    final def tag: int = name.toString().hashCode();

    /** The simple name of this Symbol (this is always a term name) */
    final def simpleName: Name =
      if (isConstructor && !settings.debug.value) owner.name.toTermName else name;

    /** String representation of symbol's definition key word */
    final def keyString: String =
      if (isTrait)
        if (hasFlag(JAVA)) "interface" else "trait"
      else if (isClass) "class"
      else if (isType && !hasFlag(PARAM)) "type"
      else if (isVariable) "var"
      else if (isPackage) "package"
      else if (isModule) "object"
      else if (isMethod) "def"
      else if (isTerm && (!hasFlag(PARAM) || hasFlag(PARAMACCESSOR))) "val"
      else "";

    /** String representation of symbol's kind */
    final def kindString: String =
      if (isPackageClass)
        if (settings.debug.value) "package class" else "package"
      else if (isAnonymousClass) "<template>"
      else if (isRefinementClass) ""
      else if (isModuleClass) "singleton class"
      else if (isTrait) "trait"
      else if (isClass) "class"
      else if (isType) "type"
      else if (isVariable) "variable"
      else if (isPackage) "package"
      else if (isModule) "object"
      else if (isConstructor) "constructor"
      else if (isMethod) "method"
      else if (isTerm) "value"
      else "";

    /** String representation of symbol's simple name.
     *  If !settings.debug translates expansions of operators back to operator symbol.
     *  E.g. $eq => =.
     *  If settings.uniquId adds id.
     */
    final def nameString: String =
      simpleName.decode + idString;

    /** String representation of symbol's full name with `separator'
     *  between class names.
     *  Never translates expansions of operators back to operator symbol.
     *  Never adds id.
     */
    final def fullNameString(separator: char): String =
      if (owner.isRoot || owner.isEmptyPackageClass) simpleName.toString()
      else owner.fullNameString(separator) + separator + simpleName;

    final def fullNameString: String = fullNameString('.');

    /** If settings.uniqid is set, the symbol's id, else "" */
    final def idString: String =
      if (settings.uniqid.value) "#" + id else "";

    /** String representation, including symbol's kind
     *  e.g., "class Foo", "method Bar".
     */
    override def toString(): String =
      compose(List(kindString, nameString));

    /** String representation of location. */
    final def locationString: String =
      if (owner.isClass &&
          (!owner.isAnonymousClass && !owner.isRefinementClass || settings.debug.value))
	" in " + (if (owner.isModuleClass) "object " + owner.nameString  else owner)
      else "";

    /** String representation of symbol's definition following its name */
    final def infoString(tp: Type): String = {
      def typeParamsString: String = tp match {
	case PolyType(tparams, _) if (tparams.length != 0) =>
	  (tparams map (.defString)).mkString("[", ",", "]")
	case _ =>
	  ""
      }
      if (isClass)
	typeParamsString + " extends " + tp.resultType
      else if (isAliasType)
	typeParamsString + " = " + tp.resultType
      else if (isAbstractType)
	tp match {
	  case TypeBounds(lo, hi) =>
	    (if (lo.symbol == AllClass) "" else " >: " + lo) +
	    (if (hi.symbol == AnyClass) "" else " <: " + hi)
	  case _ =>
	    "<: " + tp;
	}
      else if (isModule)
	moduleClass.infoString(tp)
      else
	tp match {
	  case PolyType(tparams, res) =>
	    typeParamsString + infoString(res)
	  case MethodType(pts, res) =>
	    pts.mkString("(", ",", ")") + infoString(res)
	  case _ =>
	    ": " + tp
	}
    }

    /** String representation of symbol's variance */
    private def varianceString: String =
      if (variance == 1) "+"
      else if (variance == -1) "-"
      else "";

    /** String representation of symbol's definition */
    final def defString: String =
      compose(List(flagsToString(flags & ExplicitFlags),
		   keyString,
		   varianceString + nameString,
		   infoString(rawInfo)));

    /** Concatenate strings separated by spaces */
    private def compose(ss: List[String]): String =
      ss.filter("" !=).mkString("", " ", "");
  }

  /** A class for term symbols */
  class TermSymbol(initOwner: Symbol, initPos: int, initName: Name) extends Symbol(initOwner, initPos, initName) {
    override def isTerm = true;
    def cloneSymbolImpl(owner: Symbol): Symbol =
      new TermSymbol(owner, pos, name);
  }

  /** A class for module symbols */
  class ModuleSymbol(initOwner: Symbol, initPos: int, initName: Name, mclazz: ClassSymbol) extends TermSymbol(initOwner, initPos, initName) {
    setFlag(MODULE | FINAL);
    private val clazz: ClassSymbol =
      if (mclazz != null) mclazz else new ModuleClassSymbol(this);
    override def moduleClass: ClassSymbol = clazz;
    override def cloneSymbolImpl(owner: Symbol): Symbol =
      new ModuleSymbol(owner, pos, name, clazz);
  }

  /** A class of type symbols. Alias and abstract types are direct instances
   *  of this class. Classes are instances of a subclass.
   */
  class TypeSymbol(initOwner: Symbol, initPos: int, initName: Name) extends Symbol(initOwner, initPos, initName) {
    override def isType = true;
    private var tyconCache: Type = null;
    private var tpeCache: Type = _;
    private var valid: Phase = null;
    override def tpe: Type = {
      assert(tpeCache != NoType, this);
      if (valid != phase) {
        if (hasFlag(INITIALIZED)) valid = phase;
        tpeCache = NoType;
        tpeCache = typeRef(if (isTypeParameter) NoPrefix else owner.thisType,
                           this, unsafeTypeParams map (.tpe))
      }
      assert(tpeCache != null/*, "" + this + " " + phase*/);
      tpeCache
    }
    override def typeConstructor: Type = {
      if (tyconCache == null)
	tyconCache = typeRef(if (isTypeParameter) NoPrefix else owner.thisType,
			     this, List());
      tyconCache;
    }
    override def setInfo(tp: Type): this.type = {
      valid = null;
      tyconCache = null;
      super.setInfo(tp);
      this
    }
    override def reset(completer: Type): unit = {
      super.reset(completer);
      valid = null;
      tyconCache = null;
    }
    def cloneSymbolImpl(owner: Symbol): Symbol =
      new TypeSymbol(owner, pos, name);
  }

  /** A class for class symbols */
  class ClassSymbol(initOwner: Symbol, initPos: int, initName: Name)
   extends TypeSymbol(initOwner, initPos, initName) {
    var sourceFile: AbstractFile = null;
    private var thissym: Symbol = this;
    override def isClass: boolean = true;
    override def reset(completer: Type): unit = {
      super.reset(completer);
      thissym = this;
    }

    private var thisTypeCache: Type = null;

    /** the type this.type in this class */
    override def thisType: Type = {
      if (thisTypeCache == null) {
	thisTypeCache =
	  if (isModuleClass && !isRoot) {
	    assert(sourceModule != NoSymbol);
	    singleType(owner.thisType, sourceModule);
	  } else ThisType(this);
      }
      thisTypeCache
    }

    /** A symbol carrying the self type of the class as its type */
    override def thisSym: Symbol = thissym;

    /** Sets the self type of the class */
    override def typeOfThis_=(tp: Type): unit =
      thissym = newThisSym(pos).setInfo(tp);

    override def cloneSymbolImpl(owner: Symbol): Symbol = {
      assert(!isModuleClass);
      val clone = new ClassSymbol(owner, pos, name);
      if (thisSym != this) clone.typeOfThis = typeOfThis;
      clone
    }

    override def sourceModule = if (isModuleClass) owner.linkedModule else NoSymbol;
  }

  /** A class for module class symbols
   *  Note: Not all module classes are of this type; when unpickled, we get plain class symbols!
   */
  class ModuleClassSymbol(module: ModuleSymbol) extends ClassSymbol(module.owner, module.pos, module.name.toTypeName) {
    setFlag(module.getFlag(ModuleToClassFlags) | MODULE | FINAL);
    override def sourceModule = module;
  }

  /** An object repreesenting a missing symbol */
  object NoSymbol extends Symbol(null, Position.NOPOS, nme.NOSYMBOL) {
    super.setInfo(NoType);
    override def setInfo(info: Type): this.type = { assert(info == NoType); this }
    override def enclClass: Symbol = this;
    override def toplevelClass: Symbol = this;
    override def enclMethod: Symbol = this;
    override def owner: Symbol = throw new Error();
    override def alternatives: List[Symbol] = List();
    override def reset(completer: Type): unit = {}
    def cloneSymbolImpl(owner: Symbol): Symbol = throw new Error();
  }

  def cloneSymbols(syms: List[Symbol]): List[Symbol] = {
    val syms1 = syms map (.cloneSymbol);
    for (val sym1 <- syms1) sym1.setInfo(sym1.info.substSym(syms, syms1));
    syms1
  }

  def cloneSymbols(syms: List[Symbol], owner: Symbol): List[Symbol] = {
    val syms1 = syms map (.cloneSymbol(owner));
    for (val sym1 <- syms1) sym1.setInfo(sym1.info.substSym(syms, syms1));
    syms1
  }

  /** An exception for cyclic references of symbol definitions */
  case class CyclicReference(sym: Symbol, info: Type) extends TypeError("illegal cyclic reference involving " + sym);

  /** A class for type histories */
  private class TypeHistory(val start: Phase, val info: Type, val prev: TypeHistory);
}

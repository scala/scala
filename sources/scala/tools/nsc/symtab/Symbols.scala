/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.symtab;

import scala.tools.util.{AbstractFile, Position}
import Flags._;

[_trait_] abstract class Symbols: SymbolTable {
  import definitions._;

  private var ids = 0;

  //for statistics:
  def symbolCount = ids;
  var typeSymbolCount = 0;
  var classSymbolCount = 0;

  type AttrInfo = Pair[Type, List[Constant]];

  val emptySymbolArray = new Array[Symbol](0);

  /** The class for all symbols */
  abstract class Symbol(initOwner: Symbol, initPos: int, initName: Name) {

    var rawowner = initOwner;
    var rawname = initName;
    private var rawflags: long = 0;
    private var rawpos = initPos;
    val id = { ids = ids + 1; ids }

    var validForRun: CompilerRun = NoRun;

    def pos = rawpos;
    def setPos(pos: int): this.type = { this.rawpos = pos; this }

    var attributes: List[AttrInfo] = List();

// Creators -------------------------------------------------------------------

    final def newValue(pos: int, name: Name) =
      new TermSymbol(this, pos, name);
    final def newVariable(pos: int, name: Name) =
      newValue(pos, name).setFlag(MUTABLE);
    final def newValueParameter(pos: int, name: Name) =
      newValue(pos, name).setFlag(PARAM);
    final def newLocalDummy(pos: int) =
      newValue(pos, nme.LOCAL(this)).setInfo(NoType);
    final def newMethod(pos: int, name: Name) =
      newValue(pos, name).setFlag(METHOD);
    final def newLabel(pos: int, name: Name) =
      newMethod(pos, name).setFlag(LABEL);
    final def newConstructor(pos: int) =
      newMethod(pos, nme.CONSTRUCTOR);
    final def newModule(pos: int, name: Name, clazz: ClassSymbol) =
      new ModuleSymbol(this, pos, name).setFlag(MODULE | FINAL).setModuleClass(clazz);
    final def newModule(pos: int, name: Name) = {
      val m = new ModuleSymbol(this, pos, name).setFlag(MODULE | FINAL);
      m.setModuleClass(new ModuleClassSymbol(m))
    }
    final def newPackage(pos: int, name: Name) = {
      assert(isPackageClass);
      val m = newModule(pos, name).setFlag(JAVA | PACKAGE);
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
    final def newModuleClass(pos: int, name: Name) =
      new ModuleClassSymbol(this, pos, name);
    final def newAnonymousClass(pos: int) =
      newClass(pos, nme.ANON_CLASS_NAME.toTypeName);
    final def newAnonymousFunctionClass(pos: int) =
      newClass(pos, nme.ANON_FUN_NAME.toTypeName);
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
    final def isVariable = isTerm && hasFlag(MUTABLE) && !isMethod;
    final def isSetter = isTerm && hasFlag(ACCESSOR) && nme.isSetterName(name);
       //todo: make independent of name, as this can be forged.
    final def hasGetter = isTerm && nme.isLocalName(name);
    final def isValueParameter = isTerm && hasFlag(PARAM);
    final def isLocalDummy = isTerm && nme.isLocalDummyName(name);
    final def isMethod = isTerm && hasFlag(METHOD);
    final def isSourceMethod = isTerm && (flags & (METHOD | STABLE)) == METHOD;
    final def isLabel = isTerm && hasFlag(LABEL);
    final def isClassConstructor = isTerm && (name == nme.CONSTRUCTOR);
    final def isMixinConstructor = isTerm && (name == nme.MIXIN_CONSTRUCTOR);
    final def isConstructor = isTerm && (name == nme.CONSTRUCTOR) || (name == nme.MIXIN_CONSTRUCTOR);
    final def isModule = isTerm && hasFlag(MODULE);
    final def isStaticModule = isModule && isStatic && !isMethod;
    final def isPackage = isModule && hasFlag(PACKAGE);
    final def isThisSym = isTerm && name == nme.this_;
    final def isError = hasFlag(IS_ERROR);
    final def isTrait = isClass & hasFlag(TRAIT);
    final def isAliasType = isType && !isClass && !hasFlag(DEFERRED);
    final def isAbstractType = isType && !isClass && hasFlag(DEFERRED);
    final def isTypeParameter = isType && hasFlag(PARAM);
    final def isAnonymousClass = isClass && (originalName startsWith nme.ANON_CLASS_NAME);
      // startsWith necessary because name may grow when lifted and also because of anonymous function classes
    final def isRefinementClass = isClass && name == nme.REFINE_CLASS_NAME.toTypeName; // no lifting for refinement classes
    final def isModuleClass = isClass && hasFlag(MODULE);
    final def isPackageClass = isClass && hasFlag(PACKAGE);
    final def isRoot = isPackageClass && name == nme.ROOT.toTypeName;
    final def isRootPackage = isPackage && name == nme.ROOT;
    final def isEmptyPackage = isPackage && name == nme.EMPTY_PACKAGE_NAME;
    final def isEmptyPackageClass = isPackageClass && name == nme.EMPTY_PACKAGE_NAME.toTypeName;

    /** Does this symbol denote a stable value? */
    final def isStable =
      isTerm && !hasFlag(MUTABLE) && (!hasFlag(METHOD) || hasFlag(STABLE));

    /** Does this symbol denote the primary constructor
     * of its enclosing class or trait? */
    final def isPrimaryConstructor =
      isConstructor && owner.primaryConstructor == this;

    /** Is this symbol an implementation class for a trait ? */
    final def isImplClass: boolean = isClass && nme.isImplClassName(name);

    final def needsImplClass: boolean =
      isTrait && (!hasFlag(INTERFACE) || hasFlag(lateINTERFACE)) && !isImplClass;

    /** Is this symbol a module variable ? */
    final def isModuleVar: boolean = isVariable && nme.isModuleVarName(name);

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
      isClass && (hasFlag(SEALED) || isUnboxedClass(this));

    /** Is this symbol locally defined? I.e. not accessed from outside `this' instance */
    final def isLocal: boolean = owner.isTerm;

    /** Is this symbol a constant? */
    final def isConstant: boolean =
      isStable && (tpe match {
  case ConstantType(_) => true
  case PolyType(_, ConstantType(_)) => true
  case MethodType(_, ConstantType(_)) => true
  case _ => false
      });

    /** Is this class nested in another class or module (not a package)? */
    final def isNestedClass: boolean =
      isClass && !isRoot && !owner.isPackageClass;

    /** Is this class locally defined?
     *  A class is local, if
     *   - it is anonymous, or
     *   - its owner is a value
     *   - it is defined within a local class214
     */
    final def isLocalClass: boolean =
      isClass && (isAnonymousClass || isRefinementClass || isLocal ||
                  !owner.isPackageClass && owner.isLocalClass);

    /** Symbol was preloaded from package */
    final def isExternal: boolean = rawpos == Position.NOPOS;

    /** A a member of class `base' is incomplete if (1) it is declared deferred or
     *  (2) it is abstract override and its super symbol in `base' is nonexistent or inclomplete.
     */
    final def isIncompleteIn(base: Symbol): boolean =
      (this hasFlag DEFERRED) ||
      (this hasFlag ABSOVERRIDE) && {
        val supersym = superSymbol(base);
        supersym == NoSymbol || supersym.isIncompleteIn(base)
      }

    final def isInitialized: boolean =
      validForRun == currentRun;

    /** The variance of this symbol as an integer */
    final def variance: int =
      if (hasFlag(COVARIANT)) 1
      else if (hasFlag(CONTRAVARIANT)) -1
      else 0;

// Flags, owner, and name attributes --------------------------------------------------------------

    def owner: Symbol = rawowner;
    final def owner_=(owner: Symbol): unit = { rawowner = owner }

    def name: Name = rawname;
    final def name_=(name: Name): unit = { rawname = name }

    def originalName = nme.originalName(name);

    final def flags = {
      val fs = rawflags & phase.flagMask;
      (fs | ((fs & LateFlags) >>> LateShift)) & ~(fs >>> AntiShift)
    }
    final def flags_=(fs: long) = rawflags = fs;
    final def setFlag(mask: long): this.type = { rawflags = rawflags | mask; this }
    final def resetFlag(mask: long): this.type = { rawflags = rawflags & ~mask; this }
    final def getFlag(mask: long): long = flags & mask;
    final def hasFlag(mask: long): boolean = (flags & mask) != 0;
    final def resetFlags: unit = { rawflags = rawflags & TopLevelCreationFlags }

// Info and Type -------------------------------------------------------------------

    private var infos: TypeHistory = null;
    private var limit: Phase#Id = 0;

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
      while (validForRun != currentRun) {
  //if (settings.debug.value) System.out.println("completing " + this);//DEBUG
        var ifs = infos;
        assert(ifs != null, this.name);
        while (ifs.prev != null) {
          ifs = ifs.prev;
        }
  val tp = ifs.info;
  //if (settings.debug.value) System.out.println("completing " + this.rawname + tp.getClass());//debug
        if ((rawflags & LOCKED) != 0) {
          setInfo(ErrorType);
          throw CyclicReference(this, tp);
        }
        rawflags = rawflags | LOCKED;
  val current = phase;
        try {
    phase = phaseWithId(ifs.start);
          tp.complete(this);
    // if (settings.debug.value && (validForRun == currentRun) System.out.println("completed " + this/* + ":" + info*/);//DEBUG
          rawflags = rawflags & ~LOCKED
        } finally {
    phase = current
        }
  cnt = cnt + 1;
  // allow for two completions:
  //   one: sourceCompleter to LazyType, two: LazyType to completed type
  if (cnt == 3) throw new Error("no progress in completing " + this + ":" + tp);
      }
      rawInfo
    }

    /** Set initial info. */
    def setInfo(info: Type): this.type = {
      assert(info != null);
      var pid = phase.id;
      if (pid == 0) {
        // can happen when we initialize NoSymbol before running the compiler
        assert(name == nme.NOSYMBOL);
        pid = 1
      }
      infos = new TypeHistory(pid, info, null);
      limit = pid;
      if (info.isComplete) {
        rawflags = rawflags & ~LOCKED;
        validForRun = currentRun
      } else {
        rawflags = rawflags & ~LOCKED;
        validForRun = NoRun
      }
      this
    }

    /** Set new info valid from start of this phase. */
    final def updateInfo(info: Type): Symbol = {
      assert(infos.start <= phase.id);
      if (infos.start == phase.id) infos = infos.prev;
      infos = new TypeHistory(phase.id, info, infos);
      this
    }

    /** Return info without checking for initialization or completing */
    final def rawInfo: Type = {
      if (limit < phase.id) {
  if (validForRun == currentRun) {
    val current = phase;
          var itr = infoTransformers.nextFrom(limit);
          infoTransformers = itr; // caching optimization
          while (itr.pid != NoPhase.id && itr.pid < current.id) {
            phase = phaseWithId(itr.pid);
            val info1 = itr.transform(this, infos.info);
            limit = phase.id + 1;
      if (info1 ne infos.info) {
        infos = new TypeHistory(limit, info1, infos);
            }
            itr = itr.nextFrom(limit)
          }
          phase = current;
          limit = current.id;
  }
  assert(infos != null, name);
  infos.info
      } else {
  var infos = this.infos;
  while (phase.id < infos.start && infos.prev != null) infos = infos.prev;
  infos.info
      }
    }

    /** Initialize the symbol */
    final def initialize: this.type = {
      if (!isInitialized) info;
      this
    }

    /** Was symbol's type updated during given phase? */
    final def isUpdatedAt(pid: Phase#Id): boolean = {
      var infos = this.infos;
      while (infos != null && infos.start != pid + 1) infos = infos.prev;
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
      rawpos = Position.NOPOS;
      infos = null;
      limit = NoPhase.id;
      setInfo(completer)
    }

// Comparisons ----------------------------------------------------------------

    /** A total ordering between symbols that refines the class
     *  inheritance graph (i.e. subclass.isLess(superclass) always holds).
     *  the ordering is given by: (isType, -|closure| for type symbols, id)
     */
    final def isLess(that: Symbol): boolean = {
      def closureLength(sym: Symbol) =
  if (sym.isAbstractType) 1 + sym.info.bounds.hi.closure.length
  else sym.info.closure.length;
      if (this.isType)
  that.isType &&
  {val diff = closureLength(this) - closureLength(that);
   diff > 0 || diff == 0 && this.id < that.id}
      else
  that.isType || this.id < that.id;
    }

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
        //assert(info.isInstanceOf[OverloadedType], "" + this + ":" + info);//DEBUG
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
      assert(!(result hasFlag OVERLOADED), result.alternatives);
      result
    }

// Cloneing -------------------------------------------------------------------

    /** A clone of this symbol */
    final def cloneSymbol: Symbol =
      cloneSymbol(owner);

    /** A clone of this symbol, but with given owner */
    final def cloneSymbol(owner: Symbol): Symbol =
      cloneSymbolImpl(owner).setInfo(info.cloneInfo(owner)).setFlag(this.rawflags);

    /** Internal method to clone a symbol's implementation without flags or type
     */
    def cloneSymbolImpl(owner: Symbol): Symbol;

// Access to related symbols --------------------------------------------------

    /** The next enclosing class */
    def enclClass: Symbol = if (isClass) this else owner.enclClass;

    /** The next enclosing method */
    def enclMethod: Symbol = if (isSourceMethod) this else owner.enclMethod;

    /** The primary constructor of a class */
    def primaryConstructor: Symbol = {
      val c = info.decl(if (isTrait || isImplClass) nme.MIXIN_CONSTRUCTOR else nme.CONSTRUCTOR);
      if (c hasFlag OVERLOADED) c.alternatives.head else c
    }

    /** The self symbol of a class with explicit self type, or else the symbol itself.
     */
    def thisSym: Symbol = this;

    /** The type of `this' in a class, or else the type of the symbol itself. */
    final def typeOfThis = thisSym.tpe;

    /** Sets the type of `this' in a class */
    def typeOfThis_=(tp: Type): unit = throw new Error("typeOfThis cannot be set for " + this);

    /** If symbol is a class, the type this.type in this class, otherwise NoPrefix */
    def thisType: Type = NoPrefix;

    /** Return every accessor of a primary constructor parameter in this case class
      * todo: limit to accessors for first constructor parameter section.
      */
    final def caseFieldAccessors: List[Symbol] =
      info.decls.toList filter (sym => !(sym hasFlag PRIVATE) && sym.hasFlag(CASEACCESSOR));

    final def constrParamAccessors: List[Symbol] =
      info.decls.toList filter (sym => !sym.isMethod && sym.hasFlag(PARAMACCESSOR));

    /** The symbol accessed by this accessor function.
     */
    final def accessed: Symbol = {
      assert(hasFlag(ACCESSOR));
      owner.info.decl(nme.getterToLocal(if (isSetter) nme.setterToGetter(name) else name))
    }

    final def implClass: Symbol = owner.info.decl(nme.implClassName(name));

    /** For a paramaccessor: a superclass paramaccessor for which this symbol is
     *  an alias, NoSymbol for all others */
    def alias: Symbol = NoSymbol;

    /** The class with the same name in the same package as this module or
     *  case class factory
     */
    final def linkedClass: Symbol = {
      if (owner.isPackageClass)
  owner.info.decl(name.toTypeName).suchThat(sym => sym.rawInfo ne NoType)
      else NoSymbol;
    }

    /** The module or case class factory with the same name in the same
     *  package as this class.
     */
    final def linkedModule: Symbol =
      if (owner.isPackageClass)
  owner.info.decl(name.toTermName).suchThat(
          sym => (sym hasFlag MODULE) && (sym.rawInfo ne NoType));
      else NoSymbol;

    /** The top-level class containing this symbol */
    def toplevelClass: Symbol =
      if (isClass && owner.isPackageClass) this else owner.toplevelClass;

    /** For a module its linked class, for a class its linked module or case factory otherwise */
    final def linkedSym: Symbol =
      if (isTerm) linkedClass
      else if (isClass && owner.isPackageClass)
        owner.info.decl(name.toTermName).suchThat(sym => sym.rawInfo ne NoType)
      else NoSymbol;

    final def toInterface: Symbol =
      if (isImplClass) {
  val iface = tpe.parents.last.symbol;
  assert(nme.implClassName(iface.name) == name, this);
  iface
      } else this;

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
        !sym.isTerm || (tpe matches owner.thisType.memberType(sym)));

    /** The symbol overriding this symbol in given subclass */
    final def overridingSymbol(base: Symbol): Symbol =
      base.info.nonPrivateDecl(name).suchThat(sym =>
        !sym.isTerm || (base.thisType.memberType(sym) matches base.thisType.memberType(this)));

    /** The symbol accessed by a super in the definition of this symbol when seen from
     *  class `base'. This symbol is always concrete.
     *  pre: `this.owner' is in the base class sequence of `base'.
     */
    final def superSymbol(base: Symbol): Symbol = {
      var bcs = base.info.baseClasses.dropWhile(owner !=).tail;
      var sym: Symbol = NoSymbol;
      while (!bcs.isEmpty && sym == NoSymbol) {
        if (!bcs.head.isImplClass)
    sym = overriddenSymbol(bcs.head).suchThat(sym => !sym.hasFlag(DEFERRED));
  bcs = bcs.tail
      }
      sym
    }

    /** The getter of this value definition in class `base', or NoSymbol if none exists */
    final def getter(base: Symbol): Symbol =
      base.info.decl(nme.getterName(name)) filter (.hasFlag(ACCESSOR));

    /** The setter of this value definition, or NoSymbol if none exists */
    final def setter(base: Symbol): Symbol =
      base.info.decl(nme.getterToSetter(nme.getterName(name))) filter (.hasFlag(ACCESSOR));

    /** Remove private modifier from symbol `sym's definition. If `sym' is a
     *  term symbol rename it by expanding its name to avoid name clashes
     */
    final def makeNotPrivate(base: Symbol): unit =
      if (isTerm && (this hasFlag PRIVATE)) {
        setFlag(notPRIVATE);
        if (!hasFlag(DEFERRED)) setFlag(lateFINAL);
  expandName(base)
      }

    /** change name by appending $$<fully-qualified-name-of-class `base'>
     *  Do the same for any accessed symbols or setters/getters
     */
    def expandName(base: Symbol): unit =
      if (this != NoSymbol && !hasFlag(EXPANDEDNAME)) {
        setFlag(EXPANDEDNAME);
        if (hasFlag(ACCESSOR)) {
          accessed.expandName(base);
        } else if (hasGetter) {
          getter(owner).expandName(base);
          setter(owner).expandName(base);
        }
        name = base.expandedName(name)
      }

    def expandedName(name: Name): Name =
      newTermName(fullNameString('$') + nme.EXPAND_SEPARATOR_STRING + name);

/*
    def referenced: Symbol =
      throw new Error("referenced inapplicable for " + this);

    def setReferenced(sym: Symbol): Symbol =
      throw new Error("setReferenced inapplicable for " + this);
*/
// ToString -------------------------------------------------------------------

    /** A tag which (in the ideal case) uniquely identifies class symbols */
    final def tag: int = fullNameString.hashCode();

    /** The simple name of this Symbol (this is always a term name) */
    final def simpleName: Name = name;

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
      else if (isClassConstructor) "constructor"
      else if (isSourceMethod) "method"
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

    def infosString = infos.toString();

    /** String representation of symbol's variance */
    private def varianceString: String =
      if (variance == 1) "+"
      else if (variance == -1) "-"
      else "";

    /** String representation of symbol's definition */
    final def defString: String =
      compose(List(flagsToString(flags & ExplicitFlags),
       keyString,
       varianceString + nameString + infoString(rawInfo)));

    /** Concatenate strings separated by spaces */
    private def compose(ss: List[String]): String =
      ss.filter("" !=).mkString("", " ", "");
  }

  /** A class for term symbols */
  class TermSymbol(initOwner: Symbol, initPos: int, initName: Name) extends Symbol(initOwner, initPos, initName) {
    override def isTerm = true;

    protected var referenced: Symbol = NoSymbol;

    def cloneSymbolImpl(owner: Symbol): Symbol = {
      val clone = new TermSymbol(owner, pos, name);
      clone.referenced = referenced;
      clone
    }

    override def alias: Symbol =
      if (hasFlag(SUPERACCESSOR | PARAMACCESSOR | MIXEDIN)) initialize.referenced else NoSymbol;

    def setAlias(alias: Symbol): TermSymbol = {
      assert(alias != NoSymbol);
      assert(hasFlag(SUPERACCESSOR | PARAMACCESSOR | MIXEDIN));
      referenced = alias;
      this
    }

    override def moduleClass: Symbol =
      if (hasFlag(MODULE)) referenced else NoSymbol;

    def setModuleClass(clazz: Symbol): TermSymbol = {
      assert(hasFlag(MODULE));
      referenced = clazz;
      this
    }
  }

  /** A class for term symbols */
  class ModuleSymbol(initOwner: Symbol, initPos: int, initName: Name) extends TermSymbol(initOwner, initPos, initName) {

    private var flatname = nme.EMPTY;

    override def owner: Symbol =
      if (phase.flatClasses && !hasFlag(METHOD) &&
          rawowner != NoSymbol && !rawowner.isPackageClass) rawowner.owner
      else rawowner;

    override def name: Name =
      if (phase.flatClasses && !hasFlag(METHOD) &&
          rawowner != NoSymbol && !rawowner.isPackageClass) {
  if (flatname == nme.EMPTY) {
    assert(rawowner.isClass);
    flatname = newTermName(rawowner.name.toString() + "$" + rawname);
  }
  flatname
      } else rawname;

    override def cloneSymbolImpl(owner: Symbol): Symbol = {
      val clone = new ModuleSymbol(owner, pos, name);
      clone.referenced = referenced;
      clone
    }
  }

  /** A class of type symbols. Alias and abstract types are direct instances
   *  of this class. Classes are instances of a subclass.
   */
  class TypeSymbol(initOwner: Symbol, initPos: int, initName: Name) extends Symbol(initOwner, initPos, initName) {
    override def isType = true;
    private var tyconCache: Type = null;
    private var tyconRun: CompilerRun = null;
    private var tpeCache: Type = _;
    private var tpePhase: Phase = null;
    override def tpe: Type = {
      assert(tpeCache ne NoType, this);
      if (tpePhase != phase) {
        if (isValid(tpePhase)) {
    tpePhase = phase
        } else {
          if (isInitialized) tpePhase = phase;
          tpeCache = NoType;
    val targs = if (phase.erasedTypes && this != ArrayClass) List()
          else unsafeTypeParams map (.tpe);
          tpeCache = typeRef(if (isTypeParameter) NoPrefix else owner.thisType, this, targs)
        }
      }
      assert(tpeCache != null/*, "" + this + " " + phase*/);//debug
      tpeCache
    }
    override def typeConstructor: Type = {
      if (tyconCache == null || tyconRun != currentRun) {
  tyconCache = typeRef(if (isTypeParameter) NoPrefix else owner.thisType, this, List());
        tyconRun = currentRun;
      }
      assert(tyconCache != null);
      tyconCache
    }
    override def setInfo(tp: Type): this.type = {
      tpePhase = null;
      tyconCache = null;
      super.setInfo(tp);
      this
    }
    override def reset(completer: Type): unit = {
      super.reset(completer);
      tpePhase = null;
      tyconCache = null;
    }
    def cloneSymbolImpl(owner: Symbol): Symbol =
      new TypeSymbol(owner, pos, name);
    if (util.Statistics.enabled) typeSymbolCount = typeSymbolCount + 1;
  }

  /** A class for class symbols */
  class ClassSymbol(initOwner: Symbol, initPos: int, initName: Name) extends TypeSymbol(initOwner, initPos, initName) {
    var sourceFile: AbstractFile = null;
    private var thissym: Symbol = this;
    override def isClass: boolean = true;
    override def reset(completer: Type): unit = {
      super.reset(completer);
      thissym = this;
    }

    private var flatname = nme.EMPTY;

    override def owner: Symbol =
      if (phase.flatClasses && rawowner != NoSymbol && !rawowner.isPackageClass) rawowner.owner
      else rawowner;

    override def name: Name =
      if (phase.flatClasses && rawowner != NoSymbol && !rawowner.isPackageClass) {
  if (flatname == nme.EMPTY) {
    assert(rawowner.isClass);
    flatname = newTypeName(rawowner.name.toString() + "$" + rawname);
  }
  flatname
      } else rawname;

    private var thisTypeCache: Type = _;
    private var thisTypePhase: Phase = null;

    /** the type this.type in this class */
    override def thisType: Type = {
      val p = thisTypePhase;
      if (p != phase) {
        thisTypePhase = phase;
        if (!(isValid(p) /*||
        thisTypePhase != null && thisTypePhase.erasedTypes && phase.erasedTypes*/)) {
    thisTypeCache =
      if (isModuleClass && !isRoot && !phase.erasedTypes)
        singleType(owner.thisType, sourceModule);
      else ThisType(this);
        }
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

    override def sourceModule = if (isModuleClass) linkedModule else NoSymbol;

    if (util.Statistics.enabled) classSymbolCount = classSymbolCount + 1;
  }

  /** A class for module class symbols
   *  Note: Not all module classes are of this type; when unpickled, we get plain class symbols!
   */
  class ModuleClassSymbol(owner: Symbol, pos: int, name: Name) extends ClassSymbol(owner, pos, name) {
    private var module: Symbol = null;
    def this(module: TermSymbol) = {
      this(module.owner, module.pos, module.name.toTypeName);
      setFlag(module.getFlag(ModuleToClassFlags) | MODULE | FINAL);
      setSourceModule(module);
    }
    override def sourceModule = module;
    def setSourceModule(module: Symbol): unit = this.module = module
  }

  /** An object repreesenting a missing symbol */
  object NoSymbol extends Symbol(null, Position.NOPOS, nme.NOSYMBOL) {
    setInfo(NoType);
    override def setInfo(info: Type): this.type = { assert(info eq NoType); super.setInfo(info) }
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
  private case class TypeHistory(start: Phase#Id, info: Type, prev: TypeHistory) {
    assert(prev == null || start > prev.start, this);
    assert(start != 0);
    override def toString() = "TypeHistory(" + phaseWithId(start) + "," + info + "," + prev + ")";
  }
}

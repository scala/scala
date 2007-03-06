/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.symtab

import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.util.{Position, SourceFile}
import Flags._

trait Symbols requires SymbolTable {
  import definitions._

  private var ids = 0

  //for statistics:
  def symbolCount = ids
  var typeSymbolCount = 0
  var classSymbolCount = 0

  val emptySymbolArray = new Array[Symbol](0)
  val emptySymbolSet = Set.empty[Symbol]
  type PositionType;
  def NoPos : PositionType;
  def FirstPos : PositionType;
  implicit def coercePosToInt(pos : PositionType) : Int;
  def coerceIntToPos(pos : Int) : PositionType;
  object RequiresIntsAsPositions {
    implicit def coerceIntToPos0(pos: Int) =
      coerceIntToPos(pos)
  }

  /** The class for all symbols */
  abstract class Symbol(initOwner: Symbol, initPos: PositionType, initName: Name) {

    var rawowner = initOwner
    var rawname = initName
    var rawflags: long = 0
    private var rawpos = initPos
    val id = { ids = ids + 1; ids }

    var validTo: Period = NoPeriod

    def pos = rawpos
    def setPos(pos: PositionType): this.type = { this.rawpos = pos; this }

    def namePos(source: SourceFile) = {
      val pos: Int = this.pos
      val buf = source.content
      if (pos == Position.NOPOS) Position.NOPOS
      else if (isTypeParameter) pos - name.length
      else if (isVariable || isMethod || isClass || isModule) {
        var ret = pos
        if (buf(pos) == ',') ret = ret + 1
        else if (isClass)  ret = ret + ("class").length()
        else if (isModule) ret = ret + ("object").length()
        else ret = ret + ("var").length()
        while (buf(ret).isWhitespace) ret = ret + 1
        ret
      }
      else if (isValue) {
        if (pos < (buf.length + ("val ").length())) {
          if ((buf(pos + 0) == 'v') &&
              (buf(pos + 1) == 'a') &&
              (buf(pos + 2) == 'l') &&
              (buf(pos + 3) == ' ')) {
            var pos0 = pos + 4
            while (pos0 < buf.length && buf(pos0).isWhitespace)
              pos0 = pos0 + 1
            pos0

          } else pos
        } else pos
      }
      else -1
    }

    var attributes: List[AnnotationInfo[Constant]] = List()

    var privateWithin: Symbol = _

// Creators -------------------------------------------------------------------

    final def newValue(pos: PositionType, name: Name) =
      new TermSymbol(this, pos, name)
    final def newVariable(pos: PositionType, name: Name) =
      newValue(pos, name).setFlag(MUTABLE)
    final def newValueParameter(pos: PositionType, name: Name) =
      newValue(pos, name).setFlag(PARAM)
    final def newLocalDummy(pos: PositionType) =
      newValue(pos, nme.LOCAL(this)).setInfo(NoType)
    final def newMethod(pos: PositionType, name: Name) =
      newValue(pos, name).setFlag(METHOD)
    final def newLabel(pos: PositionType, name: Name) =
      newMethod(pos, name).setFlag(LABEL)
    final def newConstructor(pos: PositionType) =
      newMethod(pos, nme.CONSTRUCTOR)
    final def newModule(pos: PositionType, name: Name, clazz: ClassSymbol) =
      new ModuleSymbol(this, pos, name).setFlag(MODULE | FINAL)
        .setModuleClass(clazz)
    final def newModule(pos: PositionType, name: Name) = {
      val m = new ModuleSymbol(this, pos, name).setFlag(MODULE | FINAL)
      m.setModuleClass(new ModuleClassSymbol(m))
    }
    final def newPackage(pos: PositionType, name: Name) = {
      assert(name == nme.ROOT || isPackageClass)
      val m = newModule(pos, name).setFlag(JAVA | PACKAGE)
      m.moduleClass.setFlag(JAVA | PACKAGE)
      m
    }
    final def newThisSym(pos: PositionType) =
      newValue(pos, nme.this_).setFlag(SYNTHETIC)
    final def newThisSkolem: Symbol =
      new ThisSkolem(owner, pos, name, this)
        .setFlag(SYNTHETIC | FINAL)
    final def newImport(pos: PositionType) =
      newValue(pos, nme.IMPORT)
    final def newOverloaded(pre: Type, alternatives: List[Symbol]): Symbol =
      newValue(alternatives.head.pos, alternatives.head.name)
      .setFlag(OVERLOADED)
      .setInfo(OverloadedType(pre, alternatives))

    final def newOuterAccessor(pos: PositionType) = {
      val sym = newMethod(pos, nme.OUTER)
      sym setFlag (STABLE | SYNTHETIC)
      if (isTrait) sym setFlag DEFERRED
      sym.expandName(this)
      sym.referenced = this
      sym
    }

    final def newErrorValue(name: Name) =
      newValue(pos, name).setFlag(SYNTHETIC | IS_ERROR).setInfo(ErrorType)
    final def newAliasType(pos: PositionType, name: Name) =
      new TypeSymbol(this, pos, name)
    final def newAbstractType(pos: PositionType, name: Name) =
      new TypeSymbol(this, pos, name).setFlag(DEFERRED)
    final def newTypeParameter(pos: PositionType, name: Name) =
      newAbstractType(pos, name).setFlag(PARAM)
    final def newTypeSkolem: Symbol =
      new TypeSkolem(owner, pos, name, this)
        .setFlag(flags)
    final def newClass(pos: PositionType, name: Name) =
      new ClassSymbol(this, pos, name)
    final def newModuleClass(pos: PositionType, name: Name) =
      new ModuleClassSymbol(this, pos, name)
    final def newAnonymousClass(pos: PositionType) =
      newClass(pos, nme.ANON_CLASS_NAME.toTypeName)
    final def newAnonymousFunctionClass(pos: PositionType) = {
      val anonfun = newClass(pos, nme.ANON_FUN_NAME.toTypeName)
      anonfun.attributes =
        AnnotationInfo(definitions.SerializableAttr.tpe, List(), List()) :: anonfun.attributes
      anonfun
    }
    final def newRefinementClass(pos: PositionType) =
      newClass(pos, nme.REFINE_CLASS_NAME.toTypeName)
    final def newErrorClass(name: Name) = {
      val clazz = newClass(pos, name).setFlag(SYNTHETIC | IS_ERROR)
      clazz.setInfo(ClassInfoType(List(), new ErrorScope(this), clazz))
      clazz
    }
    final def newErrorSymbol(name: Name): Symbol =
      if (name.isTypeName) newErrorClass(name) else newErrorValue(name)

// Tests ----------------------------------------------------------------------

    def isTerm   = false         //to be overridden
    def isType   = false         //to be overridden
    def isClass  = false         //to be overridden

    final def isValue = isTerm && !(isModule && hasFlag(PACKAGE | JAVA))
    final def isVariable  = isTerm && hasFlag(MUTABLE) && !isMethod
    final def isCapturedVariable  = isVariable && hasFlag(CAPTURED)

    final def isGetter = isTerm && hasFlag(ACCESSOR) && !nme.isSetterName(name)
    final def isSetter = isTerm && hasFlag(ACCESSOR) && nme.isSetterName(name)
       //todo: make independent of name, as this can be forged.
    final def hasGetter = isTerm && nme.isLocalName(name)
    final def isValueParameter = isTerm && hasFlag(PARAM)
    final def isLocalDummy = isTerm && nme.isLocalDummyName(name)
    final def isMethod = isTerm && hasFlag(METHOD)
    final def isSourceMethod = isTerm && (flags & (METHOD | STABLE)) == METHOD
    final def isLabel = isTerm && hasFlag(LABEL)
    final def isClassConstructor = isTerm && (name == nme.CONSTRUCTOR)
    final def isMixinConstructor = isTerm && (name == nme.MIXIN_CONSTRUCTOR)
    final def isConstructor = isTerm && (name == nme.CONSTRUCTOR) || (name == nme.MIXIN_CONSTRUCTOR)
    final def isModule = isTerm && hasFlag(MODULE)
    final def isStaticModule = isModule && isStatic && !isMethod
    final def isPackage = isModule && hasFlag(PACKAGE)
    final def isThisSym = isTerm && owner.thisSym == this
    final def isThisSkolem = isTerm && deSkolemize != this
    final def isError = hasFlag(IS_ERROR)
    final def isErroneous = isError || isInitialized && tpe.isErroneous
    final def isTrait = isClass & hasFlag(TRAIT)
    final def isAliasType = isType && !isClass && !hasFlag(DEFERRED)
    final def isAbstractType = isType && !isClass && hasFlag(DEFERRED)
    final def isTypeParameterOrSkolem = isType && hasFlag(PARAM)
    final def isTypeParameter         = isTypeParameterOrSkolem && deSkolemize == this
    final def isClassLocalToConstructor = isClass && hasFlag(INCONSTRUCTOR)
    final def isAnonymousClass = isClass && (originalName startsWith nme.ANON_CLASS_NAME)
      // startsWith necessary because name may grow when lifted and also because of anonymous function classes
    final def isRefinementClass = isClass && name == nme.REFINE_CLASS_NAME.toTypeName; // no lifting for refinement classes
    final def isModuleClass = isClass && hasFlag(MODULE)
    final def isPackageClass = isClass && hasFlag(PACKAGE)
    final def isRoot = isPackageClass && name == nme.ROOT.toTypeName
    final def isRootPackage = isPackage && name == nme.ROOTPKG
    final def isEmptyPackage = isPackage && name == nme.EMPTY_PACKAGE_NAME
    final def isEmptyPackageClass = isPackageClass && name == nme.EMPTY_PACKAGE_NAME.toTypeName
    def isDeprecated =
      attributes exists (attr => attr.atp.symbol == DeprecatedAttr)

    /** Does this symbol denote a wrapper object of the interpreter or its class? */
    final def isInterpreterWrapper =
      (isModule || isModuleClass) &&
      owner.isEmptyPackageClass &&
      name.toString.startsWith(nme.INTERPRETER_LINE_PREFIX) &&
      name.toString.endsWith(nme.INTERPRETER_WRAPPER_SUFFIX)

    /** Does this symbol denote a stable value? */
    final def isStable =
      isTerm && !hasFlag(MUTABLE) && (!hasFlag(METHOD | BYNAMEPARAM) || hasFlag(STABLE))

    /** Is this symbol a public */
    final def isPublic: boolean =
      !hasFlag(PRIVATE | PROTECTED) && privateWithin == NoSymbol

    /** Is this symbol a private local */
    final def isPrivateLocal =
      hasFlag(PRIVATE) && hasFlag(LOCAL)

    /** Does this symbol denote the primary constructor of its enclosing class? */
    final def isPrimaryConstructor =
      isConstructor && owner.primaryConstructor == this

    /** Is this symbol a case class factory method? */
    final def isCaseFactory =
      isMethod && hasFlag(CASE)

    /** Is this symbol an implementation class for a mixin? */
    final def isImplClass: boolean = isClass && hasFlag(IMPLCLASS)

    /** Is this symbol a trait which needs an implementation class? */
    final def needsImplClass: boolean =
      isTrait && (!hasFlag(INTERFACE) || hasFlag(lateINTERFACE)) && !isImplClass

    /** Is this a symbol which exists only in the implementation class, not in its trait? */
    final def isImplOnly: boolean = (
      hasFlag(PRIVATE) ||
      (owner.isImplClass || owner.isTrait) &&
      (hasFlag(notPRIVATE | LIFTED) && !hasFlag(ACCESSOR | SUPERACCESSOR | MODULE) || isConstructor)
    )

    /** Is this symbol a module variable ? */
    final def isModuleVar: boolean = isVariable && hasFlag(MODULEVAR)

    /** Is this symbol static (i.e. with no outer instance)? */
    final def isStatic: boolean =
      hasFlag(STATIC) || isRoot || owner.isStaticOwner

    /** Does this symbol denote a class that defines static symbols? */
    final def isStaticOwner: boolean =
      isPackageClass || isModuleClass && isStatic

    /** Is this symbol final?*/
    final def isFinal: boolean = (
      hasFlag(FINAL) ||
      isTerm && (
        hasFlag(PRIVATE) || isLocal || owner.isClass && owner.hasFlag(FINAL | MODULE))
    )

    /** Is this symbol a sealed class?*/
    final def isSealed: boolean =
      isClass && (hasFlag(SEALED) || isUnboxedClass(this))

    /** Is this symbol locally defined? I.e. not accessed from outside `this' instance */
    final def isLocal: boolean = owner.isTerm

    /** Is this symbol a constant? */
    final def isConstant: boolean =
      isStable && (tpe match {
        case ConstantType(_) => true
        case PolyType(_, ConstantType(_)) => true
        case MethodType(_, ConstantType(_)) => true
        case _ => false
      })

    /** Is this class nested in another class or module (not a package)? */
    final def isNestedClass: boolean =
      isClass && !isRoot && !owner.isPackageClass

    /** Is this class locally defined?
     *  A class is local, if
     *   - it is anonymous, or
     *   - its owner is a value
     *   - it is defined within a local class214
     */
    final def isLocalClass: boolean =
      isClass && (isAnonymousClass || isRefinementClass || isLocal ||
                  !owner.isPackageClass && owner.isLocalClass)

    /** A a member of class `base' is incomplete if
     *  (1) it is declared deferred or
     *  (2) it is abstract override and its super symbol in `base' is
     *      nonexistent or inclomplete.
     *
     *  @param base ...
     *  @return     ...
     */
    final def isIncompleteIn(base: Symbol): boolean = (
      (this hasFlag DEFERRED) ||
      (this hasFlag ABSOVERRIDE) && {
        val supersym = superSymbol(base)
        supersym == NoSymbol || supersym.isIncompleteIn(base)
      }
    )

    final def exists: boolean =
      this != NoSymbol && (!owner.isPackageClass || { rawInfo.load(this); rawInfo != NoType })

    final def isInitialized: boolean =
      validTo != NoPeriod

    final def isCovariant: boolean = isType && hasFlag(COVARIANT)

    final def isContravariant: boolean = isType && hasFlag(CONTRAVARIANT)

    /** The variance of this symbol as an integer */
    final def variance: int =
      if (isCovariant) 1
      else if (isContravariant) -1
      else 0

// Flags, owner, and name attributes --------------------------------------------------------------

    def owner: Symbol = rawowner
    final def owner_=(owner: Symbol): unit = { rawowner = owner }

    def ownerChain: List[Symbol] = this :: owner.ownerChain

    def name: Name = rawname

    final def name_=(name: Name): unit = { rawname = name }

    /** If this symbol has an expanded name, its original name, otherwise its name itself.
     *  @see expandName
     */
    def originalName = nme.originalName(name)

    final def flags = {
      val fs = rawflags & phase.flagMask
      (fs | ((fs & LateFlags) >>> LateShift)) & ~(fs >>> AntiShift)
    }
    final def flags_=(fs: long) = rawflags = fs
    final def setFlag(mask: long): this.type = { rawflags = rawflags | mask; this }
    final def resetFlag(mask: long): this.type = { rawflags = rawflags & ~mask; this }
    final def getFlag(mask: long): long = flags & mask
    final def hasFlag(mask: long): boolean = (flags & mask) != 0
    final def resetFlags: unit = { rawflags = rawflags & TopLevelCreationFlags }

    /** The class up to which this symbol is accessible,
     *  or NoSymbol if it is public or not a class member
     */
    final def accessBoundary(base: Symbol): Symbol = {
      if (hasFlag(PRIVATE)) owner
      else if (privateWithin != NoSymbol && !phase.erasedTypes) privateWithin
      else if (hasFlag(PROTECTED)) base
      else NoSymbol
    }

// Info and Type -------------------------------------------------------------------

    private[Symbols] var infos: TypeHistory = null

    /** Get type. The type of a symbol is:
     *  for a type symbol, the type corresponding to the symbol itself
     *  for a term symbol, its usual type
     */
    def tpe: Type = info

    /** Get type info associated with symbol at current phase, after
     *  ensuring that symbol is initialized (i.e. type is completed).
     */
    def info: Type = {
      var cnt = 0
      while (validTo == NoPeriod) {
        //if (settings.debug.value) System.out.println("completing " + this);//DEBUG
        assert(infos ne null, this.name)
        assert(infos.prev eq null, this.name)
        val tp = infos.info
        //if (settings.debug.value) System.out.println("completing " + this.rawname + tp.getClass());//debug
        if ((rawflags & LOCKED) != 0) {
          setInfo(ErrorType)
          throw CyclicReference(this, tp)
        }
        rawflags = rawflags | LOCKED
        val current = phase
        try {
          phase = phaseOf(infos.validFrom)
          tp.complete(this)
          // if (settings.debug.value && runId(validTo) == currentRunId) System.out.println("completed " + this/* + ":" + info*/);//DEBUG
          rawflags = rawflags & ~LOCKED
        } finally {
          phase = current
        }
        cnt = cnt + 1
        // allow for two completions:
        //   one: sourceCompleter to LazyType, two: LazyType to completed type
        if (cnt == 3) throw new Error("no progress in completing " + this + ":" + tp)
      }
      val result = rawInfo
      result
    }

    /** Set initial info. */
    def setInfo(info: Type): this.type = {
      assert(info ne null)
      infos = TypeHistory(currentPeriod, info, null)
      if (info.isComplete) {
        rawflags = rawflags & ~LOCKED
        validTo = currentPeriod
      } else {
        rawflags = rawflags & ~LOCKED
        validTo = NoPeriod
      }
      this
    }

    /** Set new info valid from start of this phase. */
    final def updateInfo(info: Type): Symbol = {
      assert(phaseId(infos.validFrom) <= phase.id)
      if (phaseId(infos.validFrom) == phase.id) infos = infos.prev
      infos = TypeHistory(currentPeriod, info, infos)
      this
    }

    /** Return info without checking for initialization or completing */
    def rawInfo: Type = {
      var infos = this.infos
      assert(infos != null, name)
      val curPeriod = currentPeriod
      val curPid = phaseId(curPeriod)

      if (validTo != NoPeriod) {

        // skip any infos that concern later phases
        while (curPid < phaseId(infos.validFrom) && infos.prev != null)
          infos = infos.prev

        if (validTo < curPeriod) {
          // adapt any infos that come from previous runs
          val current = phase
          try {
            infos = adaptInfos(infos)

            //assert(runId(validTo) == currentRunId, name)
            //assert(runId(infos.validFrom) == currentRunId, name)

            if (validTo < curPeriod) {
              var itr = infoTransformers.nextFrom(phaseId(validTo))
              infoTransformers = itr; // caching optimization
              while (itr.pid != NoPhase.id && itr.pid < current.id) {
                phase = phaseWithId(itr.pid)
                val info1 = itr.transform(this, infos.info)
                if (info1 ne infos.info) {
                  infos = TypeHistory(currentPeriod + 1, info1, infos)
                  this.infos = infos
                }
                validTo = currentPeriod + 1 // to enable reads from same symbol during info-transform
                itr = itr.next
              }
              validTo = if (itr.pid == NoPhase.id) curPeriod
                        else period(currentRunId, itr.pid)
            }
          } finally {
            phase = current
          }
        }
      }
      infos.info
    }

    private def adaptInfos(infos: TypeHistory): TypeHistory =
      if (infos == null || runId(infos.validFrom) == currentRunId) {
        infos
      } else {
        val prev1 = adaptInfos(infos.prev)
        if (prev1 ne infos.prev) prev1
        else {
          def adaptToNewRun(info: Type): Type =
            if (isPackageClass) info else adaptToNewRunMap(info)
          val pid = phaseId(infos.validFrom)
          validTo = period(currentRunId, pid)
          phase = phaseWithId(pid)
          val info1 = adaptToNewRun(infos.info)
          if (info1 eq infos.info) {
            infos.validFrom = validTo
            infos
          } else {
            this.infos = TypeHistory(validTo, info1, prev1)
            this.infos
          }
        }
      }

    /** Initialize the symbol */
    final def initialize: this.type = {
      if (!isInitialized) info
      this
    }

    /** Was symbol's type updated during given phase? */
    final def isUpdatedAt(pid: Phase#Id): boolean = {
      var infos = this.infos
      while ((infos ne null) && phaseId(infos.validFrom) != pid + 1) infos = infos.prev
      infos ne null
    }

    /** The type constructor of a symbol is:
     *  For a type symbol, the type corresponding to the symbol itself,
     *  excluding parameters.
     *  Not applicable for term symbols.
     */
    def typeConstructor: Type =
      throw new Error("typeConstructor inapplicable for " + this)

    /** The type parameters of this symbol */
    def unsafeTypeParams: List[Symbol] = rawInfo.typeParams
    /*
      val limit = phaseId(validTo)
      (if (limit < phase.id) infos.info else rawInfo).typeParams
    */

    def typeParams: List[Symbol] = {
      rawInfo.load(this); rawInfo.typeParams
    }

    def getAttributes(clazz: Symbol): List[AnnotationInfo[Constant]] =
      attributes.filter(.atp.symbol.isNonBottomSubClass(clazz))

    /** Reset symbol to initial state
     */
    def reset(completer: Type): unit = {
      resetFlags
      infos = null
      validTo = NoPeriod
      //limit = NoPhase.id
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
        else sym.info.closure.length
      if (this.isType)
        (that.isType &&
         { val diff = closureLength(this) - closureLength(that)
           diff > 0 || diff == 0 && this.id < that.id })
      else
        that.isType || this.id < that.id
    }

    /** A partial ordering between symbols.
     *  (this isNestedIn that) holds iff this symbol is defined within
     *  a class or method defining that symbol
     */
    final def isNestedIn(that: Symbol): boolean =
      owner == that || owner != NoSymbol && (owner isNestedIn that)

    /** Is this class symbol a subclass of that symbol? */
    final def isNonBottomSubClass(that: Symbol): boolean =
      this == that || this.isError || that.isError ||
      info.closurePos(that) >= 0

    final def isSubClass(that: Symbol): boolean = {
      isNonBottomSubClass(that) ||
      this == AllClass ||
      this == AllRefClass &&
      (that == AnyClass ||
       that != AllClass && (that isSubClass AnyRefClass))
    }

// Overloaded Alternatives ---------------------------------------------------------

    def alternatives: List[Symbol] =
      if (hasFlag(OVERLOADED)) info.asInstanceOf[OverloadedType].alternatives
      else List(this)

    def filter(cond: Symbol => boolean): Symbol =
      if (hasFlag(OVERLOADED)) {
        //assert(info.isInstanceOf[OverloadedType], "" + this + ":" + info);//DEBUG
        val alts = alternatives
        val alts1 = alts filter cond
        if (alts1 eq alts) this
        else if (alts1.isEmpty) NoSymbol
        else if (alts1.tail.isEmpty) alts1.head
        else owner.newOverloaded(info.prefix, alts1)
      } else if (cond(this)) this
      else NoSymbol

    def suchThat(cond: Symbol => boolean): Symbol = {
      val result = filter(cond)
      assert(!(result hasFlag OVERLOADED), result.alternatives)
      result
    }

// Cloneing -------------------------------------------------------------------

    /** A clone of this symbol */
    final def cloneSymbol: Symbol =
      cloneSymbol(owner)

    /** A clone of this symbol, but with given owner */
    final def cloneSymbol(owner: Symbol): Symbol =
      cloneSymbolImpl(owner).setInfo(info.cloneInfo(owner)).setFlag(this.rawflags)

    /** Internal method to clone a symbol's implementation without flags or type
     */
    def cloneSymbolImpl(owner: Symbol): Symbol

// Access to related symbols --------------------------------------------------

    /** The next enclosing class */
    def enclClass: Symbol = if (isClass) this else owner.enclClass

    /** The next enclosing method */
    def enclMethod: Symbol = if (isSourceMethod) this else owner.enclMethod

    /** The primary constructor of a class */
    def primaryConstructor: Symbol = {
      val c = info.decl(
        if (isTrait || isImplClass) nme.MIXIN_CONSTRUCTOR
        else nme.CONSTRUCTOR)
      if (c hasFlag OVERLOADED) c.alternatives.head else c
    }

    /** The self symbol of a class with explicit self type, or else the
     *  symbol itself.
     */
    def thisSym: Symbol = this

    /** The type of `this' in a class, or else the type of the symbol itself. */
    def typeOfThis = thisSym.tpe

    /** Sets the type of `this' in a class */
    def typeOfThis_=(tp: Type): unit =
      throw new Error("typeOfThis cannot be set for " + this)

    /** If symbol is a class, the type <code>this.type</code> in this class,
     * otherwise <code>NoPrefix</code>.
     */
    def thisType: Type = NoPrefix

    /** Return every accessor of a primary constructor parameter in this case class
      * todo: limit to accessors for first constructor parameter section.
      */
    final def caseFieldAccessors: List[Symbol] =
      info.decls.toList filter (sym => !(sym hasFlag PRIVATE) && sym.hasFlag(CASEACCESSOR))

    final def constrParamAccessors: List[Symbol] =
      info.decls.toList filter (sym => !sym.isMethod && sym.hasFlag(PARAMACCESSOR))

    /** The symbol accessed by this accessor function.
     */
    final def accessed: Symbol = {
      assert(hasFlag(ACCESSOR))
      owner.info.decl(nme.getterToLocal(if (isSetter) nme.setterToGetter(name) else name))
    }

    /** The implementation class of a trait */
    final def implClass: Symbol = owner.info.decl(nme.implClassName(name))

    /** The class that is logically an outer class of given `clazz'.
     *  This is the enclosing class, except for classes defined locally to constructors,
     *  where it is the outer class of the enclosing class
     */
    final def outerClass: Symbol =
      if (owner.isClass) owner
      else if (isClassLocalToConstructor) owner.owner.outerClass
      else owner.outerClass

    /** For a paramaccessor: a superclass paramaccessor for which this symbol
     *  is an alias, NoSymbol for all others */
    def alias: Symbol = NoSymbol

    /** For an outer accessor: The class from which the outer originates.
     *  For all other symbols: NoSymbol
     */
    def outerSource: Symbol = NoSymbol

    /** The superclass of this class */
    def superClass: Symbol = if (info.parents.isEmpty) NoSymbol else info.parents.head.symbol

    /** The directly or indirectly inherited mixins of this class
     *  except for mixin classes inherited by the superclass. Mixin classes appear
     *  in linearlization order.
     */
    def mixinClasses: List[Symbol] = {
      val sc = superClass
      info.baseClasses.tail.takeWhile(sc ne)
    }

    /** The top-level class containing this symbol */
    def toplevelClass: Symbol =
      if (isClass && owner.isPackageClass) this else owner.toplevelClass

    /** The class with the same name in the same package as this module or
     *  case class factory
     */
    final def linkedClassOfModule: Symbol = {
      if (this != NoSymbol)
        owner.info.decl(name.toTypeName).suchThat(sym => sym.rawInfo ne NoType)
      else NoSymbol
    }

    /** The module or case class factory with the same name in the same
     *  package as this class.
     */
    final def linkedModuleOfClass: Symbol =
      if (this.isClass && !this.isAnonymousClass && !this.isRefinementClass) {
        owner.rawInfo.decl(name.toTermName).suchThat(
          sym => (sym hasFlag MODULE) && (sym.rawInfo ne NoType))
      } else NoSymbol

    /** For a module its linked class, for a class its linked module or case
     *  factory otherwise.
     */
    final def linkedSym: Symbol =
      if (isTerm) linkedClassOfModule
      else if (isClass) owner.info.decl(name.toTermName).suchThat(sym => sym.rawInfo ne NoType)
      else NoSymbol

    /** For a module class its linked class, for a plain class
     *  the module class of itys linked module.
     */
    final def linkedClassOfClass: Symbol =
      if (isModuleClass) linkedClassOfModule else linkedModuleOfClass.moduleClass

    /** If this symbol is an implementation class, its interface, otherwise the symbol itself
     *  The method follows two strategies to determine the interface.
     *   - during or after erasure, it takes the last parent of the implementatation class
     *     (which is always the interface, by convention)
     *   - before erasure, it looks up the interface name in the scope of the owner of the class.
     *     This only works for implementation classes owned by other classes or traits.
     */
    final def toInterface: Symbol =
      if (isImplClass) {
        val result =
          if (phase.next.erasedTypes) {
            assert(!tpe.parents.isEmpty, this)
            tpe.parents.last.symbol
          } else {
            owner.info.decl(nme.interfaceName(name))
          }
        assert(result != NoSymbol, this)
        result
      } else this

    /** The module corresponding to this module class (note that this
     *  is not updated when a module is cloned).
     */
    def sourceModule: Symbol = NoSymbol

    /** The module class corresponding to this module.
     */
    def moduleClass: Symbol = NoSymbol

    /** The non-abstract, symbol whose type matches the type of this symbol
     *  in in given class.
     *
     *  @param ofclazz   The class containing the symbol's definition
     *  @param site      The base type from which member types are computed
     */
    final def matchingSymbol(ofclazz: Symbol, site: Type): Symbol =
      ofclazz.info.nonPrivateDecl(name).filter(sym =>
        !sym.isTerm || (site.memberType(this) matches site.memberType(sym)))

    /** The symbol overridden by this symbol in given class `ofclazz' */
    final def overriddenSymbol(ofclazz: Symbol): Symbol =
      matchingSymbol(ofclazz, owner.thisType)

    /** The symbol overriding this symbol in given subclass `ofclazz' */
    final def overridingSymbol(ofclazz: Symbol): Symbol =
      matchingSymbol(ofclazz, ofclazz.thisType)

    final def allOverriddenSymbols: List[Symbol] =
      if (owner.isClass)
        for { val bc <- owner.info.baseClasses.tail
              val s = overriddenSymbol(bc)
              s != NoSymbol } yield s
      else List()

    /** The symbol accessed by a super in the definition of this symbol when
     *  seen from class `base'. This symbol is always concrete.
     *  pre: `this.owner' is in the base class sequence of `base'.
     */
    final def superSymbol(base: Symbol): Symbol = {
      var bcs = base.info.baseClasses.dropWhile(owner !=).tail
      var sym: Symbol = NoSymbol
      while (!bcs.isEmpty && sym == NoSymbol) {
        if (!bcs.head.isImplClass)
          sym = matchingSymbol(bcs.head, base.thisType).suchThat(
            sym => !sym.hasFlag(DEFERRED))
        bcs = bcs.tail
      }
      sym
    }

    /** The getter of this value or setter definition in class `base', or NoSymbol if
     *  none exists.
     */
    final def getter(base: Symbol): Symbol = {
      val getterName = if (isSetter) nme.setterToGetter(name) else nme.getterName(name)
      base.info.decl(getterName) filter (.hasFlag(ACCESSOR))
    }

    /** The setter of this value or getter definition, or NoSymbol if none exists */
    final def setter(base: Symbol): Symbol =
      base.info.decl(nme.getterToSetter(nme.getterName(name))) filter (.hasFlag(ACCESSOR))

    /** The case factory corresponding to this case class
     *  @pre case class is a member of some other class or package
     */
    final def caseFactory: Symbol =
      initialize.owner.info.decl(name.toTermName).suchThat(.isCaseFactory)

    /** If this symbol is a skolem, its corresponding type parameter, otherwise this */
    def deSkolemize: Symbol = this

    /** Remove private modifier from symbol `sym's definition. If `sym' is a
     *  term symbol rename it by expanding its name to avoid name clashes
     */
    final def makeNotPrivate(base: Symbol): unit =
      if (this hasFlag PRIVATE) {
        setFlag(notPRIVATE)
        if (!hasFlag(DEFERRED) && isTerm) setFlag(lateFINAL)
        if (!isStaticModule) {
          expandName(base)
          if (isModule) moduleClass.makeNotPrivate(base)
        }
      }

    /** change name by appending $$<fully-qualified-name-of-class `base'>
     *  Do the same for any accessed symbols or setters/getters
     */
    def expandName(base: Symbol): unit =
      if (this.isTerm && this != NoSymbol && !hasFlag(EXPANDEDNAME)) {
        setFlag(EXPANDEDNAME)
        if (hasFlag(ACCESSOR) && !hasFlag(DEFERRED)) {
          accessed.expandName(base)
        } else if (hasGetter) {
          getter(owner).expandName(base)
          setter(owner).expandName(base)
        }
        name = base.expandedName(name)
        if (isType) name = name.toTypeName
      }

    /** The expanded name of `name' relative to this class as base
     */
    def expandedName(name: Name): Name = {

      newTermName(fullNameString('$') + nme.EXPAND_SEPARATOR_STRING + name)
    }

    def sourceFile: AbstractFile =
      (if (isModule) moduleClass else toplevelClass).sourceFile

    def sourceFile_=(f: AbstractFile): unit =
      throw new Error("sourceFile_= inapplicable for " + this)

    def isFromClassFile: Boolean =
      (if (isModule) moduleClass else toplevelClass).isFromClassFile

    /** If this is a sealed class, its known direct subclasses. Otherwise Set.empty */
    def children: Set[Symbol] = emptySymbolSet

    /** Declare given subclass `sym' of this sealed class */
    def addChild(sym: Symbol) {
      throw new Error("addChild inapplicable for " + this)
    }


// ToString -------------------------------------------------------------------

    /** A tag which (in the ideal case) uniquely identifies class symbols */
    final def tag: int = fullNameString.hashCode()

    /** The simple name of this Symbol */
    final def simpleName: Name = name

    /** String representation of symbol's definition key word */
    final def keyString: String =
      if (isTrait && hasFlag(JAVA)) "interface"
      else if (isTrait) "trait"
      else if (isClass) "class"
      else if (isType && !hasFlag(PARAM)) "type"
      else if (isVariable) "var"
      else if (isPackage) "package"
      else if (isModule) "object"
      else if (isMethod) "def"
      else if (isTerm && (!hasFlag(PARAM) || hasFlag(PARAMACCESSOR))) "val"
      else ""

    /** String representation of symbol's kind */
    final def kindString: String =
      if (isPackageClass)
        if (settings.debug.value) "package class" else "package"
      else if (isModuleClass)
        if (settings.debug.value) "singleton class" else "object"
      else if (isAnonymousClass) "template"
      else if (isRefinementClass) ""
      else if (isTrait) "trait"
      else if (isClass) "class"
      else if (isType) "type"
      else if (isVariable) "variable"
      else if (isPackage) "package"
      else if (isModule) "object"
      else if (isClassConstructor) "constructor"
      else if (isSourceMethod) "method"
      else if (isTerm) "value"
      else ""

    /** String representation of symbol's simple name.
     *  If !settings.debug translates expansions of operators back to operator symbol.
     *  E.g. $eq => =.
     *  If settings.uniquId adds id.
     */
    def nameString: String = {
      var s = simpleName.decode.toString
      if (s endsWith nme.LOCAL_SUFFIX)
        s = s.substring(0, s.length - nme.LOCAL_SUFFIX.length)
      s + idString
    }

    /** String representation of symbol's full name with <code>separator</code>
     *  between class names.
     *  Never translates expansions of operators back to operator symbol.
     *  Never adds id.
     */
    final def fullNameString(separator: char): String = {
      assert(owner != NoSymbol, this)
      var str =
        if (owner.isRoot ||
            owner.isEmptyPackageClass ||
            owner.isInterpreterWrapper) simpleName.toString
        else owner.enclClass.fullNameString(separator) + separator + simpleName
      if (str.charAt(str.length - 1) == ' ') str = str.substring(0, str.length - 1)
      str
    }

    final def fullNameString: String = fullNameString('.')

    /** If settings.uniqid is set, the symbol's id, else "" */
    final def idString: String =
      if (settings.uniqid.value) "#" + id else ""

    /** String representation, including symbol's kind
     *  e.g., "class Foo", "method Bar".
     */
    override def toString(): String =
      compose(List(kindString, if (isClassConstructor) owner.nameString else nameString))

    /** String representation of location. */
    final def locationString: String =
      if (owner.isClass &&
          ((!owner.isAnonymousClass &&
            !owner.isRefinementClass &&
            !owner.isInterpreterWrapper &&
            !owner.isRoot &&
            !owner.isEmptyPackageClass) || settings.debug.value))
        " in " + owner else ""

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
            "<: " + tp
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

    def infosString = infos.toString()

    /** String representation of symbol's variance */
    private def varianceString: String =
      if (variance == 1) "+"
      else if (variance == -1) "-"
      else ""

    /** String representation of symbol's definition */
    def defString: String = {
      val f = if (settings.debug.value) flags
              else if (owner.isRefinementClass) flags & ExplicitFlags & ~OVERRIDE
              else flags & ExplicitFlags
      compose(List(flagsToString(f), keyString, varianceString + nameString + infoString(rawInfo)))
    }

    /** Concatenate strings separated by spaces */
    private def compose(ss: List[String]): String =
      ss.filter("" !=).mkString("", " ", "")
  }

  /** A class for term symbols */
  class TermSymbol(initOwner: Symbol, initPos: PositionType, initName: Name)
  extends Symbol(initOwner, initPos, initName) {
    override def isTerm = true

    privateWithin = NoSymbol

    protected var referenced: Symbol = NoSymbol

    def cloneSymbolImpl(owner: Symbol): Symbol = {
      val clone = new TermSymbol(owner, pos, name)
      clone.referenced = referenced
      clone
    }

    override def alias: Symbol =
      if (hasFlag(SUPERACCESSOR | PARAMACCESSOR | MIXEDIN)) initialize.referenced
      else NoSymbol

    def setAlias(alias: Symbol): TermSymbol = {
      assert(alias != NoSymbol, this)
      assert(!(alias hasFlag OVERLOADED), alias)

      assert(hasFlag(SUPERACCESSOR | PARAMACCESSOR | MIXEDIN), this)
      referenced = alias
      this
    }

    override def outerSource: Symbol =
      if (name endsWith nme.OUTER) initialize.referenced
      else NoSymbol

    override def moduleClass: Symbol =
      if (hasFlag(MODULE)) referenced else NoSymbol

    def setModuleClass(clazz: Symbol): TermSymbol = {
      assert(hasFlag(MODULE))
      referenced = clazz
      this
    }
  }

  /** A class for module symbols */
  class ModuleSymbol(initOwner: Symbol, initPos: PositionType, initName: Name)
  extends TermSymbol(initOwner, initPos, initName) {

    private var flatname = nme.EMPTY

    override def owner: Symbol =
      if (phase.flatClasses && !hasFlag(METHOD) &&
          rawowner != NoSymbol && !rawowner.isPackageClass) rawowner.owner
      else rawowner

    override def name: Name =
      if (phase.flatClasses && !hasFlag(METHOD) &&
          rawowner != NoSymbol && !rawowner.isPackageClass) {
        if (flatname == nme.EMPTY) {
          assert(rawowner.isClass)
          flatname = newTermName(rawowner.name.toString() + "$" + rawname)
        }
        flatname
      } else rawname

    override def cloneSymbolImpl(owner: Symbol): Symbol = {
      val clone = new ModuleSymbol(owner, pos, name)
      clone.referenced = referenced
      clone
    }
  }

  /** A class for type parameters viewed from inside their scopes */
  class ThisSkolem(initOwner: Symbol, initPos: PositionType,
                   initName: Name, clazz: Symbol)
  extends TermSymbol(initOwner, initPos, initName) {
    override def deSkolemize = clazz
    override def cloneSymbolImpl(owner: Symbol): Symbol = {
      throw new Error("should not clone a this skolem")
    }
    override def nameString: String = clazz.name.toString() + ".this"
  }

  /** A class of type symbols. Alias and abstract types are direct instances
   *  of this class. Classes are instances of a subclass.
   */
  class TypeSymbol(initOwner: Symbol, initPos: PositionType, initName: Name)
  extends Symbol(initOwner, initPos, initName) {
    override def isType = true
    privateWithin = NoSymbol
    private var tyconCache: Type = null
    private var tyconRunId = NoRunId
    private var tpeCache: Type = _
    private var tpePeriod = NoPeriod
    override def tpe: Type = {
      if (tpeCache eq NoType) throw CyclicReference(this, typeConstructor)
      if (tpePeriod != currentPeriod) {
        if (isValid(tpePeriod)) {
          tpePeriod = currentPeriod
        } else {
          if (isInitialized) tpePeriod = currentPeriod
          tpeCache = NoType
          val targs = if (phase.erasedTypes && this != ArrayClass) List()
          else unsafeTypeParams map (.tpe)
          tpeCache = typeRef(if (isTypeParameterOrSkolem) NoPrefix else owner.thisType, this, targs)
        }
      }
      assert(tpeCache ne null/*, "" + this + " " + phase*/)//debug
      tpeCache
    }

    override def typeConstructor: Type = {
      if ((tyconCache eq null) || tyconRunId != currentRunId) {
        tyconCache = typeRef(if (isTypeParameter) NoPrefix else owner.thisType, this, List())
        tyconRunId = currentRunId
      }
      assert(tyconCache ne null)
      tyconCache
    }

    override def setInfo(tp: Type): this.type = {
      tpePeriod = NoPeriod
      tyconCache = null
      tp match { //debug
        case TypeRef(_, sym, _) =>
          assert(sym != this, this)
        case ClassInfoType(parents, _, _) =>
          for(val p <- parents) assert(p.symbol != this, owner)
        case _ =>
      }
      super.setInfo(tp)
      this
    }

    override def reset(completer: Type): unit = {
      super.reset(completer)
      tpePeriod = NoPeriod
      tyconRunId = NoRunId
    }

    def cloneSymbolImpl(owner: Symbol): Symbol =
      new TypeSymbol(owner, pos, name)

    if (util.Statistics.enabled) typeSymbolCount = typeSymbolCount + 1
  }

  /** A class for type parameters viewed from inside their scopes */
  class TypeSkolem(initOwner: Symbol, initPos: PositionType,
                   initName: Name, typeParam: Symbol)
  extends TypeSymbol(initOwner, initPos, initName) {
    override def deSkolemize = typeParam
    override def cloneSymbolImpl(owner: Symbol): Symbol = {
      throw new Error("should not clone a type skolem")
    }
    override def nameString: String =
      if (settings.debug.value) (super.nameString + "&")
      else super.nameString
  }

  /** A class for class symbols */
  class ClassSymbol(initOwner: Symbol, initPos: PositionType, initName: Name)
  extends TypeSymbol(initOwner, initPos, initName) {

    /** The classfile from which this class was loaded. Maybe null. */
    var classFile: AbstractFile = null;

    private var source: AbstractFile = null
    override def sourceFile =
      if (owner.isPackageClass) source else super.sourceFile
    override def sourceFile_=(f: AbstractFile): unit = {
      //System.err.println("set source file of " + this + ": " + f);
      source = f
    }
    override def isFromClassFile = {
      if (classFile ne null) true
      else if (owner.isPackageClass) false
      else super.isFromClassFile
    }
    private var thissym: Symbol = this
    override def isClass: boolean = true
    override def reset(completer: Type): unit = {
      super.reset(completer)
      thissym = this
    }

    private var flatname = nme.EMPTY

    override def owner: Symbol =
      if (phase.flatClasses && rawowner != NoSymbol && !rawowner.isPackageClass) rawowner.owner
      else rawowner

    override def name: Name =
      if (phase.flatClasses && rawowner != NoSymbol && !rawowner.isPackageClass) {
        if (flatname == nme.EMPTY) {
          assert(rawowner.isClass)
          flatname = newTypeName(rawowner.name.toString() + "$" + rawname)
        }
        flatname
      } else rawname

    private var thisTypeCache: Type = _
    private var thisTypePeriod = NoPeriod

    /** the type this.type in this class */
    override def thisType: Type = {
      val period = thisTypePeriod
      if (period != currentPeriod) {
        thisTypePeriod = currentPeriod
        if (!isValid(period)) thisTypeCache = mkThisType(this)
      }
      thisTypeCache
    }

    /** A symbol carrying the self type of the class as its type */
    override def thisSym: Symbol = thissym

    override def typeOfThis: Type =
      if (getFlag(MODULE | IMPLCLASS) == MODULE && owner != NoSymbol)
        singleType(owner.thisType, sourceModule)
      else thissym.tpe

    /** Sets the self type of the class */
    override def typeOfThis_=(tp: Type): unit =
      thissym = newThisSym(pos).setInfo(tp)

    override def cloneSymbolImpl(owner: Symbol): Symbol = {
      assert(!isModuleClass)
      val clone = new ClassSymbol(owner, pos, name)
      if (thisSym != this) {
        clone.typeOfThis = typeOfThis
        clone.thisSym.name = thisSym.name
      }
      clone
    }

    override def sourceModule =
      if (isModuleClass) linkedModuleOfClass else NoSymbol

    private var childSet: Set[Symbol] = emptySymbolSet
    override def children: Set[Symbol] = childSet
    override def addChild(sym: Symbol) { childSet = childSet + sym }

    if (util.Statistics.enabled) classSymbolCount = classSymbolCount + 1
  }

  /** A class for module class symbols
   *  Note: Not all module classes are of this type; when unpickled, we get
   *  plain class symbols!
   */
  class ModuleClassSymbol(owner: Symbol, pos: PositionType, name: Name)
  extends ClassSymbol(owner, pos, name) {
    private var module: Symbol = null
    def this(module: TermSymbol) = {
      this(module.owner, module.pos, module.name.toTypeName)
      setFlag(module.getFlag(ModuleToClassFlags) | MODULE | FINAL)
      setSourceModule(module)
    }
    override def sourceModule = module
    def setSourceModule(module: Symbol): unit = this.module = module
  }

  /** An object repreesenting a missing symbol */
  object NoSymbol extends Symbol(null, NoPos, nme.NOSYMBOL) {
    setInfo(NoType)
    privateWithin = this
    override def setInfo(info: Type): this.type = {
      infos = TypeHistory(1, NoType, null)
      rawflags = rawflags & ~ LOCKED
      validTo = currentPeriod
      this
    }
    override def defString: String = toString
    override def enclClass: Symbol = this
    override def toplevelClass: Symbol = this
    override def enclMethod: Symbol = this
    override def owner: Symbol = throw new Error("no-symbol does not have owner")
    override def sourceFile: AbstractFile = null
    override def ownerChain: List[Symbol] = List()
    override def alternatives: List[Symbol] = List()
    override def reset(completer: Type): unit = {}
    override def info: Type = NoType
    override def rawInfo: Type = NoType
    def cloneSymbolImpl(owner: Symbol): Symbol = throw new Error()
  }

  case class AnnotationInfo[+T](atp: Type, args: List[T], assocs: List[(Name, T)]) {
    override def toString: String =
      atp +
      (if (args.isEmpty) ""
       else args.mkString("(", ", ", ")")) +
      (if (assocs.isEmpty) ""
       else (assocs map { case (x, y) => x+" = "+y } mkString ("{", ", ", "}")))
  }

  def cloneSymbols(syms: List[Symbol]): List[Symbol] = {
    val syms1 = syms map (.cloneSymbol)
    for (val sym1 <- syms1) sym1.setInfo(sym1.info.substSym(syms, syms1))
    syms1
  }

  def cloneSymbols(syms: List[Symbol], owner: Symbol): List[Symbol] = {
    val syms1 = syms map (.cloneSymbol(owner))
    for (val sym1 <- syms1) sym1.setInfo(sym1.info.substSym(syms, syms1))
    syms1
  }

  /** An exception for cyclic references of symbol definitions */
  case class CyclicReference(sym: Symbol, info: Type)
  extends TypeError("illegal cyclic reference involving " + sym)

  /** A class for type histories */
  private sealed case class TypeHistory(var validFrom: Period, info: Type, prev: TypeHistory) {
    assert((prev eq null) || phaseId(validFrom) > phaseId(prev.validFrom), this)
    assert(validFrom != NoPeriod)
    override def toString() =
      "TypeHistory(" + phaseOf(validFrom)+":"+runId(validFrom) + "," + info + "," + prev + ")"
  }
}

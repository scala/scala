/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$


package scala.tools.nsc
package symtab

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Map
import io.AbstractFile
import util.{Position, NoPosition, BatchSourceFile}
import util.Statistics._
import Flags._

//todo: get rid of MONOMORPHIC flag

trait Symbols extends reflect.generic.Symbols { self: SymbolTable =>
  import definitions._

  private var ids = 0
  def symbolCount = ids // statistics

  val emptySymbolArray = new Array[Symbol](0)

  /** Used for deciding in the IDE whether we can interrupt the compiler */
  protected var activeLocks = 0

  /** Used to keep track of the recursion depth on locked symbols */
  private var recursionTable = Map.empty[Symbol, Int]

  private var nextexid = 0
  private def freshExistentialName() = {
    nextexid += 1
    "_"+nextexid
  }

/*
  type Position;
  def NoPos : Position;
  def FirstPos : Position;
  implicit def coercePosToInt(pos : Position) : Int;
  def coerceIntToPos(pos : Int) : Position;
  object RequiresIntsAsPositions {
    implicit def coerceIntToPos0(pos: Int) =
      coerceIntToPos(pos)
  }
  */
  /** The class for all symbols */
  abstract class Symbol(initOwner: Symbol, initPos: Position, initName: Name) extends AbsSymbol {

    var rawowner = initOwner
    var rawname = initName
    var rawflags: Long = 0
    private var rawpos = initPos

    val id = { ids += 1; ids } // identity displayed when -uniqid

//    assert(id != 7498, initName+"/"+initOwner)

    var validTo: Period = NoPeriod

    def pos = rawpos
    def setPos(pos: Position): this.type = { this.rawpos = pos; this }

    def namePos(source: BatchSourceFile) = {
      val pos: Int = this.pos.pointOrElse(-1)
      val buf = source.content
      if (pos == -1) -1
      else if (isTypeParameter) pos - name.length
      else if (isVariable || isMethod || isClass || isModule) {
        var ret = pos
        if (buf(pos) == ',') ret += 1
        else if (isClass)  ret += "class".length()
        else if (isModule) ret += "object".length()
        else ret += "var".length()
        while (buf(ret).isWhitespace) ret += 1
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
              pos0 += 1
            pos0

          } else pos
        } else pos
      }
      else -1
    }

// annotations

    private var rawannots: List[AnnotationInfoBase] = Nil

    /* Used in namer to check whether annotations were already assigned or not */
    def rawAnnotations:List[AnnotationInfoBase] = rawannots

    /** After the typer phase (before, look at the definition's Modifiers), contains
     *  the annotations attached to member a definition (class, method, type, field).
     */
    def annotations: List[AnnotationInfo] = {
      // .initialize: the type completer of the symbol parses the annotations,
      // see "def typeSig" in Namers
      val annots1 = initialize.rawannots map {
        case LazyAnnotationInfo(annot) => annot()
        case a @ AnnotationInfo(_, _, _) => a
      } filter { a => !a.atp.isError }
      rawannots = annots1
      annots1
    }

    def setAnnotations(annots: List[AnnotationInfoBase]): this.type = {
      this.rawannots = annots
      this
    }

    override def addAnnotation(annot: AnnotationInfo) {
      setAnnotations(annot :: this.rawannots)
    }

    /** Does this symbol have an annotation of the given class? */
    def hasAnnotation(cls: Symbol) =
      getAnnotation(cls).isDefined

    def getAnnotation(cls: Symbol): Option[AnnotationInfo] =
      annotations find (_.atp.typeSymbol == cls)

    /** Finds the requested annotation and returns Some(Tree) containing
     *  the argument at position 'index', or None if either the annotation
     *  or the index does not exist.
     */
    private def getAnnotationArg(cls: Symbol, index: Int) =
      for (AnnotationInfo(_, args, _) <- getAnnotation(cls) ; if args.size > index) yield
        args(index)

    /** Remove all annotations matching the given class. */
    def removeAnnotation(cls: Symbol): Unit =
      setAnnotations(annotations filterNot (_.atp.typeSymbol == cls))

    /** set when symbol has a modifier of the form private[X], NoSymbol otherwise.
     *  Here's some explanation how privateWithin gets combined with access flags:
     *
     * PRIVATE    means class private, as in Java.
     * PROTECTED  means protected as in Java, except that access within
     *            the same package is not automatically allowed.
     * LOCAL      should only be set with PRIVATE or PROTECTED.
     *            It means that access is restricted to be from the same object.
     *
     * Besides these, there's the privateWithin field in Symbols which gives a visibility barrier,
     * where privateWithin == NoSymbol means no barrier. privateWithin is incompatible with
     * PRIVATE and LOCAL. If it is combined with PROTECTED, the two are additive. I.e.
     * the symbol is then accessible from within the privateWithin region as well
     * as from all subclasses. Here's a tanslation of Java's accessibility modifiers:
     * Java private:   PRIVATE flag set, privateWithin == NoSymbol
     * Java package:   no flag set, privateWithin == enclosing package
     * Java protected:  PROTECTED flag set, privateWithin == enclosing package
     * Java public:   no flag set, privateWithin == NoSymbol
     */
    private[this] var _privateWithin: Symbol = _
    def privateWithin = _privateWithin
    override def privateWithin_=(sym: Symbol) { _privateWithin = sym }

// Creators -------------------------------------------------------------------

    final def newValue(pos: Position, name: Name) =
      new TermSymbol(this, pos, name)
    final def newValue(name: Name, pos: Position = NoPosition) =
      new TermSymbol(this, pos, name)
    final def newVariable(pos: Position, name: Name) =
      newValue(pos, name).setFlag(MUTABLE)
    final def newValueParameter(pos: Position, name: Name) =
      newValue(pos, name).setFlag(PARAM)
    /** Create local dummy for template (owner of local blocks) */
    final def newLocalDummy(pos: Position) =
      newValue(pos, nme.LOCAL(this)).setInfo(NoType)
    final def newMethod(pos: Position, name: Name) =
      new MethodSymbol(this, pos, name).setFlag(METHOD)
    final def newMethod(name: Name, pos: Position = NoPosition) =
      new MethodSymbol(this, pos, name).setFlag(METHOD)
    final def newLabel(pos: Position, name: Name) =
      newMethod(pos, name).setFlag(LABEL)
    final def newConstructor(pos: Position) =
      newMethod(pos, nme.CONSTRUCTOR)
    final def newModule(pos: Position, name: Name, clazz: ClassSymbol) =
      new ModuleSymbol(this, pos, name).setFlag(MODULE | FINAL)
        .setModuleClass(clazz)
    final def newModule(name: Name, clazz: Symbol, pos: Position = NoPosition) =
      new ModuleSymbol(this, pos, name).setFlag(MODULE | FINAL)
        .setModuleClass(clazz.asInstanceOf[ClassSymbol])
    final def newModule(pos: Position, name: Name) = {
      val m = new ModuleSymbol(this, pos, name).setFlag(MODULE | FINAL)
      m.setModuleClass(new ModuleClassSymbol(m))
    }
    final def newPackage(pos: Position, name: Name) = {
      assert(name == nme.ROOT || isPackageClass)
      val m = newModule(pos, name).setFlag(JAVA | PACKAGE)
      m.moduleClass.setFlag(JAVA | PACKAGE)
      m
    }
    final def newThisSym(pos: Position) =
      newValue(pos, nme.this_).setFlag(SYNTHETIC)
    final def newImport(pos: Position) =
      newValue(pos, nme.IMPORT)

    /** @param pre   type relative to which alternatives are seen.
     *  for instance:
     *  class C[T] {
     *    def m(x: T): T
     *    def m'(): T
     *  }
     *  val v: C[Int]
     *
     *  Then v.m  has symbol TermSymbol(flags = {OVERLOADED},
     *                                  tpe = OverloadedType(C[Int], List(m, m')))
     *  You recover the type of m doing a
     *
     *    m.tpe.asSeenFrom(pre, C)   (generally, owner of m, which is C here).
     *
     *  or:
     *
     *    pre.memberType(m)
     */
    final def newOverloaded(pre: Type, alternatives: List[Symbol]): Symbol =
      newValue(alternatives.head.pos, alternatives.head.name)
      .setFlag(OVERLOADED)
      .setInfo(OverloadedType(pre, alternatives))

    /** for explicit outer phase */
    final def newOuterAccessor(pos: Position) = {
      val sym = newMethod(pos, nme.OUTER)
      sym setFlag (STABLE | SYNTHETIC)
      if (isTrait) sym setFlag DEFERRED
      sym.expandName(this)
      sym.referenced = this
      sym
    }

    final def newErrorValue(name: Name) =
      newValue(pos, name).setFlag(SYNTHETIC | IS_ERROR).setInfo(ErrorType)

    /** Symbol of a type definition  type T = ...
     */
    final def newAliasType(pos: Position, name: Name) =
      new TypeSymbol(this, pos, name)
    final def newAliasType(name: Name, pos: Position = NoPosition) =
      new TypeSymbol(this, pos, name)

    /** Symbol of an abstract type  type T >: ... <: ...
     */
    final def newAbstractType(pos: Position, name: Name) =
      new TypeSymbol(this, pos, name).setFlag(DEFERRED)
    final def newAbstractType(name: Name, pos: Position = NoPosition) =
      new TypeSymbol(this, pos, name).setFlag(DEFERRED)

    /** Symbol of a type parameter
     */
    final def newTypeParameter(pos: Position, name: Name) =
      newAbstractType(pos, name).setFlag(PARAM)

    /** Synthetic value parameters when parameter symbols are not available
     */
    final def newSyntheticValueParamss(argtypess: List[List[Type]]): List[List[Symbol]] = {
      var cnt = 0
      def freshName() = { cnt += 1; newTermName("x$" + cnt) }
      def param(tp: Type) =
        newValueParameter(owner.pos.focus, freshName()).setFlag(SYNTHETIC).setInfo(tp)
      argtypess map (_.map(param))
    }

    /** Make an existential variable.
     *  @param name    suffix to be appended to the freshly generated name
     *                 It's ususally "", except for type variables abstracting
     *                 over values, where it is ".type".
     *  @param owner   The owner of the variable
     *  @param bounds  The variable's bounds
     */
    final def newExistential(pos: Position, name: Name): Symbol =
      newAbstractType(pos, name.toTypeName).setFlag(EXISTENTIAL)

    final def freshExistential(suffix: String): Symbol =
      newExistential(pos, freshExistentialName()+suffix)

    /** Synthetic value parameters when parameter symbols are not available.
     *  Calling this method multiple times will re-use the same parameter names.
     */
    final def newSyntheticValueParams(argtypes: List[Type]): List[Symbol] =
      newSyntheticValueParamss(List(argtypes)).head

    /** Synthetic value parameter when parameter symbol is not available.
     *  Calling this method multiple times will re-use the same parameter name.
     */
    final def newSyntheticValueParam(argtype: Type): Symbol =
      newSyntheticValueParams(List(argtype)).head

    /** Type skolems are type parameters ``seen from the inside''
     *  Assuming a polymorphic method m[T], its type is a PolyType which has a TypeParameter
     *  with name `T' in its typeParams list. While type checking the parameters, result type and
     *  body of the method, there's a local copy of `T' which is a TypeSkolem.
     */
    final def newTypeSkolem: Symbol =
      new TypeSkolem(owner, pos, name, this)
        .setFlag(flags)

    final def newClass(pos: Position, name: Name) =
      new ClassSymbol(this, pos, name)
    final def newClass(name: Name, pos: Position = NoPosition) =
      new ClassSymbol(this, pos, name)

    final def newModuleClass(pos: Position, name: Name) =
      new ModuleClassSymbol(this, pos, name)
    final def newModuleClass(name: Name, pos: Position = NoPosition) =
      new ModuleClassSymbol(this, pos, name)

    final def newAnonymousClass(pos: Position) =
      newClass(pos, nme.ANON_CLASS_NAME.toTypeName)

    final def newAnonymousFunctionClass(pos: Position) =
      newClass(pos, nme.ANON_FUN_NAME.toTypeName)

    /** Refinement types P { val x: String; type T <: Number }
     *  also have symbols, they are refinementClasses
     */
    final def newRefinementClass(pos: Position) =
      newClass(pos, nme.REFINE_CLASS_NAME.toTypeName)

    final def newErrorClass(name: Name) = {
      val clazz = newClass(pos, name).setFlag(SYNTHETIC | IS_ERROR)
      clazz.setInfo(ClassInfoType(List(), new ErrorScope(this), clazz))
      clazz
    }

    final def newErrorSymbol(name: Name): Symbol =
      if (name.isTypeName) newErrorClass(name) else newErrorValue(name)

// Locking and unlocking ------------------------------------------------------

    // True if the symbol is unlocked.
    // True if the symbol is locked but still below the allowed recursion depth.
    // False otherwise
    def lockOK: Boolean = {
      ((rawflags & LOCKED) == 0L) ||
      ((settings.Yrecursion.value != 0) &&
       (recursionTable get this match {
         case Some(n) => (n <= settings.Yrecursion.value)
         case None => true }))
    }

    // Lock a symbol, using the handler if the recursion depth becomes too great.
    def lock(handler: => Unit) = {
      if ((rawflags & LOCKED) != 0L) {
        if (settings.Yrecursion.value != 0) {
          recursionTable get this match {
            case Some(n) =>
              if (n > settings.Yrecursion.value) {
                handler
              } else {
                recursionTable += (this -> (n + 1))
              }
            case None =>
              recursionTable += (this -> 1)
          }
        } else { handler }
      } else {
        rawflags |= LOCKED
        activeLocks += 1
      }
    }

    // Unlock a symbol
    def unlock() = {
      if ((rawflags & LOCKED) != 0L) {
        activeLocks -= 1
        rawflags = rawflags & ~LOCKED
        if (settings.Yrecursion.value != 0)
          recursionTable -= this
      }
    }

// Tests ----------------------------------------------------------------------

    /** Is this symbol a type but not a class? */
    def isNonClassType = false

    /** Term symbols with the exception of static parts of Java classes and packages.
     */
    final def isValue = isTerm && !(isModule && hasFlag(PACKAGE | JAVA))

    final def isVariable  = isTerm && hasFlag(MUTABLE) && !isMethod

    // interesting only for lambda lift. Captured variables are accessed from inner lambdas.
    final def isCapturedVariable  = isVariable && hasFlag(CAPTURED)

    final def isGetter = isTerm && hasFlag(ACCESSOR) && !nme.isSetterName(name)
    final def isSetter = isTerm && hasFlag(ACCESSOR) && nme.isSetterName(name)
       //todo: make independent of name, as this can be forged.

    final def hasGetter = isTerm && nme.isLocalName(name)

    final def isValueParameter = isTerm && hasFlag(PARAM)
    final def isLocalDummy = isTerm && nme.isLocalDummyName(name)
    final def isLabel = isMethod && !hasFlag(ACCESSOR) && hasFlag(LABEL)
    final def isInitializedToDefault = !isType && (getFlag(DEFAULTINIT | ACCESSOR) == (DEFAULTINIT | ACCESSOR))
    final def isClassConstructor = isTerm && (name == nme.CONSTRUCTOR)
    final def isMixinConstructor = isTerm && (name == nme.MIXIN_CONSTRUCTOR)
    final def isConstructor = isTerm && (name == nme.CONSTRUCTOR) || (name == nme.MIXIN_CONSTRUCTOR)
    final def isStaticModule = isModule && isStatic && !isMethod
    final def isThisSym = isTerm && owner.thisSym == this
    //final def isMonomorphicType = isType && hasFlag(MONOMORPHIC)
    final def isError = hasFlag(IS_ERROR)
    final def isErroneous = isError || isInitialized && tpe.isErroneous
    override final def isTrait: Boolean = isClass && hasFlag(TRAIT | notDEFERRED)     // A virtual class becomes a trait (part of DEVIRTUALIZE)
    final def isTypeParameterOrSkolem = isType && hasFlag(PARAM)
    final def isTypeSkolem            = isSkolem && hasFlag(PARAM)
    // a type symbol bound by an existential type, for instance the T in
    // List[T] forSome { type T }
    final def isExistentialSkolem     = isExistentiallyBound && isSkolem
    final def isExistentialQuantified = isExistentiallyBound && !isSkolem

    // class C extends D( { class E { ... } ... } ). Here, E is a class local to a constructor
    final def isClassLocalToConstructor = isClass && hasFlag(INCONSTRUCTOR)

    final def isAnonymousClass = isClass && (originalName startsWith nme.ANON_CLASS_NAME) // todo: find out why we can't use containsName here.
    final def isAnonymousFunction = hasFlag(SYNTHETIC) && (name containsName nme.ANON_FUN_NAME)

    final def isClassOfModule = isModuleClass || isClass && nme.isLocalName(name)
    final def isPackageObject = isModule && name == nme.PACKAGEkw && owner.isPackageClass
    final def isPackageObjectClass = isModuleClass && name.toTermName == nme.PACKAGEkw && owner.isPackageClass
    final def definedInPackage  = owner.isPackageClass || owner.isPackageObjectClass
    final def isPredefModule = isModule && name == nme.Predef && owner.isScalaPackageClass // not printed as a prefix
    final def isScalaPackage = isPackage && name == nme.scala_ && owner.isRoot || // not printed as a prefix
                               isPackageObject && owner.isScalaPackageClass
    final def isScalaPackageClass: Boolean = isPackageClass && owner.isRoot && name == nme.scala_.toTypeName ||
                                    isPackageObjectClass && owner.isScalaPackageClass // not printed as a prefix

    /** Is symbol a monomorphic type?
     *  assumption: if a type starts out as monomorphic, it will not acquire
     *  type parameters in later phases.
     */
    final def isMonomorphicType =
      isType && {
        var is = infos
        (is eq null) || {
          while (is.prev ne null) { is = is.prev }
          is.info.isComplete && is.info.typeParams.isEmpty
        }
      }

    def isDeprecated        = hasAnnotation(DeprecatedAttr)
    def deprecationMessage  = getAnnotationArg(DeprecatedAttr, 0) collect { case Literal(const) => const.stringValue }
    // !!! when annotation arguments are not literal strings, but any sort of
    // assembly of strings, there is a fair chance they will turn up here not as
    // Literal(const) but some arbitrary AST.  However nothing in the compiler
    // prevents someone from writing a @migration annotation with a calculated
    // string.  So this needs attention.  For now the fact that migration is
    // private[scala] ought to provide enough protection.
    def migrationMessage    = getAnnotationArg(MigrationAnnotationClass, 2) collect {
      case Literal(const) => const.stringValue
      case x              => x.toString           // should not be necessary, but better than silently ignoring an issue
    }
    def elisionLevel        = getAnnotationArg(ElidableMethodClass, 0) collect { case Literal(Constant(x: Int)) => x }

    /** Does this symbol denote a wrapper object of the interpreter or its class? */
    final def isInterpreterWrapper =
      (isModule || isModuleClass) &&
      owner.isEmptyPackageClass &&
      name.toString.startsWith(nme.INTERPRETER_LINE_PREFIX) &&
      name.toString.endsWith(nme.INTERPRETER_WRAPPER_SUFFIX)

    override def isEffectiveRoot = super.isEffectiveRoot || isInterpreterWrapper

    /** Is this symbol an accessor method for outer? */
    final def isOuterAccessor = {
      hasFlag(STABLE | SYNTHETIC) &&
      originalName == nme.OUTER
    }

    /** Is this symbol an accessor method for outer? */
    final def isOuterField = {
      hasFlag(SYNTHETIC) &&
      originalName == nme.OUTER_LOCAL
    }

    /** Does this symbol denote a stable value? */
    final def isStable =
      isTerm &&
      !hasFlag(MUTABLE) &&
      (!hasFlag(METHOD | BYNAMEPARAM) || hasFlag(STABLE)) &&
      !(tpe.isVolatile && !hasAnnotation(uncheckedStableClass))

    def isVirtualClass =
      hasFlag(DEFERRED) && isClass

    def isVirtualTrait =
      hasFlag(DEFERRED) && isTrait

    /** Is this symbol a public */

    /** Does this symbol denote the primary constructor of its enclosing class? */
    final def isPrimaryConstructor =
      isConstructor && owner.primaryConstructor == this

    /** Does this symbol denote an auxiliary constructor of its enclosing class? */
    final def isAuxiliaryConstructor =
      isConstructor && !isPrimaryConstructor

    /** Is this symbol a synthetic apply or unapply method in a companion object of a case class? */
    final def isCaseApplyOrUnapply =
      isMethod && hasFlag(CASE) && hasFlag(SYNTHETIC)


    /** Is this symbol a trait which needs an implementation class? */
    final def needsImplClass: Boolean =
      isTrait && (!hasFlag(INTERFACE) || hasFlag(lateINTERFACE)) && !isImplClass

    /** Is this a symbol which exists only in the implementation class, not in its trait? */
    final def isImplOnly: Boolean =
      hasFlag(PRIVATE) ||
      (owner.isImplClass || owner.isTrait) &&
      ((hasFlag(notPRIVATE | LIFTED) && !hasFlag(ACCESSOR | SUPERACCESSOR | MODULE) || isConstructor) ||
       (hasFlag(LIFTED) && isModule && isMethod))

    /** Is this symbol a module variable ? */
    final def isModuleVar: Boolean = isVariable && hasFlag(MODULEVAR)

    /** Is this symbol static (i.e. with no outer instance)? */
    final def isStatic: Boolean =
      hasFlag(STATIC) || isRoot || owner.isStaticOwner

    /** Is this symbol a static member of its class? (i.e. needs to be implemented as a Java static?) */
    final def isStaticMember: Boolean =
      hasFlag(STATIC) || owner.isImplClass

    /** Does this symbol denote a class that defines static symbols? */
    final def isStaticOwner: Boolean =
      isPackageClass || isModuleClass && isStatic

    /** Is this symbol effectively final? I.e, it cannot be overridden */
    final def isEffectivelyFinal: Boolean = isFinal || isTerm && (
      hasFlag(PRIVATE) || isLocal || owner.isClass && owner.hasFlag(FINAL | MODULE))

    /** Is this symbol locally defined? I.e. not accessed from outside `this' instance */
    final def isLocal: Boolean = owner.isTerm

    /** Is this symbol a constant? */
    final def isConstant: Boolean =
      isStable && (tpe match {
        case ConstantType(_) => true
        case PolyType(_, ConstantType(_)) => true
        case MethodType(_, ConstantType(_)) => true
        case _ => false
      })

    /** Is this class nested in another class or module (not a package)? */
    final def isNestedClass: Boolean =
      isClass && !isRoot && !owner.isPackageClass

    /** Is this class locally defined?
     *  A class is local, if
     *   - it is anonymous, or
     *   - its owner is a value
     *   - it is defined within a local class
     */
    final def isLocalClass: Boolean =
      isClass && (isAnonymousClass || isRefinementClass || isLocal ||
                  !owner.isPackageClass && owner.isLocalClass)

    /** Is this class or type defined as a structural refinement type?
     */
    final def isStructuralRefinement: Boolean =
      (isClass || isType || isModule) && info.normalize/*.underlying*/.isStructuralRefinement


    /** Is this symbol a member of class `clazz'
     */
    def isMemberOf(clazz: Symbol) =
      clazz.info.member(name).alternatives contains this

    /** A a member of class `base' is incomplete if
     *  (1) it is declared deferred or
     *  (2) it is abstract override and its super symbol in `base' is
     *      nonexistent or incomplete.
     *
     *  @param base ...
     *  @return     ...
     */
    final def isIncompleteIn(base: Symbol): Boolean =
      this.isDeferred ||
      (this hasFlag ABSOVERRIDE) && {
        val supersym = superSymbol(base)
        supersym == NoSymbol || supersym.isIncompleteIn(base)
      }

    final def exists: Boolean =
      this != NoSymbol && (!owner.isPackageClass || { rawInfo.load(this); rawInfo != NoType })

    final def isInitialized: Boolean =
      validTo != NoPeriod

    final def isStableClass: Boolean = {
      def hasNoAbstractTypeMember(clazz: Symbol): Boolean =
        (clazz hasFlag STABLE) || {
          var e = clazz.info.decls.elems
          while ((e ne null) && !(e.sym.isAbstractType && info.member(e.sym.name) == e.sym))
            e = e.next
          e == null
        }
      def checkStable() =
        (info.baseClasses forall hasNoAbstractTypeMember) && { setFlag(STABLE); true }
      isClass && (hasFlag(STABLE) || checkStable())
    }


    /** The variance of this symbol as an integer */
    final def variance: Int =
      if (isCovariant) 1
      else if (isContravariant) -1
      else 0

// Flags, owner, and name attributes --------------------------------------------------------------

    def owner: Symbol = rawowner
    override final def owner_=(owner: Symbol) { rawowner = owner }

    def ownerChain: List[Symbol] = this :: owner.ownerChain

    def ownersIterator: Iterator[Symbol] = new Iterator[Symbol] {
      private var current = Symbol.this
      def hasNext = current ne NoSymbol
      def next = { val r = current; current = current.owner; r }
    }

    // same as ownerChain contains sym, but more efficient
    def hasTransOwner(sym: Symbol) = {
      var o = this
      while ((o ne sym) && (o ne NoSymbol)) o = o.owner
      o eq sym
    }

    def name: Name = rawname

    final def name_=(name: Name) {
      if (name != rawname) {
        if (owner.isClass) {
          var ifs = owner.infos
          while (ifs != null) {
            ifs.info.decls.rehash(this, name)
            ifs = ifs.prev
          }
        }
        rawname = name
      }
    }

    /** If this symbol has an expanded name, its original name, otherwise its name itself.
     *  @see expandName
     */
    def originalName = nme.originalName(name)

    final def flags: Long = {
      val fs = rawflags & phase.flagMask
      (fs | ((fs & LateFlags) >>> LateShift)) & ~(fs >>> AntiShift)
    }
    override final def flags_=(fs: Long) = rawflags = fs
    final def setFlag(mask: Long): this.type = { rawflags = rawflags | mask; this }
    final def resetFlag(mask: Long): this.type = { rawflags = rawflags & ~mask; this }
    final def getFlag(mask: Long): Long = flags & mask
    final def resetFlags { rawflags = rawflags & TopLevelCreationFlags }

    /** The class or term up to which this symbol is accessible,
     *  or RootClass if it is public
     */
    def accessBoundary(base: Symbol): Symbol = {
      if (hasFlag(PRIVATE) || owner.isTerm) owner
      else if (privateWithin != NoSymbol && !phase.erasedTypes) privateWithin
      else if (hasFlag(PROTECTED)) base
      else RootClass
    }

    def isLessAccessibleThan(other: Symbol): Boolean = {
      val tb = this.accessBoundary(owner)
      val ob1 = other.accessBoundary(owner)
      val ob2 = ob1.linkedClassOfClass
      var o = tb
      while (o != NoSymbol && o != ob1 && o != ob2) {
        o = o.owner
      }
      o != NoSymbol && o != tb
    }

// Info and Type -------------------------------------------------------------------

    private[Symbols] var infos: TypeHistory = null

    /** Get type. The type of a symbol is:
     *  for a type symbol, the type corresponding to the symbol itself,
     *    @M you should use tpeHK for a type symbol with type parameters if
     *       the kind of the type need not be *, as tpe introduces dummy arguments
     *       to generate a type of kind *
     *  for a term symbol, its usual type
     */
    override def tpe: Type = info

    /** Get type info associated with symbol at current phase, after
     *  ensuring that symbol is initialized (i.e. type is completed).
     */
    override def info: Type = try {
      var cnt = 0
      while (validTo == NoPeriod) {
        //if (settings.debug.value) System.out.println("completing " + this);//DEBUG
        assert(infos ne null, this.name)
        assert(infos.prev eq null, this.name)
        val tp = infos.info
        //if (settings.debug.value) System.out.println("completing " + this.rawname + tp.getClass());//debug
        if ((rawflags & LOCKED) != 0L) { // rolled out once for performance
          lock {
            setInfo(ErrorType)
            throw CyclicReference(this, tp)
          }
        } else {
          rawflags |= LOCKED
          activeLocks += 1
        }
        val current = phase
        try {
          phase = phaseOf(infos.validFrom)
          tp.complete(this)
          // if (settings.debug.value && runId(validTo) == currentRunId) System.out.println("completed " + this/* + ":" + info*/);//DEBUG
          unlock()
        } finally {
          phase = current
        }
        cnt += 1
        // allow for two completions:
        //   one: sourceCompleter to LazyType, two: LazyType to completed type
        if (cnt == 3) abort("no progress in completing " + this + ":" + tp)
      }
      val result = rawInfo
      result
    } catch {
      case ex: CyclicReference =>
        if (settings.debug.value) println("... trying to complete "+this)
        throw ex
    }

    override def info_=(info: Type) {
      assert(info ne null)
      infos = TypeHistory(currentPeriod, info, null)
      unlock()
      validTo = if (info.isComplete) currentPeriod else NoPeriod
    }

    /** Set initial info. */
    def setInfo(info: Type): this.type = { info_=(info); this }

    def setInfoOwnerAdjusted(info: Type): this.type = setInfo(info.atOwner(this))

    /** Set new info valid from start of this phase. */
    final def updateInfo(info: Type): Symbol = {
      assert(phaseId(infos.validFrom) <= phase.id)
      if (phaseId(infos.validFrom) == phase.id) infos = infos.prev
      infos = TypeHistory(currentPeriod, info, infos)
      this
    }

    def hasRawInfo: Boolean = infos ne null

    /** Return info without checking for initialization or completing */
    def rawInfo: Type = {
      var infos = this.infos
      assert(infos != null)
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

    // adapt to new run in fsc.
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
    final def isUpdatedAt(pid: Phase#Id): Boolean = {
      var infos = this.infos
      while ((infos ne null) && phaseId(infos.validFrom) != pid + 1) infos = infos.prev
      infos ne null
    }

    /** Was symbol's type updated during given phase? */
    final def hasTypeAt(pid: Phase#Id): Boolean = {
      var infos = this.infos
      while ((infos ne null) && phaseId(infos.validFrom) > pid) infos = infos.prev
      infos ne null
    }

    /** Modify term symbol's type so that a raw type C is converted to an existential C[_]
     *
     * This is done in checkAccessible and overriding checks in refchecks
     * We can't do this on class loading because it would result in infinite cycles.
     */
    private var triedCooking: Boolean = false
    final def cookJavaRawInfo() {
      // println("cookJavaRawInfo: "+(rawname, triedCooking))
      if(triedCooking) return else triedCooking = true // only try once...
      doCookJavaRawInfo()
    }

    protected def doCookJavaRawInfo(): Unit


    /** The type constructor of a symbol is:
     *  For a type symbol, the type corresponding to the symbol itself,
     *  excluding parameters.
     *  Not applicable for term symbols.
     */
    def typeConstructor: Type =
      abort("typeConstructor inapplicable for " + this)

    /** @M -- tpe vs tpeHK:
     * Symbol::tpe creates a TypeRef that has dummy type arguments to get a type of kind *
     * Symbol::tpeHK creates a TypeRef without type arguments, but with type params --> higher-kinded if non-empty list of tpars
     * calling tpe may hide errors or introduce spurious ones
     *   (e.g., when deriving a type from the symbol of a type argument that must be higher-kinded)
     * as far as I can tell, it only makes sense to call tpe in conjunction with a substitution that replaces the generated dummy type arguments by their actual types
     */
    def tpeHK = if (isType) typeConstructor else tpe // @M! used in memberType

    /** The type parameters of this symbol, without ensuring type completion.
     *  assumption: if a type starts out as monomorphic, it will not acquire
     *  type parameters later.
     */
    def unsafeTypeParams: List[Symbol] =
      if (isMonomorphicType) List()
      else {
        val current = phase
        try {
          while (phase.keepsTypeParams) phase = phase.prev
          rawInfo.typeParams
        } finally {
          phase = current
        }
      }

    /** The type parameters of this symbol.
     *  assumption: if a type starts out as monomorphic, it will not acquire
     *  type parameters later.
     */
    def typeParams: List[Symbol] =
      if (isMonomorphicType) List() else { rawInfo.load(this); rawInfo.typeParams }

    /** The value parameter sections of this symbol.
     */
    def paramss: List[List[Symbol]] = info.paramss

    /** The least proper supertype of a class; includes all parent types
     *  and refinement where needed. You need to compute that in a situation like this:
     *  {
     *    class C extends P { ... }
     *    new C
     *  }
     */
    def classBound: Type = {
      val tp = refinedType(info.parents, owner)
      val thistp = tp.typeSymbol.thisType
      val oldsymbuf = new ListBuffer[Symbol]
      val newsymbuf = new ListBuffer[Symbol]
      for (sym <- info.decls.toList) {
        // todo: what about public references to private symbols?
        if (sym.isPublic && !sym.isConstructor) {
          oldsymbuf += sym
          newsymbuf += (
            if (sym.isClass)
              tp.typeSymbol.newAbstractType(sym.pos, sym.name).setInfo(sym.existentialBound)
            else
              sym.cloneSymbol(tp.typeSymbol))
        }
      }
      val oldsyms = oldsymbuf.toList
      val newsyms = newsymbuf.toList
      for (sym <- newsyms) {
        addMember(thistp, tp, sym.setInfo(sym.info.substThis(this, thistp).substSym(oldsyms, newsyms)))
      }
      tp
    }

    /** If we quantify existentially over this symbol,
     *  the bound of the type variable that stands for it
     *  pre: symbol is a term, a class, or an abstract type (no alias type allowed)
     */
    def existentialBound: Type =
      if (this.isClass)
         polyType(this.typeParams, TypeBounds(NothingClass.tpe, this.classBound))
      else if (this.isAbstractType)
         this.info
      else if (this.isTerm)
         TypeBounds(NothingClass.tpe, intersectionType(List(this.tpe, SingletonClass.tpe)))
      else
        abort("unexpected alias type: "+this)

    /** Reset symbol to initial state
     */
    def reset(completer: Type) {
      resetFlags
      infos = null
      validTo = NoPeriod
      //limit = NoPhase.id
      setInfo(completer)
    }

// Comparisons ----------------------------------------------------------------

    /** A total ordering between symbols that refines the class
     *  inheritance graph (i.e. subclass.isLess(superclass) always holds).
     *  the ordering is given by: (_.isType, -_.baseTypeSeq.length) for type symbols, followed by `id'.
     */
    final def isLess(that: Symbol): Boolean = {
      def baseTypeSeqLength(sym: Symbol) =
        if (sym.isAbstractType) 1 + sym.info.bounds.hi.baseTypeSeq.length
        else sym.info.baseTypeSeq.length
      if (this.isType)
        (that.isType &&
         { val diff = baseTypeSeqLength(this) - baseTypeSeqLength(that)
           diff > 0 || diff == 0 && this.id < that.id })
      else
        that.isType || this.id < that.id
    }

    /** A partial ordering between symbols.
     *  (this isNestedIn that) holds iff this symbol is defined within
     *  a class or method defining that symbol
     */
    final def isNestedIn(that: Symbol): Boolean =
      owner == that || owner != NoSymbol && (owner isNestedIn that)

    /** Is this class symbol a subclass of that symbol? */
    final def isNonBottomSubClass(that: Symbol): Boolean =
      this == that || this.isError || that.isError ||
      info.baseTypeIndex(that) >= 0

    final def isSubClass(that: Symbol): Boolean = {
      isNonBottomSubClass(that) ||
      this == NothingClass ||
      this == NullClass &&
      (that == AnyClass ||
       that != NothingClass && (that isSubClass AnyRefClass))
    }

// Overloaded Alternatives ---------------------------------------------------------

    def alternatives: List[Symbol] =
      if (hasFlag(OVERLOADED)) info.asInstanceOf[OverloadedType].alternatives
      else List(this)

    def filter(cond: Symbol => Boolean): Symbol =
      if (hasFlag(OVERLOADED)) {
        //assert(info.isInstanceOf[OverloadedType], "" + this + ":" + info);//DEBUG
        val alts = alternatives
        val alts1 = alts filter cond
        if (alts1 eq alts) this
        else if (alts1.isEmpty) NoSymbol
        else if (alts1.tail.isEmpty) alts1.head
        else owner.newOverloaded(info.prefix, alts1)
      } else if (this == NoSymbol || cond(this)) {
        this
      } else NoSymbol

    def suchThat(cond: Symbol => Boolean): Symbol = {
      val result = filter(cond)
      assert(!(result hasFlag OVERLOADED), result.alternatives)
      result
    }

// Cloneing -------------------------------------------------------------------

    /** A clone of this symbol */
    final def cloneSymbol: Symbol =
      cloneSymbol(owner)

    /** A clone of this symbol, but with given owner */
    final def cloneSymbol(owner: Symbol): Symbol = {
      val newSym = cloneSymbolImpl(owner)
      newSym.setInfo(info.cloneInfo(newSym))
        .setFlag(this.rawflags).setAnnotations(this.annotations)
    }

    /** Internal method to clone a symbol's implementation without flags or type
     */
    def cloneSymbolImpl(owner: Symbol): Symbol

// Access to related symbols --------------------------------------------------

    /** The primary constructor of a class */
    def primaryConstructor: Symbol = {
      var c = info.decl(
        if (isTrait || isImplClass) nme.MIXIN_CONSTRUCTOR
        else nme.CONSTRUCTOR)
      c = if (c hasFlag OVERLOADED) c.alternatives.head else c
      //assert(c != NoSymbol)
      c
    }

    /** The self symbol of a class with explicit self type, or else the
     *  symbol itself.
     */
    def thisSym: Symbol = this

    /** The type of `this' in a class, or else the type of the symbol itself. */
    def typeOfThis = thisSym.tpe

    /** If symbol is a class, the type <code>this.type</code> in this class,
     * otherwise <code>NoPrefix</code>.
     * We always have: thisType <:< typeOfThis
     */
    def thisType: Type = NoPrefix

    /** Return every accessor of a primary constructor parameter in this case class.
     *  The scope declarations may be out of order because fields with less than private
     *  access are first given a regular getter, then a new renamed getter which comes
     *  later in the declaration list.  For this reason we have to pinpoint the
     *  right accessors by starting with the original fields (which will be in the right
     *  order) and looking for getters with applicable names.  The getters may have the
     *  standard name "foo" or may have been renamed to "foo$\d+" in SyntheticMethods.
     *  See ticket #1373.
     */
    final def caseFieldAccessors: List[Symbol] = {
      val allWithFlag = info.decls.toList filter (_ hasFlag CASEACCESSOR)
      val (accessors, fields) = allWithFlag partition (_.isMethod)

      def findAccessor(field: Symbol): Symbol = {
        // There is another renaming the field may have undergone, for instance as in
        // ticket #2175: case class Property[T](private var t: T), t becomes Property$$t.
        // So we use the original name everywhere.
        val getterName    = nme.getterName(field.originalName)

        // Note this is done in two passes intentionally, to ensure we pick up the original
        // getter if present before looking for the renamed getter.
        def origGetter    = accessors find (_.originalName == getterName)
        def renamedGetter = accessors find (_.originalName startsWith (getterName + "$"))
        val accessorName  = origGetter orElse renamedGetter

        // This fails more gracefully rather than throw an Error as it used to because
        // as seen in #2625, we can reach this point with an already erroneous tree.
        accessorName getOrElse NoSymbol
        // throw new Error("Could not find case accessor for %s in %s".format(field, this))
      }

      fields map findAccessor
    }

    final def constrParamAccessors: List[Symbol] =
      info.decls.toList filter (sym => !sym.isMethod && sym.hasFlag(PARAMACCESSOR))

    /** The symbol accessed by this accessor (getter or setter) function.
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
      else if (isClassLocalToConstructor) owner.enclClass.outerClass
      else owner.outerClass

    /** For a paramaccessor: a superclass paramaccessor for which this symbol
     *  is an alias, NoSymbol for all others
     */
    def alias: Symbol = NoSymbol

    /** For a lazy value, its lazy accessor. NoSymbol for all others */
    def lazyAccessor: Symbol = NoSymbol

    /** For an outer accessor: The class from which the outer originates.
     *  For all other symbols: NoSymbol
     */
    def outerSource: Symbol = NoSymbol

    /** The superclass of this class */
    def superClass: Symbol = if (info.parents.isEmpty) NoSymbol else info.parents.head.typeSymbol

    /** The directly or indirectly inherited mixins of this class
     *  except for mixin classes inherited by the superclass. Mixin classes appear
     *  in linearization order.
     */
    def mixinClasses: List[Symbol] = {
      val sc = superClass
      ancestors takeWhile (sc ne)
    }

    /** All directly or indirectly inherited classes.
     */
    def ancestors: List[Symbol] = info.baseClasses drop 1

    /** The package containing this symbol, or NoSymbol if there
     *  is not one. */
    def enclosingPackage: Symbol =
      if (this == NoSymbol) this else {
        var packSym = this.owner
        while ((packSym != NoSymbol)
               && !packSym.isPackageClass)
          packSym = packSym.owner
        if (packSym != NoSymbol)
          packSym = packSym.companionModule
        packSym
      }

    /** The top-level class containing this symbol */
    def toplevelClass: Symbol =
      if (owner.isPackageClass) {
        if (isClass) this else moduleClass
      } else owner.toplevelClass

    /** Is this symbol defined in the same scope and compilation unit as `that' symbol?
     */
    def isCoDefinedWith(that: Symbol) =
      (this.rawInfo ne NoType) && {
        val res =
          !this.owner.isPackageClass ||
          (this.sourceFile eq null) ||
          (that.sourceFile eq null) ||
          (this.sourceFile eq that.sourceFile) ||
          (this.sourceFile == that.sourceFile)

        // recognize companion object in separate file and fail, else compilation
        // appears to succeed but highly opaque errors come later: see bug #1286
        if (res == false) {
          val (f1, f2) = (this.sourceFile, that.sourceFile)
          if (f1 != null && f2 != null && f1.path != f2.path)
            throw FatalError("Companions '" + this + "' and '" + that + "' must be defined in same file.")
        }

        res
      }

    /** @PP: Added diagram because every time I come through here I end up
     *       losing my train of thought.  [Renaming occurs.] This diagram is a
     *       bit less necessary since the renaming, but leaving in place
     *       due to high artistic merit.
     *
     * class Foo  <
     *  ^  ^ (2)   \
     *  |  |  |     \
     *  | (5) |     (3)
     *  |  |  |       \
     * (1) v  v        \
     * object Foo (4)-> > class Foo$
     *
     * (1) companionClass
     * (2) companionModule
     * (3) linkedClassOfClass
     * (4) moduleClass
     * (5) companionSymbol
     */

    /** The class with the same name in the same package as this module or
     *  case class factory.
     */
    final def companionClass: Symbol = {
      if (this != NoSymbol)
        flatOwnerInfo.decl(name.toTypeName).suchThat(_ isCoDefinedWith this)
      else NoSymbol
    }

    /** A helper method that factors the common code used the discover a companion module of a class. If a companion
      * module exists, its symbol is returned, otherwise, `NoSymbol` is returned. The method assumes that `this`
      * symbol has already been checked to be a class (using `isClass`). */
    private final def companionModule0: Symbol =
      flatOwnerInfo.decl(name.toTermName).suchThat(
          sym => (sym hasFlag MODULE) && (sym isCoDefinedWith this))

    /** The module or case class factory with the same name in the same
     *  package as this class.
     */
    final def companionModule: Symbol =
      if (this.isClass && !this.isAnonymousClass && !this.isRefinementClass)
        companionModule0
      else NoSymbol

    /** For a module its linked class, for a class its linked module or case
     *  factory otherwise.
     */
    final def companionSymbol: Symbol =
      if (isTerm) companionClass
      else if (isClass)
        companionModule0
      else NoSymbol

    /** For a module class: its linked class
     *   For a plain class: the module class of its linked module.
     *
     *  Then object Foo has a `moduleClass' (invisible to the user, the backend calls it Foo$
     *  linkedClassOfClass goes from class Foo$ to class Foo, and back.
     */
    final def linkedClassOfClass: Symbol =
      if (isModuleClass) companionClass else companionModule.moduleClass

    /**
     * Returns the rawInfo of the owner. If the current phase has flat classes, it first
     * applies all pending type maps to this symbol.
     *
     * assume this is the ModuleSymbol for B in the following definition:
     *   package p { class A { object B { val x = 1 } } }
     *
     * The owner after flatten is "package p" (see "def owner"). The flatten type map enters
     * symbol B in the decls of p. So to find a linked symbol ("object B" or "class B")
     * we need to apply flatten to B first. Fixes #2470.
     */
    private final def flatOwnerInfo: Type = {
      if (phase.flatClasses && rawowner != NoSymbol && !rawowner.isPackageClass)
        info
      owner.rawInfo
    }

    /** If this symbol is an implementation class, its interface, otherwise the symbol itself
     *  The method follows two strategies to determine the interface.
     *   - during or after erasure, it takes the last parent of the implementation class
     *     (which is always the interface, by convention)
     *   - before erasure, it looks up the interface name in the scope of the owner of the class.
     *     This only works for implementation classes owned by other classes or traits.
     */
    final def toInterface: Symbol =
      if (isImplClass) {
        val result =
          if (phase.next.erasedTypes) {
            assert(!tpe.parents.isEmpty, this)
            tpe.parents.last.typeSymbol
          } else {
            owner.info.decl(nme.interfaceName(name))
          }
        assert(result != NoSymbol, this)
        result
      } else this

    /** The module class corresponding to this module.
     */
    def moduleClass: Symbol = NoSymbol

    /** The non-private symbol whose type matches the type of this symbol
     *  in in given class.
     *
     *  @param ofclazz   The class containing the symbol's definition
     *  @param site      The base type from which member types are computed
     */
    final def matchingSymbol(ofclazz: Symbol, site: Type): Symbol =
      ofclazz.info.nonPrivateDecl(name).filter(sym =>
        !sym.isTerm || (site.memberType(this) matches site.memberType(sym)))

    /** The non-private member of `site' whose type and name match the type of this symbol
     */
    final def matchingSymbol(site: Type, admit: Long = 0L): Symbol =
      site.nonPrivateMemberAdmitting(name, admit).filter(sym =>
        !sym.isTerm || (site.memberType(this) matches site.memberType(sym)))

    /** The symbol overridden by this symbol in given class `ofclazz'.
     *  @pre 'ofclazz' is a base class of this symbol's owner.
     */
    final def overriddenSymbol(ofclazz: Symbol): Symbol =
      if (isClassConstructor) NoSymbol else matchingSymbol(ofclazz, owner.thisType)

    /** The symbol overriding this symbol in given subclass `ofclazz'
     *  @pre: `ofclazz' is a subclass of this symbol's owner
     */
    final def overridingSymbol(ofclazz: Symbol): Symbol =
      if (isClassConstructor) NoSymbol else matchingSymbol(ofclazz, ofclazz.thisType)

    final def allOverriddenSymbols: List[Symbol] =
      if (!owner.isClass) Nil
      else owner.ancestors map overriddenSymbol filter (_ != NoSymbol)

    /** The virtual classes overridden by this virtual class (excluding `clazz' itself)
     *  Classes appear in linearization order (with superclasses before subclasses)
     */
    final def overriddenVirtuals: List[Symbol] =
      if (isVirtualTrait && hasFlag(OVERRIDE))
        this.owner.ancestors
          .map(_.info.decl(name))
          .filter(_.isVirtualTrait)
          .reverse
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
          sym = matchingSymbol(bcs.head, base.thisType).suchThat(!_.isDeferred)
        bcs = bcs.tail
      }
      sym
    }

    /** The getter of this value or setter definition in class `base', or NoSymbol if
     *  none exists.
     */
    final def getter(base: Symbol): Symbol = {
      val getterName = if (isSetter) nme.setterToGetter(name) else nme.getterName(name)
      base.info.decl(getterName) filter (_.hasFlag(ACCESSOR))
    }

    /** The setter of this value or getter definition, or NoSymbol if none exists */
    final def setter(base: Symbol): Symbol = setter(base, false)

    final def setter(base: Symbol, hasExpandedName: Boolean): Symbol = {
      var sname = nme.getterToSetter(nme.getterName(name))
      if (hasExpandedName) sname = nme.expandedSetterName(sname, base)
      base.info.decl(sname) filter (_.hasFlag(ACCESSOR))
    }

    /** The case module corresponding to this case class
     *  @pre case class is a member of some other class or package
     */
    final def caseModule: Symbol = {
      var modname = name.toTermName
      if (privateWithin.isClass && !privateWithin.isModuleClass && !hasFlag(EXPANDEDNAME))
        modname = nme.expandedName(modname, privateWithin)
      initialize.owner.info.decl(modname).suchThat(_.isModule)
    }

    /** If this symbol is a type parameter skolem (not an existential skolem!)
     *  its corresponding type parameter, otherwise this */
    def deSkolemize: Symbol = this

    /** If this symbol is an existential skolem the location (a Tree or null)
     *  where it was unpacked. Resulttype is AnyRef because trees are not visible here. */
    def unpackLocation: AnyRef = null

    /** Remove private modifier from symbol `sym's definition. If `sym' is a
     *  term symbol rename it by expanding its name to avoid name clashes
     */
    final def makeNotPrivate(base: Symbol) {
      if (this hasFlag PRIVATE) {
        setFlag(notPRIVATE)
        if (isMethod && !isDeferred) setFlag(lateFINAL)
        if (!isStaticModule && !isClassConstructor) {
          expandName(base)
          if (isModule) moduleClass.makeNotPrivate(base)
        }
      }
    }

    /** change name by appending $$<fully-qualified-name-of-class `base'>
     *  Do the same for any accessed symbols or setters/getters
     */
    def expandName(base: Symbol) {
      if (this.isTerm && this != NoSymbol && !hasFlag(EXPANDEDNAME)) {
        setFlag(EXPANDEDNAME)
        if (hasFlag(ACCESSOR) && !isDeferred) {
          accessed.expandName(base)
        } else if (hasGetter) {
          getter(owner).expandName(base)
          setter(owner).expandName(base)
        }
        name = nme.expandedName(name, base)
        if (isType) name = name.toTypeName
      }
    }

    def sourceFile: AbstractFile =
      (if (isModule) moduleClass else toplevelClass).sourceFile

    def sourceFile_=(f: AbstractFile) {
      abort("sourceFile_= inapplicable for " + this)
    }

    def isFromClassFile: Boolean =
      (if (isModule) moduleClass else toplevelClass).isFromClassFile

    /** If this is a sealed class, its known direct subclasses. Otherwise Set.empty */
    def children: List[Symbol] = Nil

    /** Recursively finds all sealed descendants and returns a sorted list. */
    def sealedDescendants: List[Symbol] = {
      val kids = children flatMap (_.sealedDescendants)
      val all = if (this hasFlag ABSTRACT) kids else this :: kids

      all.distinct sortBy (_.sealedSortName)
    }

// ToString -------------------------------------------------------------------

    /** A tag which (in the ideal case) uniquely identifies class symbols */
    final def tag: Int = fullName.hashCode()

    /** The simple name of this Symbol */
    final def simpleName: Name = name

    /** The String used to order otherwise identical sealed symbols.
     *  This uses data which is stable across runs and variable classpaths
     *  (the initial Name) before falling back on id, which varies depending
     *  on exactly when a symbol is loaded.
     */
    final def sealedSortName: String = initName.toString + "#" + id

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
      else if (isAnonymousClass) "anonymous class"
      else if (isRefinementClass) ""
      else if (isTrait) "trait"
      else if (isClass) "class"
      else if (isType) "type"
      else if (isTerm && hasFlag(LAZY)) "lazy value"
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
    def nameString: String = decodedName + idString

    /** The name of the symbol before decoding, e.g. `$eq$eq` instead of `==`.
     */
    def encodedName: String = name.toString

    /** If settings.uniqid is set, the symbol's id, else "" */
    final def idString: String =
      if (settings.uniqid.value) "#"+id // +" in "+owner.name+"#"+owner.id // DEBUG
      else ""

    /** String representation, including symbol's kind
     *  e.g., "class Foo", "method Bar".
     */
    override def toString(): String =
      if (isValueParameter && owner.isSetter)
        "parameter of setter "+owner.nameString
      else if (isPackageObject || isPackageObjectClass)
        "package object "+owner.nameString
      else
        compose(List(kindString,
                     if (isClassConstructor) owner.simpleName.decode+idString else nameString))

    /** If owner is a package object, its owner, else the normal owner.
     */
    def ownerSkipPackageObject =
      if (owner.isPackageObjectClass) owner.owner else owner

    /** String representation of location. */
    def locationString: String = {
      val owner = ownerSkipPackageObject
      if (owner.isClass &&
          ((!owner.isAnonymousClass &&
            !owner.isRefinementClass &&
            !owner.isInterpreterWrapper &&
            !owner.isRoot &&
            !owner.isEmptyPackageClass) || settings.debug.value))
        " in " + owner else ""
    }

    /** String representation of symbol's definition following its name */
    final def infoString(tp: Type): String = {
      def typeParamsString: String = tp match {
        case PolyType(tparams, _) if (tparams.length != 0) =>
          (tparams map (_.defString)).mkString("[", ",", "]")
        case _ =>
          ""
      }
      if (isClass)
        typeParamsString + " extends " + tp.resultType
      else if (isAliasType)
        typeParamsString + " = " + tp.resultType
      else if (isAbstractType)
        typeParamsString + {
          tp.resultType match {
            case TypeBounds(lo, hi) =>
              (if (lo.typeSymbol == NothingClass) "" else " >: " + lo) +
              (if (hi.typeSymbol == AnyClass) "" else " <: " + hi)
            case rtp =>
              "<: " + rtp
          }
        }
      else if (isModule)
        moduleClass.infoString(tp)
      else
        tp match {
          case PolyType(tparams, res) =>
            typeParamsString + infoString(res)
          case MethodType(params, res) =>
           params.map(_.defString).mkString("(", ",", ")") + infoString(res)
          case _ =>
            ": " + tp
        }
    }

    def infosString = infos.toString()

    /** String representation of symbol's variance */
    def varianceString: String =
      if (variance == 1) "+"
      else if (variance == -1) "-"
      else ""

    /** String representation of symbol's definition */
    def defString: String = {
      val f = if (settings.debug.value) flags
              else if (owner.isRefinementClass) flags & ExplicitFlags & ~OVERRIDE
              else flags & ExplicitFlags
      compose(List(flagsToString(f), keyString, varianceString + nameString +
                   (if (hasRawInfo) infoString(rawInfo) else "<_>")))
    }

    /** Concatenate strings separated by spaces */
    private def compose(ss: List[String]): String =
      ss.filter("" !=).mkString("", " ", "")

    def isSingletonExistential: Boolean =
      (name endsWith nme.dottype) && (info.bounds.hi.typeSymbol isSubClass SingletonClass)

    /** String representation of existentially bound variable */
    def existentialToString =
      if (isSingletonExistential && !settings.debug.value)
        "val "+name.subName(0, name.length - nme.dottype.length)+": "+
        dropSingletonType(info.bounds.hi)
      else defString
  }

  /** A class for term symbols */
  class TermSymbol(initOwner: Symbol, initPos: Position, initName: Name)
  extends Symbol(initOwner, initPos, initName) {
    override def isTerm = true

    privateWithin = NoSymbol

    protected var referenced: Symbol = NoSymbol

    def cloneSymbolImpl(owner: Symbol): Symbol =
      new TermSymbol(owner, pos, name).copyAttrsFrom(this)

    def copyAttrsFrom(original: TermSymbol): this.type = {
      referenced = original.referenced
      this
    }

    private val validAliasFlags = SUPERACCESSOR | PARAMACCESSOR | MIXEDIN | SPECIALIZED

    override def alias: Symbol =
      if (hasFlag(validAliasFlags)) initialize.referenced
      else NoSymbol

    def setAlias(alias: Symbol): TermSymbol = {
      assert(alias != NoSymbol, this)
      assert(!(alias hasFlag OVERLOADED), alias)

      assert(hasFlag(validAliasFlags), this)
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

    def setLazyAccessor(sym: Symbol): TermSymbol = {
      assert(hasFlag(LAZY) && (referenced == NoSymbol || referenced == sym), this)
      referenced = sym
      this
    }

    override def lazyAccessor: Symbol = {
      assert(hasFlag(LAZY), this)
      referenced
    }

    protected def doCookJavaRawInfo() {
      def cook(sym: Symbol) {
        require(sym hasFlag JAVA)
        // @M: I think this is more desirable, but Martin prefers to leave raw-types as-is as much as possible
        // object rawToExistentialInJava extends TypeMap {
        //   def apply(tp: Type): Type = tp match {
        //     // any symbol that occurs in a java sig, not just java symbols
        //     // see http://lampsvn.epfl.ch/trac/scala/ticket/2454#comment:14
        //     case TypeRef(pre, sym, List()) if !sym.typeParams.isEmpty =>
        //       val eparams = typeParamsToExistentials(sym, sym.typeParams)
        //       existentialAbstraction(eparams, TypeRef(pre, sym, eparams map (_.tpe)))
        //     case _ =>
        //       mapOver(tp)
        //   }
        // }
        val tpe1 = rawToExistential(sym.tpe)
        // println("cooking: "+ sym +": "+ sym.tpe +" to "+ tpe1)
        if (tpe1 ne sym.tpe) {
          sym.setInfo(tpe1)
        }
      }

      if (hasFlag(JAVA))
        cook(this)
      else if (hasFlag(OVERLOADED))
        for (sym2 <- alternatives)
          if (sym2 hasFlag JAVA)
            cook(sym2)
    }
  }

  /** A class for module symbols */
  class ModuleSymbol(initOwner: Symbol, initPos: Position, initName: Name)
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
          assert(rawowner.isClass, "fatal: %s has owner %s, but a class owner is required".format(rawname, rawowner))
          flatname = newTermName(compactify(rawowner.name.toString() + "$" + rawname))
        }
        flatname
      } else rawname

    override def cloneSymbolImpl(owner: Symbol): Symbol =
      new ModuleSymbol(owner, pos, name).copyAttrsFrom(this)
  }

  /** A class for method symbols */
  class MethodSymbol(initOwner: Symbol, initPos: Position, initName: Name)
  extends TermSymbol(initOwner, initPos, initName) {

    private var mtpePeriod = NoPeriod
    private var mtpePre: Type = _
    private var mtpeResult: Type = _

    override def cloneSymbolImpl(owner: Symbol): Symbol =
      new MethodSymbol(owner, pos, name).copyAttrsFrom(this)

    def typeAsMemberOf(pre: Type): Type = {
      if (mtpePeriod == currentPeriod) {
        if (mtpePre eq pre) return mtpeResult
      } else if (isValid(mtpePeriod)) {
        mtpePeriod = currentPeriod
        if (mtpePre eq pre) return mtpeResult
      }
      val res = pre.computeMemberType(this)
      mtpePeriod = currentPeriod
      mtpePre = pre
      mtpeResult = res
      res
    }
  }

  /** A class of type symbols. Alias and abstract types are direct instances
   *  of this class. Classes are instances of a subclass.
   */
  class TypeSymbol(initOwner: Symbol, initPos: Position, initName: Name)
  extends Symbol(initOwner, initPos, initName) {
    privateWithin = NoSymbol
    private var tyconCache: Type = null
    private var tyconRunId = NoRunId
    private var tpeCache: Type = _
    private var tpePeriod = NoPeriod

    override def isType = true
    override def isNonClassType = true
    override def isAbstractType = isDeferred
    override def isAliasType = !isDeferred

    /** Let's say you have a type definition
     *
     *    type T <: Number
     *
     *  and tsym is the symbol corresponding to T. Then
     *
     *    tsym.info = TypeBounds(Nothing, Number)
     *    tsym.tpe  = TypeRef(NoPrefix, T, List())
     */
    override def tpe: Type = {
      if (tpeCache eq NoType) throw CyclicReference(this, typeConstructor)
      if (tpePeriod != currentPeriod) {
        if (isValid(tpePeriod)) {
          tpePeriod = currentPeriod
        } else {
          if (isInitialized) tpePeriod = currentPeriod
          tpeCache = NoType
          val targs =
            if (phase.erasedTypes && this != ArrayClass) List()
            else unsafeTypeParams map (_.typeConstructor) //@M! use typeConstructor to generate dummy type arguments,
            // sym.tpe should not be called on a symbol that's supposed to be a higher-kinded type
            // memberType should be used instead, that's why it uses tpeHK and not tpe
          tpeCache = typeRef(if (hasFlag(PARAM | EXISTENTIAL)) NoPrefix else owner.thisType,
                             this, targs)
        }
      }
      assert(tpeCache ne null/*, "" + this + " " + phase*/)//debug
      tpeCache
    }

    // needed for experimental code for early types as type parameters
    // def refreshType() { tpePeriod = NoPeriod }

    override def typeConstructor: Type = {
      if ((tyconCache eq null) || tyconRunId != currentRunId) {
        tyconCache = typeRef(if (hasFlag(PARAM | EXISTENTIAL)) NoPrefix else owner.thisType,
                             this, List())
        tyconRunId = currentRunId
      }
      assert(tyconCache ne null)
      tyconCache
    }

    override def info_=(tp: Type) {
      tpePeriod = NoPeriod
      tyconCache = null
      if (tp.isComplete)
        tp match {
          case PolyType(_, _) => resetFlag(MONOMORPHIC)
          case NoType | AnnotatedType(_, _, _) => ;
          case _ => setFlag(MONOMORPHIC)
        }
      super.info_=(tp)
    }

    override def reset(completer: Type) {
      super.reset(completer)
      tpePeriod = NoPeriod
      tyconRunId = NoRunId
    }

    /*** example:
     * public class Test3<T> {}
     * public class Test1<T extends Test3> {}
     * info for T in Test1 should be >: Nothing <: Test3[_]
     */
    protected def doCookJavaRawInfo() {
      // don't require hasFlag(JAVA), since T in the above example does not have that flag
      val tpe1 = rawToExistential(info)
      // println("cooking type: "+ this +": "+ info +" to "+ tpe1)
      if (tpe1 ne info) {
        setInfo(tpe1)
      }
    }

    def cloneSymbolImpl(owner: Symbol): Symbol =
      new TypeSymbol(owner, pos, name)

    incCounter(typeSymbolCount)
  }

  /** A class for type parameters viewed from inside their scopes
   *
   *  @param origin  Can be either a tree, or a symbol, or null.
   *  If skolem got created from newTypeSkolem (called in Namers), origin denotes
   *  the type parameter from which the skolem was created. If it got created from
   *  skolemizeExistential, origin is either null or a Tree. If it is a Tree, it indicates
   *  where the skolem was introduced (this is important for knowing when to pack it
   *  again into ab Existential). origin is `null' only in skolemizeExistentials called
   *  from <:< or isAsSpecific, because here its value does not matter.
   *  I elieve the following invariant holds:
   *
   *     origin.isInstanceOf[Symbol] == !hasFlag(EXISTENTIAL)
   */
  class TypeSkolem(initOwner: Symbol, initPos: Position,
                   initName: Name, origin: AnyRef)
  extends TypeSymbol(initOwner, initPos, initName) {

    /** The skolemization level in place when the skolem was constructed */
    val level = skolemizationLevel

    override def isSkolem = true

    /** If typeskolem comes from a type parameter, that parameter, otherwise skolem itself */
    override def deSkolemize = origin match {
      case s: Symbol => s
      case _ => this
    }

    /** If type skolem comes from an existential, the tree where it was created */
    override def unpackLocation = origin

    override def typeParams = info.typeParams //@M! (not deSkolemize.typeParams!!), also can't leave superclass definition: use info, not rawInfo

    override def cloneSymbolImpl(owner: Symbol): Symbol =
      new TypeSkolem(owner, pos, name, origin)

    override def nameString: String =
      if (settings.debug.value) (super.nameString + "&" + level)
      else super.nameString
  }


  /** A class for class symbols */
  class ClassSymbol(initOwner: Symbol, initPos: Position, initName: Name)
  extends TypeSymbol(initOwner, initPos, initName) {

    /** The classfile from which this class was loaded. Maybe null. */
    var classFile: AbstractFile = null;

    private var source: AbstractFile = null
    override def sourceFile =
      if (owner.isPackageClass) source else super.sourceFile
    override def sourceFile_=(f: AbstractFile) {
      //System.err.println("set source file of " + this + ": " + f);
      source = f
    }
    override def isFromClassFile = {
      if (classFile ne null) true
      else if (owner.isPackageClass) false
      else super.isFromClassFile
    }
    private var thissym: Symbol = this

    override def isClass: Boolean = true
    override def isNonClassType = false
    override def isAbstractType = false
    override def isAliasType = false

    override def reset(completer: Type) {
      super.reset(completer)
      thissym = this
    }

    private var flatname = nme.EMPTY

    override def owner: Symbol =
      if (phase.flatClasses && rawowner != NoSymbol && !rawowner.isPackageClass) rawowner.owner
      else rawowner

    override def name: Name =
      if ((rawflags & notDEFERRED) != 0L && phase.devirtualized && !phase.erasedTypes) {
        newTypeName(rawname+"$trait") // (part of DEVIRTUALIZE)
      } else if (phase.flatClasses && rawowner != NoSymbol && !rawowner.isPackageClass) {
        if (flatname == nme.EMPTY) {
          assert(rawowner.isClass, "fatal: %s has owner %s, but a class owner is required".format(rawname, rawowner))
          flatname = newTypeName(compactify(rawowner.name.toString() + "$" + rawname))
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
        if (!isValid(period)) thisTypeCache = ThisType(this)
      }
      thisTypeCache
    }

    /** A symbol carrying the self type of the class as its type */
    override def thisSym: Symbol = thissym

    /** the self type of an object foo is foo.type, not class<foo>.this.type
     */
    override def typeOfThis: Type =
      if (getFlag(MODULE | IMPLCLASS) == MODULE.toLong && owner != NoSymbol)
        singleType(owner.thisType, sourceModule)
      else thissym.tpe

    /** Sets the self type of the class */
    override def typeOfThis_=(tp: Type) {
      thissym = newThisSym(pos).setInfo(tp)
    }

    override def cloneSymbolImpl(owner: Symbol): Symbol = {
      val clone = new ClassSymbol(owner, pos, name)
      if (thisSym != this) {
        clone.typeOfThis = typeOfThis
        clone.thisSym.name = thisSym.name
      }
      clone
    }

    override def sourceModule =
      if (isModuleClass) companionModule else NoSymbol

    private var childSet: Set[Symbol] = Set()
    override def children: List[Symbol] = childSet.toList sortBy (_.sealedSortName)
    override def addChild(sym: Symbol) { childSet = childSet + sym }

    incCounter(classSymbolCount)
  }

  /** A class for module class symbols
   *  Note: Not all module classes are of this type; when unpickled, we get
   *  plain class symbols!
   */
  class ModuleClassSymbol(owner: Symbol, pos: Position, name: Name)
  extends ClassSymbol(owner, pos, name) {
    private var module: Symbol = null
    def this(module: TermSymbol) = {
      this(module.owner, module.pos, module.name.toTypeName)
      setFlag(module.getFlag(ModuleToClassFlags) | MODULE | FINAL)
      sourceModule = module
    }
    override def sourceModule = module
    lazy val implicitMembers = info.implicitMembers
    override def sourceModule_=(module: Symbol) { this.module = module }
  }

  /** An object representing a missing symbol */
  object NoSymbol extends Symbol(null, NoPosition, nme.NOSYMBOL) {
    setInfo(NoType)
    privateWithin = this
    override def info_=(info: Type) {
      infos = TypeHistory(1, NoType, null)
      unlock()
      validTo = currentPeriod
    }
    override def defString: String = toString
    override def locationString: String = ""
    override def enclClass: Symbol = this
    override def toplevelClass: Symbol = this
    override def enclMethod: Symbol = this
    override def owner: Symbol = abort("no-symbol does not have owner")
    override def sourceFile: AbstractFile = null
    override def ownerChain: List[Symbol] = List()
    override def ownersIterator: Iterator[Symbol] = Iterator.empty
    override def alternatives: List[Symbol] = List()
    override def reset(completer: Type) {}
    override def info: Type = NoType
    override def rawInfo: Type = NoType
    protected def doCookJavaRawInfo() {}
    override def accessBoundary(base: Symbol): Symbol = RootClass
    def cloneSymbolImpl(owner: Symbol): Symbol = abort()
  }


  def cloneSymbols(syms: List[Symbol]): List[Symbol] = {
    val syms1 = syms map (_.cloneSymbol)
    for (sym1 <- syms1) sym1.setInfo(sym1.info.substSym(syms, syms1))
    syms1
  }

  def cloneSymbols(syms: List[Symbol], owner: Symbol): List[Symbol] = {
    val syms1 = syms map (_.cloneSymbol(owner))
    for (sym1 <- syms1) sym1.setInfo(sym1.info.substSym(syms, syms1))
    syms1
  }

  /** An exception for cyclic references of symbol definitions */
  case class CyclicReference(sym: Symbol, info: Type)
  extends TypeError("illegal cyclic reference involving " + sym) {
    // printStackTrace() // debug
  }

  /** A class for type histories */
  private sealed case class TypeHistory(var validFrom: Period, info: Type, prev: TypeHistory) {
    assert((prev eq null) || phaseId(validFrom) > phaseId(prev.validFrom), this)
    assert(validFrom != NoPeriod)
    override def toString() =
      "TypeHistory(" + phaseOf(validFrom)+":"+runId(validFrom) + "," + info + "," + prev + ")"
  }
}

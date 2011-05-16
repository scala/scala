 /* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.reflect
package internal

import scala.collection.{ mutable, immutable }
import scala.collection.mutable.ListBuffer
import util.Statistics._
import Flags._

trait Symbols /* extends reflect.generic.Symbols*/ { self: SymbolTable =>
  import definitions._

  private var ids = 0
  def symbolCount = ids // statistics

  val emptySymbolArray = new Array[Symbol](0)

  /** Used for deciding in the IDE whether we can interrupt the compiler */
  //protected var activeLocks = 0

  /** Used for debugging only */
  //protected var lockedSyms = collection.immutable.Set[Symbol]()

  /** Used to keep track of the recursion depth on locked symbols */
  private var recursionTable = immutable.Map.empty[Symbol, Int]

  private var nextexid = 0
  private def freshExistentialName(suffix: String) = {
    nextexid += 1
    newTypeName("_" + nextexid + suffix)
  }

  /** The original owner of a class. Used by the backend to generate
   *  EnclosingMethod attributes.
   */
  val originalOwner = mutable.HashMap[Symbol, Symbol]()

  /** The class for all symbols */
  abstract class Symbol(initOwner: Symbol, initPos: Position, initName: Name) extends HasFlags /*AbsSymbol */ {

    type FlagsType          = Long
    type AccessBoundaryType = Symbol
    type AnnotationType     = AnnotationInfo

    var rawowner = initOwner
    var rawname  = initName
    var rawflags = 0L

    private var rawpos = initPos
    val id = { ids += 1; ids } // identity displayed when -uniqid

    var validTo: Period = NoPeriod

    def pos = rawpos
    def setPos(pos: Position): this.type = { this.rawpos = pos; this }

// ------ creators -------------------------------------------------------------------

    final def newValue(pos: Position, name: TermName) =
      new TermSymbol(this, pos, name)
    final def newValue(name: TermName, pos: Position = NoPosition) =
      new TermSymbol(this, pos, name)
    final def newVariable(pos: Position, name: TermName) =
      newValue(pos, name).setFlag(MUTABLE)
    final def newValueParameter(pos: Position, name: TermName) =
      newValue(pos, name).setFlag(PARAM)
    /** Create local dummy for template (owner of local blocks) */
    final def newLocalDummy(pos: Position) =
      newValue(pos, nme.localDummyName(this)).setInfo(NoType)
    final def newMethod(pos: Position, name: TermName) =
      new MethodSymbol(this, pos, name).setFlag(METHOD)
    final def newMethod(name: TermName, pos: Position = NoPosition) =
      new MethodSymbol(this, pos, name).setFlag(METHOD)
    final def newLabel(pos: Position, name: TermName) =
      newMethod(pos, name).setFlag(LABEL)
    final def newConstructor(pos: Position) =
      newMethod(pos, nme.CONSTRUCTOR)
    final def newModule(pos: Position, name: TermName, clazz: ClassSymbol) =
      new ModuleSymbol(this, pos, name).setFlag(MODULE | FINAL)
        .setModuleClass(clazz)
    final def newModule(name: TermName, clazz: Symbol, pos: Position = NoPosition) =
      new ModuleSymbol(this, pos, name).setFlag(MODULE | FINAL)
        .setModuleClass(clazz.asInstanceOf[ClassSymbol])
    final def newModule(pos: Position, name: TermName) = {
      val m = new ModuleSymbol(this, pos, name).setFlag(MODULE | FINAL)
      m.setModuleClass(new ModuleClassSymbol(m))
    }
    final def newPackage(pos: Position, name: TermName) = {
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
      newValue(alternatives.head.pos, alternatives.head.name.toTermName)
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

    final def newErrorValue(name: TermName) =
      newValue(pos, name).setFlag(SYNTHETIC | IS_ERROR).setInfo(ErrorType)

    /** Symbol of a type definition  type T = ...
     */
    final def newAliasType(pos: Position, name: TypeName) =
      new TypeSymbol(this, pos, name)
    final def newAliasType(name: TypeName, pos: Position = NoPosition) =
      new TypeSymbol(this, pos, name)

    /** Symbol of an abstract type  type T >: ... <: ...
     */
    final def newAbstractType(pos: Position, name: TypeName) =
      new TypeSymbol(this, pos, name).setFlag(DEFERRED)
    final def newAbstractType(name: TypeName, pos: Position = NoPosition) =
      new TypeSymbol(this, pos, name).setFlag(DEFERRED)

    /** Symbol of a type parameter
     */
    final def newTypeParameter(pos: Position, name: TypeName) =
      newAbstractType(pos, name).setFlag(PARAM)

    /** Synthetic value parameters when parameter symbols are not available
     */
    final def newSyntheticValueParamss(argtypess: List[List[Type]]): List[List[Symbol]] = {
      var cnt = 0
      def freshName() = { cnt += 1; newTermName("x$" + cnt) }
      def param(tp: Type) =
        newValueParameter(focusPos(owner.pos), freshName()).setFlag(SYNTHETIC).setInfo(tp)
      argtypess map (_.map(param))
    }

    final def newExistential(pos: Position, name: TypeName): Symbol =
      newAbstractType(pos, name).setFlag(EXISTENTIAL)

    final def freshExistential(suffix: String): Symbol =
      newExistential(pos, freshExistentialName(suffix))

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
      new TypeSkolem(owner, pos, name.toTypeName, this)
        .setFlag(flags)

    final def newClass(pos: Position, name: TypeName) =
      new ClassSymbol(this, pos, name)
    final def newClass(name: TypeName, pos: Position = NoPosition) =
      new ClassSymbol(this, pos, name)

    final def newModuleClass(pos: Position, name: TypeName) =
      new ModuleClassSymbol(this, pos, name)
    final def newModuleClass(name: TypeName, pos: Position = NoPosition) =
      new ModuleClassSymbol(this, pos, name)

    final def newAnonymousClass(pos: Position) =
      newClass(pos, tpnme.ANON_CLASS_NAME)
    final def newAnonymousFunctionClass(pos: Position) =
      newClass(pos, tpnme.ANON_FUN_NAME)

    /** Refinement types P { val x: String; type T <: Number }
     *  also have symbols, they are refinementClasses
     */
    final def newRefinementClass(pos: Position) =
      newClass(pos, tpnme.REFINE_CLASS_NAME)

    /** Create a new getter for current symbol (which must be a field)
     */
    final def newGetter: Symbol = {
      val getter = owner.newMethod(focusPos(pos), nme.getterName(name)).setFlag(getterFlags(flags))
      getter.privateWithin = privateWithin
      getter.setInfo(MethodType(List(), tpe))
    }

    final def newErrorClass(name: TypeName) = {
      val clazz = newClass(pos, name).setFlag(SYNTHETIC | IS_ERROR)
      clazz.setInfo(ClassInfoType(List(), new ErrorScope(this), clazz))
      clazz
    }

    final def newErrorSymbol(name: Name): Symbol = name match {
      case x: TypeName  => newErrorClass(x)
      case x: TermName  => newErrorValue(x)
    }

// ----- locking and unlocking ------------------------------------------------------

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
//        activeLocks += 1
//        lockedSyms += this
      }
    }

    // Unlock a symbol
    def unlock() = {
      if ((rawflags & LOCKED) != 0L) {
//        activeLocks -= 1
//        lockedSyms -= this
        rawflags = rawflags & ~LOCKED
        if (settings.Yrecursion.value != 0)
          recursionTable -= this
      }
    }

// ----- tests ----------------------------------------------------------------------

    def isTerm         = false  // to be overridden
    def isType         = false  // to be overridden
    def isClass        = false  // to be overridden
    def isAliasType    = false  // to be overridden
    def isAbstractType = false  // to be overridden
    private[scala] def isSkolem = false // to be overridden

    /** Is this symbol a type but not a class? */
    def isNonClassType = false // to be overridden

    override final def isTrait: Boolean = isClass && hasFlag(TRAIT | notDEFERRED)     // A virtual class becomes a trait (part of DEVIRTUALIZE)
    final def isAbstractClass = isClass && hasFlag(ABSTRACT)
    final def isBridge = hasFlag(BRIDGE)
    final def isContravariant = isType && hasFlag(CONTRAVARIANT)
    final def isCovariant = isType && hasFlag(COVARIANT)
    final def isEarlyInitialized: Boolean = isTerm && hasFlag(PRESUPER)
    final def isExistentiallyBound = isType && hasFlag(EXISTENTIAL)
    final def isImplClass = isClass && hasFlag(IMPLCLASS) // Is this symbol an implementation class for a mixin?
    final def isLazyAccessor = isLazy && lazyAccessor != NoSymbol
    final def isMethod = isTerm && hasFlag(METHOD)
    final def isVarargsMethod = isMethod && hasFlag(VARARGS)
    final def isModule = isTerm && hasFlag(MODULE)
    final def isModuleClass = isClass && hasFlag(MODULE)
    final def isOverloaded = hasFlag(OVERLOADED)
    final def isRefinementClass = isClass && name == tpnme.REFINE_CLASS_NAME
    final def isSourceMethod = isMethod && !hasFlag(STABLE) // exclude all accessors!!!
    final def isTypeParameter = isType && isParameter && !isSkolem

    /** Package tests */
    final def isEmptyPackage = isPackage && name == nme.EMPTY_PACKAGE_NAME
    final def isEmptyPackageClass = isPackageClass && name == tpnme.EMPTY_PACKAGE_NAME
    final def isPackage = isModule && hasFlag(PACKAGE)
    final def isPackageClass = isClass && hasFlag(PACKAGE)
    final def isRoot = isPackageClass && owner == NoSymbol
    final def isRootPackage = isPackage && owner == NoSymbol

    /** Does this symbol denote a wrapper created by the repl? */
    final def isInterpreterWrapper = (isModule || isModuleClass) && nme.isReplWrapperName(name)

    /** Is this symbol an effective root for fullname string?
     */
    def isEffectiveRoot = isRoot || isEmptyPackageClass || isInterpreterWrapper

    /** Term symbols with the exception of static parts of Java classes and packages.
     */
    final def isValue = isTerm && !(isModule && hasFlag(PACKAGE | JAVA))

    final def isVariable  = isTerm && isMutable && !isMethod

    // interesting only for lambda lift. Captured variables are accessed from inner lambdas.
    final def isCapturedVariable  = isVariable && hasFlag(CAPTURED)

    final def isGetter = isTerm && hasAccessorFlag && !nme.isSetterName(name)
    // todo: make independent of name, as this can be forged.
    final def isSetter = isTerm && hasAccessorFlag && nme.isSetterName(name)
    def isSetterParameter = isValueParameter && owner.isSetter

    final def hasGetter = isTerm && nme.isLocalName(name)

    final def isValueParameter = isTerm && hasFlag(PARAM)
    final def isLocalDummy = isTerm && nme.isLocalDummyName(name)
    final def isInitializedToDefault = !isType && hasAllFlags(DEFAULTINIT | ACCESSOR)
    final def isClassConstructor = isTerm && (name == nme.CONSTRUCTOR)
    final def isMixinConstructor = isTerm && (name == nme.MIXIN_CONSTRUCTOR)
    final def isConstructor = isTerm && nme.isConstructorName(name)
    final def isStaticModule = isModule && isStatic && !isMethod
    final def isThisSym = isTerm && owner.thisSym == this
    final def isError = hasFlag(IS_ERROR)
    final def isErroneous = isError || isInitialized && tpe.isErroneous
    final def isTypeParameterOrSkolem = isType && hasFlag(PARAM)
    final def isHigherOrderTypeParameter = owner.isTypeParameterOrSkolem
    final def isTypeSkolem            = isSkolem && hasFlag(PARAM)
    // a type symbol bound by an existential type, for instance the T in
    // List[T] forSome { type T }
    final def isExistentialSkolem     = isExistentiallyBound && isSkolem
    final def isExistentialQuantified = isExistentiallyBound && !isSkolem

    // class C extends D( { class E { ... } ... } ). Here, E is a class local to a constructor
    final def isClassLocalToConstructor = isClass && hasFlag(INCONSTRUCTOR)

    final def isAnonymousClass    = isClass && (name containsName tpnme.ANON_CLASS_NAME)
    final def isAnonymousFunction = isSynthetic && (name containsName tpnme.ANON_FUN_NAME)
    final def isAnonOrRefinementClass = isAnonymousClass || isRefinementClass

    final def isPackageObject = isModule && name == nme.PACKAGEkw && owner.isPackageClass
    final def isPackageObjectClass = isModuleClass && name.toTermName == nme.PACKAGEkw && owner.isPackageClass
    final def definedInPackage  = owner.isPackageClass || owner.isPackageObjectClass
    final def isJavaInterface = isJavaDefined && isTrait
    final def needsFlatClasses: Boolean = phase.flatClasses && rawowner != NoSymbol && !rawowner.isPackageClass

    // not printed as prefixes
    final def isPredefModule      = this == PredefModule
    final def isScalaPackage      = (this == ScalaPackage) || (isPackageObject && owner == ScalaPackageClass)
    final def isScalaPackageClass = skipPackageObject == ScalaPackageClass

    /** If this is a package object or package object class, its owner: otherwise this.
     */
    final def skipPackageObject: Symbol = if (isPackageObjectClass) owner else this

    /** If this is a constructor, its owner: otherwise this.
     */
    final def skipConstructor: Symbol = if (isConstructor) owner else this

    /** Conditions where we omit the prefix when printing a symbol, to avoid
     *  unpleasantries like Predef.String, $iw.$iw.Foo and <empty>.Bippy.
     */
    final def printWithoutPrefix = !settings.debug.value && (
      isScalaPackageClass || isPredefModule || isEffectiveRoot || isAnonOrRefinementClass || isInterpreterWrapper
    )

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

    def isStrictFP          = hasAnnotation(ScalaStrictFPAttr) || (enclClass hasAnnotation ScalaStrictFPAttr)
    def isSerializable      = info.baseClasses.exists(p => p == SerializableClass || p == JavaSerializableClass) || hasAnnotation(SerializableAttr) // last part can be removed, @serializable annotation is deprecated
    def isDeprecated        = hasAnnotation(DeprecatedAttr)
    def hasBridgeAnnotation = hasAnnotation(BridgeClass)
    def deprecationMessage  = getAnnotation(DeprecatedAttr) flatMap (_ stringArg 0)
    def deprecationVersion  = getAnnotation(DeprecatedAttr) flatMap (_ stringArg 1)
    // !!! when annotation arguments are not literal strings, but any sort of
    // assembly of strings, there is a fair chance they will turn up here not as
    // Literal(const) but some arbitrary AST.  However nothing in the compiler
    // prevents someone from writing a @migration annotation with a calculated
    // string.  So this needs attention.  For now the fact that migration is
    // private[scala] ought to provide enough protection.
    def migrationMessage    = getAnnotation(MigrationAnnotationClass) flatMap { _.stringArg(2) }
    def elisionLevel        = getAnnotation(ElidableMethodClass) flatMap { _.intArg(0) }
    def implicitNotFoundMsg = getAnnotation(ImplicitNotFoundClass) flatMap { _.stringArg(0) }

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
      !isMutable &&
      (!hasFlag(METHOD | BYNAMEPARAM) || hasFlag(STABLE)) &&
      !(tpe.isVolatile && !hasAnnotation(uncheckedStableClass))

    def isVirtualClass =
      hasFlag(DEFERRED) && isClass

    def isVirtualTrait =
      hasFlag(DEFERRED) && isTrait

    def isLiftedMethod = isMethod && hasFlag(LIFTED)
    def isCaseClass    = isClass && isCase

    /** Does this symbol denote the primary constructor of its enclosing class? */
    final def isPrimaryConstructor =
      isConstructor && owner.primaryConstructor == this

    /** Does this symbol denote an auxiliary constructor of its enclosing class? */
    final def isAuxiliaryConstructor =
      isConstructor && !isPrimaryConstructor

    /** Is this symbol a synthetic apply or unapply method in a companion object of a case class? */
    final def isCaseApplyOrUnapply =
      isMethod && isCase && isSynthetic

    /** Is this symbol a trait which needs an implementation class? */
    final def needsImplClass: Boolean =
      isTrait && (!isInterface || hasFlag(lateINTERFACE)) && !isImplClass

    /** Is this a symbol which exists only in the implementation class, not in its trait? */
    final def isImplOnly: Boolean =
      hasFlag(PRIVATE) ||
      (owner.isImplClass || owner.isTrait) &&
      ((hasFlag(notPRIVATE | LIFTED) && !hasFlag(ACCESSOR | SUPERACCESSOR | MODULE) || isConstructor) ||
       (hasFlag(LIFTED) && isModule && isMethod))

    /** Is this symbol a module variable?
     *  This used to have to test for MUTABLE to distinguish the overloaded
     *  MODULEVAR/SYNTHETICMETH flag, but now SYNTHETICMETH is gone.
     */
    final def isModuleVar = hasFlag(MODULEVAR)

    /** Is this symbol static (i.e. with no outer instance)? */
    final def isStatic: Boolean =
      hasFlag(STATIC) || isRoot || owner.isStaticOwner

    /** Is this symbol a static constructor? */
    final def isStaticConstructor: Boolean =
      isStaticMember && isClassConstructor

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
        case NullaryMethodType(ConstantType(_)) => true
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
      isClass && (isAnonOrRefinementClass || isLocal ||
                  !owner.isPackageClass && owner.isLocalClass)

/* code for fixing nested objects
    override final def isModuleClass: Boolean =
      super.isModuleClass && !isExpandedModuleClass
*/
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

    // Does not always work if the rawInfo is a SourcefileLoader, see comment
    // in "def coreClassesFirst" in Global.
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

// ------ owner attribute --------------------------------------------------------------

    def owner: Symbol = rawowner
    final def owner_=(owner: Symbol) {
      if (originalOwner contains this) ()
      else originalOwner(this) = rawowner

      rawowner = owner
    }
    private[Symbols] def flattenName(): Name = {
      // TODO: this assertion causes me a lot of trouble in the interpeter in situations
      // where everything proceeds smoothly if there's no assert.  I don't think calling "name"
      // on a symbol is the right place to throw fatal exceptions if things don't look right.
      // It really hampers exploration.
      assert(rawowner.isClass, "fatal: %s has non-class owner %s after flatten.".format(rawname + idString, rawowner))
      nme.flattenedName(rawowner.name, rawname)
    }

    def ownerChain: List[Symbol] = this :: owner.ownerChain
    def enclClassChain: List[Symbol] = {
      if (this eq NoSymbol) Nil
      else if (isClass && !isPackageClass) this :: owner.enclClassChain
      else owner.enclClassChain
    }

    def ownersIterator: Iterator[Symbol] = new Iterator[Symbol] {
      private var current = Symbol.this
      def hasNext = current ne NoSymbol
      def next = { val r = current; current = current.owner; r }
    }

    /** same as ownerChain contains sym, but more efficient, and
     *  with a twist for refinement classes. A refinement class
     *  has a transowner X if an of its parents has transowner X.
     */
    def hasTransOwner(sym: Symbol): Boolean = {
      var o = this
      while ((o ne sym) && (o ne NoSymbol)) o = o.owner
      (o eq sym) ||
      isRefinementClass && (info.parents exists (_.typeSymbol.hasTransOwner(sym)))
    }

// ------ name attribute --------------------------------------------------------------

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

        /** The name of the symbol before decoding, e.g. `\$eq\$eq` instead of `==`.
     */
    def encodedName: String = name.toString

    /** The decoded name of the symbol, e.g. `==` instead of `\$eq\$eq`.
     */
    def decodedName: String = stripLocalSuffix(NameTransformer.decode(encodedName))

    /** The encoded full path name of this symbol, where outer names and inner names
     *  are separated by `separator` characters.
     *  Never translates expansions of operators back to operator symbol.
     *  Never adds id.
     */
    final def fullName(separator: Char): String = stripLocalSuffix {
      if (isRoot || isRootPackage || this == NoSymbol) this.toString
      else if (owner.isEffectiveRoot) encodedName
      else owner.enclClass.fullName(separator) + separator + encodedName
    }

    private def stripLocalSuffix(s: String) = s stripSuffix nme.LOCAL_SUFFIX_STRING

    /** The encoded full path name of this symbol, where outer names and inner names
     *  are separated by periods.
     */
    final def fullName: String = fullName('.')

// ------ flags attribute --------------------------------------------------------------

    final def flags: Long = {
      val fs = rawflags & phase.flagMask
      (fs | ((fs & LateFlags) >>> LateShift)) & ~(fs >>> AntiShift)
    }
    final def flags_=(fs: Long) = rawflags = fs
    final def setFlag(mask: Long): this.type = { rawflags = rawflags | mask; this }
    final def resetFlag(mask: Long): this.type = { rawflags = rawflags & ~mask; this }
    final def getFlag(mask: Long): Long = flags & mask
    final def resetFlags { rawflags = rawflags & TopLevelCreationFlags }

    /** Does symbol have ANY flag in `mask` set? */
    final def hasFlag(mask: Long): Boolean = (flags & mask) != 0L

    /** Does symbol have ALL the flags in `mask` set? */
    final def hasAllFlags(mask: Long): Boolean = (flags & mask) == mask

    /** The class or term up to which this symbol is accessible,
     *  or RootClass if it is public.
     */
    def accessBoundary(base: Symbol): Symbol = {
      if (hasFlag(PRIVATE) || isLocal) owner
      else if (hasAccessBoundary && !phase.erasedTypes) privateWithin
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

    /** See comment in HasFlags for how privateWithin combines with flags.
     */
    private[this] var _privateWithin: Symbol = _
    def privateWithin = _privateWithin
    def privateWithin_=(sym: Symbol) { _privateWithin = sym }

    /** Does symbol have a private or protected qualifier set? */
    final def hasAccessBoundary = (privateWithin != null) && (privateWithin != NoSymbol)

// ------ info and type -------------------------------------------------------------------

    private[Symbols] var infos: TypeHistory = null

    /** Get type. The type of a symbol is:
     *  for a type symbol, the type corresponding to the symbol itself,
     *    @M you should use tpeHK for a type symbol with type parameters if
     *       the kind of the type need not be *, as tpe introduces dummy arguments
     *       to generate a type of kind *
     *  for a term symbol, its usual type
     */
    def tpe: Type = info

    /** Get type info associated with symbol at current phase, after
     *  ensuring that symbol is initialized (i.e. type is completed).
     */
    def info: Type = try {
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
//          activeLocks += 1
 //         lockedSyms += this
        }
        val current = phase
        try {
          phase = phaseOf(infos.validFrom)
          tp.complete(this)
        } finally {
          unlock()
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

    def info_=(info: Type) {
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
      validTo = if (info.isComplete) currentPeriod else NoPeriod
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
    final def cookJavaRawInfo() {
      if (hasFlag(TRIEDCOOKING)) return else setFlag(TRIEDCOOKING) // only try once...
      val oldInfo = info
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
          while ((phase.prev ne NoPhase) && phase.prev.keepsTypeParams) phase = phase.prev
          if (phase ne current) phase = phase.next
          if (settings.debug.value && settings.verbose.value && (phase ne current))
            log("checking unsafeTypeParams(" + this + ") at: " + current + " reading at: " + phase)
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
      if (isMonomorphicType)
        List()
      else {
        if (validTo == NoPeriod) {
          val current = phase
          try {
            phase = phaseOf(infos.validFrom)
            rawInfo.load(this)
          } finally {
            phase = current
          }
        }
        rawInfo.typeParams
      }

    /** The value parameter sections of this symbol.
     */
    def paramss: List[List[Symbol]] = info.paramss
    def hasParamWhich(cond: Symbol => Boolean) = paramss exists (_ exists cond)

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
              tp.typeSymbol.newAbstractType(sym.pos, sym.name.toTypeName).setInfo(sym.existentialBound)
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
         polyType(this.typeParams, TypeBounds.upper(this.classBound))
      else if (this.isAbstractType)
         this.info
      else if (this.isTerm)
         TypeBounds.upper(intersectionType(List(this.tpe, SingletonClass.tpe)))
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

    /**
     * Adds the interface scala.Serializable to the parents of a ClassInfoType.
     * Note that the tree also has to be updated accordingly.
     */
    def makeSerializable() {
      info match {
        case ci @ ClassInfoType(_, _, _) =>
          updateInfo(ci.copy(parents = ci.parents ::: List(SerializableClass.tpe)))
        case i =>
          abort("Only ClassInfoTypes can be made serializable: "+ i)
      }
    }

// ----- setters implemented in selected subclasses -------------------------------------

    def typeOfThis_=(tp: Type)       { throw new UnsupportedOperationException("typeOfThis_= inapplicable for " + this) }
    def sourceModule_=(sym: Symbol)  { throw new UnsupportedOperationException("sourceModule_= inapplicable for " + this) }
    def addChild(sym: Symbol)        { throw new UnsupportedOperationException("addChild inapplicable for " + this) }

// ----- annotations ------------------------------------------------------------

    private var rawannots: List[AnnotationInfoBase] = Nil
    def rawAnnotations = rawannots

    /* Used in namer to check whether annotations were already assigned or not */
    def hasAssignedAnnotations = rawannots.nonEmpty

    /** After the typer phase (before, look at the definition's Modifiers), contains
     *  the annotations attached to member a definition (class, method, type, field).
     */
    def annotations: List[AnnotationInfo] = {
      // .initialize: the type completer of the symbol parses the annotations,
      // see "def typeSig" in Namers
      val annots1 = initialize.rawannots map {
        case x: LazyAnnotationInfo  => x.annot()
        case x: AnnotationInfo      => x
      } filterNot (_.atp.isError)
      rawannots = annots1
      annots1
    }

    def setAnnotations(annots: List[AnnotationInfoBase]): this.type = {
      this.rawannots = annots
      this
    }

    def addAnnotation(annot: AnnotationInfo) {
      setAnnotations(annot :: this.rawannots)
    }

    /** Does this symbol have an annotation of the given class? */
    def hasAnnotation(cls: Symbol) =
      getAnnotation(cls).isDefined

    def getAnnotation(cls: Symbol): Option[AnnotationInfo] =
      annotations find (_.atp.typeSymbol == cls)

    /** Remove all annotations matching the given class. */
    def removeAnnotation(cls: Symbol): Unit =
      setAnnotations(annotations filterNot (_.atp.typeSymbol == cls))

// ------ comparisons ----------------------------------------------------------------

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
    final def isNumericSubClass(that: Symbol): Boolean =
      definitions.isNumericSubClass(this, that)

// ------ overloaded alternatives ------------------------------------------------------

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

// ------ cloneing -------------------------------------------------------------------

    /** A clone of this symbol */
    final def cloneSymbol: Symbol =
      cloneSymbol(owner)

    /** A clone of this symbol, but with given owner */
    final def cloneSymbol(owner: Symbol): Symbol = {
      val newSym = cloneSymbolImpl(owner)
      newSym.privateWithin = privateWithin
      newSym.setInfo(info.cloneInfo(newSym))
        .setFlag(this.rawflags).setAnnotations(this.annotations)
    }

    /** Internal method to clone a symbol's implementation without flags or type
     */
    def cloneSymbolImpl(owner: Symbol): Symbol

// ------ access to related symbols --------------------------------------------------

    /** The next enclosing class */
    def enclClass: Symbol = if (isClass) this else owner.enclClass

    /** The next enclosing method */
    def enclMethod: Symbol = if (isSourceMethod) this else owner.enclMethod

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
      val allWithFlag = info.decls.toList filter (_.isCaseAccessor)
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
      info.decls.toList filter (sym => !sym.isMethod && sym.isParamAccessor)

    /** The symbol accessed by this accessor (getter or setter) function. */
    final def accessed: Symbol = accessed(owner.info)

    /** The symbol accessed by this accessor function, but with given owner type */
    final def accessed(ownerTp: Type): Symbol = {
      assert(hasAccessorFlag)
      ownerTp.decl(nme.getterToLocal(if (isSetter) nme.setterToGetter(name) else name))
    }

    /** The module corresponding to this module class (note that this
     *  is not updated when a module is cloned), or NoSymbol if this is not a ModuleClass
     */
    def sourceModule: Symbol = NoSymbol

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

    /** If this is a lazy value, the lazy accessor; otherwise this symbol. */
    def lazyAccessorOrSelf: Symbol = if (isLazy) lazyAccessor else this

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

    /** The package class containing this symbol, or NoSymbol if there
     *  is not one. */
    def enclosingPackageClass: Symbol =
      if (this == NoSymbol) this else {
        var packSym = this.owner
        while (packSym != NoSymbol && !packSym.isPackageClass)
          packSym = packSym.owner
        packSym
      }

    /** The package containing this symbol, or NoSymbol if there
     *  is not one. */
    def enclosingPackage: Symbol = {
      val packSym = enclosingPackageClass
      if (packSym != NoSymbol) packSym.companionModule
      else packSym
    }

    /** Return the original enclosing method of this symbol. It should return
     *  the same thing as enclMethod when called before lambda lift,
     *  but it preserves the original nesting when called afterwards.
     */
    def originalEnclosingMethod: Symbol = {
      if (isMethod) this
      else {
        val owner = originalOwner.getOrElse(this, rawowner)
        if (isLocalDummy) owner.enclClass.primaryConstructor
        else owner.originalEnclosingMethod
      }
    }

    /** The method or class which logically encloses the current symbol.
     *  If the symbol is defined in the initialization part of a template
     *  this is the template's primary constructor, otherwise it is
     *  the physically enclosing method or class.
     *
     *  Example 1:
     *
     *  def f() { val x = { def g() = ...; g() } }
     *
     *  In this case the owner chain of `g' is `x', followed by `f' and
     *  `g.logicallyEnclosingMember == f`.
     *
     *  Example 2:
     *
     *  class C {
     *    def <init> = { ... }
     *    val x = { def g() = ...; g() } }
     *  }
     *
     *  In this case the owner chain of `g' is `x', followed by `C' but
     *  g.logicallyEnclosingMember is the primary constructor symbol `<init>'
     *  (or, for traits: `$init') of `C'.
     *
     */
    def logicallyEnclosingMember: Symbol =
      if (isLocalDummy) enclClass.primaryConstructor
      else if (isMethod || isClass) this
      else owner.logicallyEnclosingMember

    /** The top-level class containing this symbol */
    def toplevelClass: Symbol =
      if (owner.isPackageClass) {
        if (isClass) this else moduleClass
      } else owner.toplevelClass

    /** Is this symbol defined in the same scope and compilation unit as `that' symbol?
     */
    def isCoDefinedWith(that: Symbol) = (
      (this.rawInfo ne NoType) &&
      (this.owner == that.owner) && {
        !this.owner.isPackageClass ||
        (this.sourceFile eq null) ||
        (that.sourceFile eq null) ||
        (this.sourceFile == that.sourceFile) || {
          // recognize companion object in separate file and fail, else compilation
          // appears to succeed but highly opaque errors come later: see bug #1286
          if (this.sourceFile.path != that.sourceFile.path)
            throw InvalidCompanions(this, that)

          false
        }
      }
    )

    /** The internal representation of classes and objects:
     *
     *  class Foo is "the class" or sometimes "the plain class"
     * object Foo is "the module"
     * class Foo$ is "the module class" (invisible to the user: it implements object Foo)
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

    /** For a module or case factory: the class with the same name in the same package.
     *  For all others: NoSymbol
     *  Note: does not work for classes owned by methods, see Namers.companionClassOf
     *
     *  object Foo  .  companionClass -->  class Foo
     */
    final def companionClass: Symbol = {
      if (this != NoSymbol)
        flatOwnerInfo.decl(name.toTypeName).suchThat(_ isCoDefinedWith this)
      else NoSymbol
    }

    /** A helper method that factors the common code used the discover a
     *  companion module of a class. If a companion module exists, its symbol is
     *  returned, otherwise, `NoSymbol` is returned. The method assumes that
     *  `this` symbol has already been checked to be a class (using `isClass`).
     */
    private final def companionModule0: Symbol =
      flatOwnerInfo.decl(name.toTermName).suchThat(
        sym => sym.hasFlag(MODULE) && (sym isCoDefinedWith this) && !sym.isMethod)

    /** For a class: the module or case class factory with the same name in the same package.
     *  For all others: NoSymbol
     *  Note: does not work for modules owned by methods, see Namers.companionModuleOf
     *
     *  class Foo  .  companionModule -->  object Foo
     */
    final def companionModule: Symbol =
      if (isClass && !isRefinementClass) companionModule0
      else NoSymbol

    /** For a module: its linked class
     *  For a plain class: its linked module or case factory.
     *  Note: does not work for modules owned by methods, see Namers.companionSymbolOf
     *
     *  class Foo  <-- companionSymbol -->  object Foo
     */
    final def companionSymbol: Symbol =
      if (isTerm) companionClass
      else if (isClass) companionModule0
      else NoSymbol

    /** For a module class: its linked class
     *   For a plain class: the module class of its linked module.
     *
     *  class Foo  <-- linkedClassOfClass -->  class Foo$
     */
    final def linkedClassOfClass: Symbol =
      if (isModuleClass) companionClass else companionModule.moduleClass

    /**
     * Returns the rawInfo of the owner. If the current phase has flat classes,
     * it first applies all pending type maps to this symbol.
     *
     * assume this is the ModuleSymbol for B in the following definition:
     *   package p { class A { object B { val x = 1 } } }
     *
     * The owner after flatten is "package p" (see "def owner"). The flatten type map enters
     * symbol B in the decls of p. So to find a linked symbol ("object B" or "class B")
     * we need to apply flatten to B first. Fixes #2470.
     */
    private final def flatOwnerInfo: Type = {
      if (needsFlatClasses)
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

    /** Returns all symbols overriden by this symbol
     */
    final def allOverriddenSymbols: List[Symbol] =
      if (!owner.isClass) Nil
      else owner.ancestors map overriddenSymbol filter (_ != NoSymbol)

    /** Returns all symbols overridden by this symbol, plus all matching symbols
     *  defined in parents of the selftype
     */
    final def extendedOverriddenSymbols: List[Symbol] =
      if (!owner.isClass) Nil
      else owner.thisSym.ancestors map overriddenSymbol filter (_ != NoSymbol)

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
      base.info.decl(getterName) filter (_.hasAccessorFlag)
    }

    /** The setter of this value or getter definition, or NoSymbol if none exists */
    final def setter(base: Symbol): Symbol = setter(base, false)

    final def setter(base: Symbol, hasExpandedName: Boolean): Symbol = {
      var sname = nme.getterToSetter(nme.getterName(name))
      if (hasExpandedName) sname = nme.expandedSetterName(sname, base)
      base.info.decl(sname) filter (_.hasAccessorFlag)
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
        if (hasAccessorFlag && !isDeferred) {
          accessed.expandName(base)
        } else if (hasGetter) {
          getter(owner).expandName(base)
          setter(owner).expandName(base)
        }
        name = nme.expandedName(name, base)
        if (isType) name = name
      }
    }
/* code for fixing nested objects
    def expandModuleClassName() {
      name = newTypeName(name.toString + "$")
    }

    def isExpandedModuleClass: Boolean = name(name.length - 1) == '$'
*/
    def sourceFile: AbstractFile =
      if (isModule) moduleClass.sourceFile
      else toplevelClass.sourceFile

    def sourceFile_=(f: AbstractFile) {
      abort("sourceFile_= inapplicable for " + this)
    }

    /** If this is a sealed class, its known direct subclasses.
     *  Otherwise, the empty set.
     */
    def children: Set[Symbol] = Set()

    /** Recursively assemble all children of this symbol.
     */
    def sealedDescendants: Set[Symbol] = children.flatMap(_.sealedDescendants) + this

    def orElse[T](alt: => Symbol): Symbol = if (this ne NoSymbol) this else alt

// ------ toString -------------------------------------------------------------------

    /** A tag which (in the ideal case) uniquely identifies class symbols */
    final def tag = fullName.##

    /** The simple name of this Symbol */
    final def simpleName: Name = name

    /** The String used to order otherwise identical sealed symbols.
     *  This uses data which is stable across runs and variable classpaths
     *  (the initial Name) before falling back on id, which varies depending
     *  on exactly when a symbol is loaded.
     */
    final def sealedSortName = initName + "#" + id

    /** String representation of symbol's definition key word */
    final def keyString: String =
      if (isJavaInterface) "interface"
      else if (isTrait) "trait"
      else if (isClass) "class"
      else if (isType && !isParameter) "type"
      else if (isVariable) "var"
      else if (isPackage) "package"
      else if (isModule) "object"
      else if (isSourceMethod) "def"
      else if (isTerm && (!isParameter || isParamAccessor)) "val"
      else ""

    /** Accurate string representation of symbols' kind, suitable for developers. */
    final def accurateKindString: String =
      if (isPackage) "package"
      else if (isPackageClass) "package class"
      else if (isPackageObject) "package object"
      else if (isPackageObjectClass) "package object class"
      else if (isRefinementClass) "refinement class"
      else if (isModule) "module"
      else if (isModuleClass) "module class"
      else sanitizedKindString

    /** String representation of symbol's kind, suitable for the masses. */
    private def sanitizedKindString: String =
      if (isPackage || isPackageClass) "package"
      else if (isModule || isModuleClass) "object"
      else if (isAnonymousClass) "anonymous class"
      else if (isRefinementClass) ""
      else if (isTrait) "trait"
      else if (isClass) "class"
      else if (isType) "type"
      else if (isTerm && isLazy) "lazy value"
      else if (isVariable) "variable"
      else if (isClassConstructor) "constructor"
      else if (isSourceMethod) "method"
      else if (isTerm) "value"
      else ""

    final def kindString: String =
      if (settings.debug.value) accurateKindString
      else sanitizedKindString

    /** If the name of the symbol's owner should be used when you care about
     *  seeing an interesting name: in such cases this symbol is e.g. a method
     *  parameter with a synthetic name, a constructor named "this", an object
     *  "package", etc.  The kind string, if non-empty, will be phrased relative
     *  to the name of the owner.
     */
    def hasMeaninglessName = (
         isSetterParameter          // x$1
      || isClassConstructor         // this
      || isPackageObject            // package
      || isPackageObjectClass       // package$
      || isRefinementClass          // <refinement>
    )

    /** String representation of symbol's simple name.
     *  If !settings.debug translates expansions of operators back to operator symbol.
     *  E.g. $eq => =.
     *  If settings.uniqid, adds id.
     */
    def nameString = decodedName + idString

    /** If settings.uniqid is set, the symbol's id, else "" */
    final def idString = if (settings.uniqid.value) "#"+id else ""

    /** String representation, including symbol's kind e.g., "class Foo", "method Bar".
     *  If hasMeaninglessName is true, uses the owner's name to disambiguate identity.
     */
    override def toString = compose(
      kindString,
      if (hasMeaninglessName) owner.nameString else nameString
    )

    /** String representation of location.
     */
    def ownsString = {
      val owns = owner.skipPackageObject
      if (owns.isClass && !owns.printWithoutPrefix && !isScalaPackageClass) "" + owns
      else ""
    }

    /** String representation of location, plus a preposition.  Doesn't do much,
     *  for backward compatibility reasons.
     */
    def locationString = ownsString match {
      case ""   => ""
      case s    => " in " + s
    }
    def fullLocationString = toString + locationString

    /** String representation of symbol's definition following its name */
    final def infoString(tp: Type): String = {
      def typeParamsString: String = tp match {
        case PolyType(tparams, _) if tparams.nonEmpty =>
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
          case NullaryMethodType(res) =>
            infoString(res)
          case MethodType(params, res) =>
            params.map(_.defString).mkString("(", ",", ")") + infoString(res)
          case _ =>
            ": " + tp
        }
    }

    def infosString = infos.toString()

    def hasFlagsToString(mask: Long): String = flagsToString(
      flags & mask,
      if (hasAccessBoundary) privateWithin.toString else ""
    )

    /** String representation of symbol's variance */
    def varianceString: String =
      if (variance == 1) "+"
      else if (variance == -1) "-"
      else ""

    def defaultFlagMask =
      if (settings.debug.value) -1L
      else if (owner.isRefinementClass) ExplicitFlags & ~OVERRIDE
      else ExplicitFlags

    def defaultFlagString = hasFlagsToString(defaultFlagMask)

    /** String representation of symbol's definition */
    def defString = compose(
      defaultFlagString,
      keyString,
      varianceString + nameString + (
        if (hasRawInfo) infoString(rawInfo) else "<_>"
      )
    )

    /** Concatenate strings separated by spaces */
    private def compose(ss: String*) = ss filter (_ != "") mkString " "

    def isSingletonExistential =
      nme.isSingletonName(name) && (info.bounds.hi.typeSymbol isSubClass SingletonClass)

    /** String representation of existentially bound variable */
    def existentialToString =
      if (isSingletonExistential && !settings.debug.value)
        "val " + nme.dropSingletonName(name) + ": " + dropSingletonType(info.bounds.hi)
      else defString
  }

  /** A class for term symbols */
  class TermSymbol(initOwner: Symbol, initPos: Position, initName: TermName)
  extends Symbol(initOwner, initPos, initName) {
    final override def isTerm = true

    override def name: TermName = super.name
    privateWithin = NoSymbol

    var referenced: Symbol = NoSymbol

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
      assert(!alias.isOverloaded, alias)
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
      assert(isLazy && (referenced == NoSymbol || referenced == sym), this)
      referenced = sym
      this
    }

    override def lazyAccessor: Symbol = {
      assert(isLazy, this)
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

      if (isJavaDefined)
        cook(this)
      else if (hasFlag(OVERLOADED))
        for (sym2 <- alternatives)
          if (sym2 hasFlag JAVA)
            cook(sym2)
    }
  }

  /** A class for module symbols */
  class ModuleSymbol(initOwner: Symbol, initPos: Position, initName: TermName)
  extends TermSymbol(initOwner, initPos, initName) {
    private var flatname: TermName = null
    // This method could use a better name from someone clearer on what the condition expresses.
    private def isFlatAdjusted = !isMethod && needsFlatClasses

    override def owner: Symbol =
      if (isFlatAdjusted) rawowner.owner
      else rawowner

    override def name: TermName =
      if (isFlatAdjusted) {
        if (flatname == null)
          flatname = flattenName().toTermName

        flatname
      } else rawname

    override def cloneSymbolImpl(owner: Symbol): Symbol =
      new ModuleSymbol(owner, pos, name).copyAttrsFrom(this)
  }

  /** A class for method symbols */
  class MethodSymbol(initOwner: Symbol, initPos: Position, initName: TermName)
  extends TermSymbol(initOwner, initPos, initName) {
    private var mtpePeriod = NoPeriod
    private var mtpePre: Type = _
    private var mtpeResult: Type = _
    private var mtpeInfo: Type = _

    override def cloneSymbolImpl(owner: Symbol): Symbol =
      new MethodSymbol(owner, pos, name).copyAttrsFrom(this)

    def typeAsMemberOf(pre: Type): Type = {
      if (mtpePeriod == currentPeriod) {
        if ((mtpePre eq pre) && (mtpeInfo eq info)) return mtpeResult
      } else if (isValid(mtpePeriod)) {
        mtpePeriod = currentPeriod
        if ((mtpePre eq pre) && (mtpeInfo eq info)) return mtpeResult
      }
      val res = pre.computeMemberType(this)
      mtpePeriod = currentPeriod
      mtpePre = pre
      mtpeInfo = info
      mtpeResult = res
      res
    }
  }

  /** A class of type symbols. Alias and abstract types are direct instances
   *  of this class. Classes are instances of a subclass.
   */
  class TypeSymbol(initOwner: Symbol, initPos: Position, initName: TypeName)
  extends Symbol(initOwner, initPos, initName) {
    privateWithin = NoSymbol
    private var tyconCache: Type = null
    private var tyconRunId = NoRunId
    private var tpeCache: Type = _
    private var tpePeriod = NoPeriod

    override def name: TypeName = super.name.asInstanceOf[TypeName]
    final override def isType = true
    override def isNonClassType = true
    override def isAbstractType = isDeferred
    override def isAliasType = !isDeferred

    private def newTypeRef(targs: List[Type]) = {
      val pre = if (hasFlag(PARAM | EXISTENTIAL)) NoPrefix else owner.thisType
      typeRef(pre, this, targs)
    }

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
          tpeCache = newTypeRef(targs)
        }
      }
      assert(tpeCache ne null/*, "" + this + " " + phase*/)//debug
      tpeCache
    }

    // needed for experimental code for early types as type parameters
    // def refreshType() { tpePeriod = NoPeriod }

    override def typeConstructor: Type = {
      if ((tyconCache eq null) || tyconRunId != currentRunId) {
        tyconCache = newTypeRef(Nil)
        tyconRunId = currentRunId
      }
      assert(tyconCache ne null)
      tyconCache
    }

    override def info_=(tp: Type) {
      tpePeriod = NoPeriod
      tyconCache = null
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
      // don't require isJavaDefined, since T in the above example does not have that flag
      val tpe1 = rawToExistential(info)
      // println("cooking type: "+ this +": "+ info +" to "+ tpe1)
      if (tpe1 ne info) {
        setInfo(tpe1)
      }
    }

    def cloneSymbolImpl(owner: Symbol): Symbol =
      new TypeSymbol(owner, pos, name)  //.toTypeName)

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
  class TypeSkolem(initOwner: Symbol, initPos: Position, initName: TypeName, origin: AnyRef)
  extends TypeSymbol(initOwner, initPos, initName) {

    /** The skolemization level in place when the skolem was constructed */
    val level = skolemizationLevel

    final override def isSkolem = true

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
  class ClassSymbol(initOwner: Symbol, initPos: Position, initName: TypeName)
  extends TypeSymbol(initOwner, initPos, initName) {

    private var source: AbstractFile = null
    private var thissym: Symbol = this

    final override def isClass = true
    final override def isNonClassType = false
    final override def isAbstractType = false
    final override def isAliasType = false

    override def sourceFile =
      if (owner.isPackageClass) source
      else super.sourceFile
    override def sourceFile_=(f: AbstractFile) { source = f }

    override def reset(completer: Type) {
      super.reset(completer)
      thissym = this
    }

    private var flatname: TypeName = null

    override def owner: Symbol =
      if (needsFlatClasses) rawowner.owner
      else rawowner

    override def name: TypeName =
      if (needsFlatClasses) {
        if (flatname == null)
          flatname = flattenName().toTypeName
        flatname
      }
      else rawname.asInstanceOf[TypeName]

    private var thisTypeCache: Type = _
    private var thisTypePeriod = NoPeriod

    private var typeOfThisCache: Type = _
    private var typeOfThisPeriod = NoPeriod

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
    override def typeOfThis: Type = {
      if (getFlag(MODULE | IMPLCLASS) == MODULE.toLong && owner != NoSymbol) {
        val period = typeOfThisPeriod
        if (period != currentPeriod) {
          typeOfThisPeriod = currentPeriod
          if (!isValid(period))
            typeOfThisCache = singleType(owner.thisType, sourceModule)
        }
        typeOfThisCache
      }
      else thissym.tpe
    }

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
    override def children = childSet
    override def addChild(sym: Symbol) { childSet = childSet + sym }

    incCounter(classSymbolCount)
  }

  /** A class for module class symbols
   *  Note: Not all module classes are of this type; when unpickled, we get
   *  plain class symbols!
   */
  class ModuleClassSymbol(owner: Symbol, pos: Position, name: TypeName)
  extends ClassSymbol(owner, pos, name) {
    private var module: Symbol = null
    def this(module: TermSymbol) = {
      this(module.owner, module.pos, module.name.toTypeName)
      setFlag(module.getFlag(ModuleToClassFlags) | MODULE | FINAL)
      sourceModule = module
    }
    override def sourceModule = module
    private var implicitMembersCacheValue: List[Symbol] = List()
    private var implicitMembersCacheKey: Type = NoType
    def implicitMembers: List[Symbol] = {
      val tp = info
      if (implicitMembersCacheKey ne tp) {
        implicitMembersCacheKey = tp
        implicitMembersCacheValue = tp.implicitMembers
      }
      implicitMembersCacheValue
    }
    override def sourceModule_=(module: Symbol) { this.module = module }
  }

  /** An object representing a missing symbol */
  object NoSymbol extends Symbol(null, NoPosition, nme.NO_NAME) {
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
    override def originalEnclosingMethod = this
  }

  def cloneSymbols[T <: Symbol](syms: List[T]): List[T] = {
    val syms1 = syms map (_.cloneSymbol.asInstanceOf[T])
    for (sym1 <- syms1) sym1.setInfo(sym1.info.substSym(syms, syms1))
    syms1
  }

  def cloneSymbols[T <: Symbol](syms: List[T], owner: Symbol): List[T] = {
    val syms1 = syms map (_.cloneSymbol(owner).asInstanceOf[T])
    for (sym1 <- syms1) sym1.setInfo(sym1.info.substSym(syms, syms1))
    syms1
  }

  /** An exception for cyclic references of symbol definitions */
  case class CyclicReference(sym: Symbol, info: Type)
  extends TypeError("illegal cyclic reference involving " + sym) {
    // printStackTrace() // debug
  }

  case class InvalidCompanions(sym1: Symbol, sym2: Symbol)
  extends Throwable("Companions '" + sym1 + "' and '" + sym2 + "' must be defined in same file") {
      override def toString = getMessage
  }

  /** A class for type histories */
  private sealed case class TypeHistory(var validFrom: Period, info: Type, prev: TypeHistory) {
    assert((prev eq null) || phaseId(validFrom) > phaseId(prev.validFrom), this)
    assert(validFrom != NoPeriod)
    override def toString() =
      "TypeHistory(" + phaseOf(validFrom)+":"+runId(validFrom) + "," + info + "," + prev + ")"
  }
}

 /* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package reflect
package internal

import scala.collection.{ mutable, immutable }
import scala.collection.mutable.ListBuffer
import util.{ Statistics, shortClassOfInstance }
import Flags._
import scala.annotation.tailrec
import scala.reflect.io.{ AbstractFile, NoAbstractFile }
import Variance._

trait Symbols extends api.Symbols { self: SymbolTable =>
  import definitions._
  import SymbolsStats._

  protected var ids = 0

  protected def nextId() = { ids += 1; ids }

  /** Used for deciding in the IDE whether we can interrupt the compiler */
  //protected var activeLocks = 0

  /** Used for debugging only */
  //protected var lockedSyms = scala.collection.immutable.Set[Symbol]()

  /** Used to keep track of the recursion depth on locked symbols */
  private var _recursionTable = immutable.Map.empty[Symbol, Int]
  def recursionTable = _recursionTable
  def recursionTable_=(value: immutable.Map[Symbol, Int]) = _recursionTable = value

  private var existentialIds = 0
  protected def nextExistentialId() = { existentialIds += 1; existentialIds }
  protected def freshExistentialName(suffix: String) = newTypeName("_" + nextExistentialId() + suffix)

  // Set the fields which point companions at one another.  Returns the module.
  def connectModuleToClass(m: ModuleSymbol, moduleClass: ClassSymbol): ModuleSymbol = {
    moduleClass.sourceModule = m
    m setModuleClass moduleClass
    m
  }

  /** Create a new free term.  Its owner is NoSymbol.
   */
  def newFreeTermSymbol(name: TermName, value: => Any, flags: Long = 0L, origin: String): FreeTermSymbol =
    new FreeTermSymbol(name, value, origin) initFlags flags

  /** Create a new free type.  Its owner is NoSymbol.
   */
  def newFreeTypeSymbol(name: TypeName, flags: Long = 0L, origin: String): FreeTypeSymbol =
    new FreeTypeSymbol(name, origin) initFlags flags

  /**
   * This map stores the original owner the first time the owner of a symbol is re-assigned.
   * The original owner of a symbol is needed in some places in the backend. Ideally, owners should
   * be versioned like the type history.
   */
  private val originalOwnerMap = perRunCaches.newMap[Symbol, Symbol]()

  // TODO - don't allow the owner to be changed without checking invariants, at least
  // when under some flag. Define per-phase invariants for owner/owned relationships,
  // e.g. after flatten all classes are owned by package classes, there are lots and
  // lots of these to be declared (or more realistically, discovered.)
  // could be private since 2.11.6, but left protected to avoid potential breakages (eg ensime)
  protected def saveOriginalOwner(sym: Symbol): Unit = {
    // some synthetic symbols have NoSymbol as owner initially
    if (sym.owner != NoSymbol) {
      if (originalOwnerMap contains sym) ()
      else defineOriginalOwner(sym, sym.rawowner)
    }
  }

  def defineOriginalOwner(sym: Symbol, owner: Symbol): Unit = {
    originalOwnerMap(sym) = owner
  }

  def symbolOf[T: WeakTypeTag]: TypeSymbol = weakTypeOf[T].typeSymbolDirect.asType

  abstract class SymbolContextApiImpl extends SymbolApi {
    this: Symbol =>

    def isFreeTerm: Boolean = false
    def asFreeTerm: FreeTermSymbol = throw new ScalaReflectionException(s"$this is not a free term")
    def isFreeType: Boolean = false
    def asFreeType: FreeTypeSymbol = throw new ScalaReflectionException(s"$this is not a free type")

    def isExistential: Boolean = this.isExistentiallyBound
    def isParamWithDefault: Boolean = this.hasDefault
    // `isByNameParam` is only true for a call-by-name parameter of a *method*,
    // an argument of the primary constructor seen in the class body is excluded by `isValueParameter`
    def isByNameParam: Boolean = this.isValueParameter && (this hasFlag BYNAMEPARAM)
    def isImplementationArtifact: Boolean = (this hasFlag BRIDGE) || (this hasFlag VBRIDGE) || (this hasFlag ARTIFACT)
    def isJava: Boolean = isJavaDefined
    def isVal: Boolean = isTerm && !isModule && !isMethod && !isMutable
    def isVar: Boolean = isTerm && !isModule && !isMethod && !isLazy && isMutable
    def isAbstract: Boolean = isAbstractClass || isDeferred || isAbstractType
    def isPrivateThis = (this hasFlag PRIVATE) && (this hasFlag LOCAL)
    def isProtectedThis = (this hasFlag PROTECTED) && (this hasFlag LOCAL)

    def newNestedSymbol(name: Name, pos: Position, newFlags: Long, isClass: Boolean): Symbol = name match {
      case n: TermName => newTermSymbol(n, pos, newFlags)
      case n: TypeName => if (isClass) newClassSymbol(n, pos, newFlags) else newNonClassSymbol(n, pos, newFlags)
    }

    def knownDirectSubclasses = {
      // See `getFlag` to learn more about the `isThreadsafe` call in the body of this method.
      if (!isCompilerUniverse && !isThreadsafe(purpose = AllOps)) initialize
      children
    }

    def selfType = {
      // See `getFlag` to learn more about the `isThreadsafe` call in the body of this method.
      if (!isCompilerUniverse && !isThreadsafe(purpose = AllOps)) initialize
      typeOfThis
    }

    def baseClasses                       = info.baseClasses
    def module                            = sourceModule
    def thisPrefix: Type                  = thisType
    def superPrefix(supertpe: Type): Type = SuperType(thisType, supertpe)

    // These two methods used to call fullyInitializeSymbol on `this`.
    //
    // The only positive effect of that is, to the best of my knowledge, convenient printing
    // (if you print a signature of the symbol that's not fully initialized,
    // you might end up with weird <?>'s in value/type params)
    //
    // Another effect is obviously full initialization of that symbol,
    // but that one shouldn't be necessary from the public API standpoint,
    // because everything that matters auto-initializes at runtime,
    // and auto-initialization at compile-time is anyway dubious
    // (I've had spurious cyclic refs caused by calling typeSignature
    // that initialized parent, which was in the middle of initialization).
    //
    // Given that and also given the pressure of being uniform with info and infoIn,
    // I've removed calls to fullyInitializeSymbol from typeSignature and typeSignatureIn,
    // injected fullyInitializeSymbol in showDecl, and injected fullyInitializeType in runtime Type.toString
    // (the latter will make things a bit harder to debug in runtime universe, because
    // toString might now very rarely cause cyclic references, but we also have showRaw that doesn't do initialization).
    //
    // Auto-initialization in runtime Type.toString is one of the examples of why a cake-based design
    // isn't a very good idea for reflection API. Sometimes we want to same pretty name for both a compiler-facing
    // and a user-facing API that should have different behaviors (other examples here include isPackage, isCaseClass, etc).
    // Within a cake it's fundamentally impossible to achieve that.
    def typeSignature: Type               = info
    def typeSignatureIn(site: Type): Type = site memberInfo this

    def toType: Type = tpe
    def toTypeIn(site: Type): Type = site.memberType(this)
    def toTypeConstructor: Type = typeConstructor
    def setAnnotations(annots: AnnotationInfo*): this.type = { setAnnotations(annots.toList); this }

    def getter: Symbol = getterIn(owner)
    def setter: Symbol = setterIn(owner)

    def companion: Symbol = {
      if (isModule && !hasPackageFlag) companionSymbol
      else if (isModuleClass && !isPackageClass) sourceModule.companionSymbol
      else if (isClass && !isModuleClass && !isPackageClass) companionSymbol
      else NoSymbol
    }

    def infoIn(site: Type): Type = typeSignatureIn(site)
    def overrides: List[Symbol] = allOverriddenSymbols
    def paramLists: List[List[Symbol]] = paramss
  }

  private[reflect] case class SymbolKind(accurate: String, sanitized: String, abbreviation: String)

  /** The class for all symbols */
  abstract class Symbol protected[Symbols] (initOwner: Symbol, initPos: Position, initName: Name)
          extends SymbolContextApiImpl
             with HasFlags
             with Annotatable[Symbol]
             with Attachable {
    // makes sure that all symbols that runtime reflection deals with are synchronized
    private def isSynchronized = this.isInstanceOf[scala.reflect.runtime.SynchronizedSymbols#SynchronizedSymbol]
    private def isAprioriThreadsafe = isThreadsafe(AllOps)
    assert(isCompilerUniverse || isSynchronized || isAprioriThreadsafe, s"unsafe symbol $initName (child of $initOwner) in runtime reflection universe")

    type AccessBoundaryType = Symbol
    type AnnotationType     = AnnotationInfo

    // TODO - don't allow names to be renamed in this unstructured fashion.
    // Rename as little as possible.  Enforce invariants on all renames.
    type TypeOfClonedSymbol >: Null <: Symbol { type NameType = Symbol.this.NameType }

    // Abstract here so TypeSymbol and TermSymbol can have a private[this] field
    // with the proper specific type.
    def rawname: NameType
    def name: NameType
    def name_=(n: Name): Unit = {
      if (shouldLogAtThisPhase) {
        def msg = s"In $owner, renaming $name -> $n"
        if (isSpecialized) debuglog(msg) else log(msg)
      }
    }
    def asNameType(n: Name): NameType

    // Syncnote: need not be protected, as only assignment happens in owner_=, which is not exposed to api
    // The null check is for NoSymbol, which can't pass a reference to itself to the constructor and also
    // can't call owner_= due to an assertion it contains.
    private[this] var _rawowner = if (initOwner eq null) this else initOwner
    private[this] var _rawflags: Long = _

    def rawowner = _rawowner
    def rawflags = _rawflags

    rawatt = initPos

    val id = nextId() // identity displayed when -uniqid
    //assert(id != 3390, initName)

    private[this] var _validTo: Period = NoPeriod

    if (traceSymbolActivity)
      traceSymbols.recordNewSymbol(this)

    def validTo = _validTo
    def validTo_=(x: Period) { _validTo = x}

    def setName(name: Name): this.type = { this.name = asNameType(name) ; this }

    // Update the surrounding scopes
    protected[this] def changeNameInOwners(name: Name) {
      if (owner.isClass) {
        var ifs = owner.infos
        while (ifs != null) {
          ifs.info.decls.rehash(this, name)
          ifs = ifs.prev
        }
      }
    }

    def rawFlagString(mask: Long): String = calculateFlagString(rawflags & mask)
    def rawFlagString: String             = rawFlagString(flagMask)
    def debugFlagString: String           = flagString(AllFlags)

    /** String representation of symbol's variance */
    def varianceString: String = variance.symbolicString

    override def flagMask =
      if (settings.debug && !isAbstractType) AllFlags
      else if (owner.isRefinementClass) ExplicitFlags & ~OVERRIDE
      else ExplicitFlags

    // make the error message more googlable
    def flagsExplanationString =
      if (isGADTSkolem) " (this is a GADT skolem)"
      else ""

    def shortSymbolClass = shortClassOfInstance(this)
    def symbolCreationString: String = (
      "%s%25s | %-40s | %s".format(
        if (settings.uniqid) "%06d | ".format(id) else "",
        shortSymbolClass,
        name.decode + " in " + owner,
        rawFlagString
      )
    )

// ------ creators -------------------------------------------------------------------

    final def newValue(name: TermName, pos: Position = NoPosition, newFlags: Long = 0L): TermSymbol =
      newTermSymbol(name, pos, newFlags)
    final def newVariable(name: TermName, pos: Position = NoPosition, newFlags: Long = 0L): TermSymbol =
      newTermSymbol(name, pos, MUTABLE | newFlags)
    final def newValueParameter(name: TermName, pos: Position = NoPosition, newFlags: Long = 0L): TermSymbol =
      newTermSymbol(name, pos, PARAM | newFlags)

    /** Create local dummy for template (owner of local blocks) */
    final def newLocalDummy(pos: Position): TermSymbol =
      newTermSymbol(nme.localDummyName(this), pos) setInfo NoType
    final def newMethod(name: TermName, pos: Position = NoPosition, newFlags: Long = 0L): MethodSymbol =
      createMethodSymbol(name, pos, METHOD | newFlags)
    final def newMethodSymbol(name: TermName, pos: Position = NoPosition, newFlags: Long = 0L): MethodSymbol =
      createMethodSymbol(name, pos, METHOD | newFlags)
    final def newLabel(name: TermName, pos: Position = NoPosition): MethodSymbol =
      newMethod(name, pos, LABEL)

    /** Propagates ConstrFlags (JAVA, specifically) from owner to constructor. */
    final def newConstructor(pos: Position, newFlags: Long = 0L): MethodSymbol =
      newMethod(nme.CONSTRUCTOR, pos, getFlag(ConstrFlags) | newFlags)

    /** Static constructor with info set. */
    def newStaticConstructor(pos: Position): MethodSymbol =
      newConstructor(pos, STATIC) setInfo UnitTpe

    /** Instance constructor with info set. */
    def newClassConstructor(pos: Position): MethodSymbol =
      newConstructor(pos) setInfo MethodType(Nil, this.tpe)

    def newLinkedModule(clazz: Symbol, newFlags: Long = 0L): ModuleSymbol = {
      val m = newModuleSymbol(clazz.name.toTermName, clazz.pos, MODULE | newFlags)
      connectModuleToClass(m, clazz.asInstanceOf[ClassSymbol])
    }
    final def newModule(name: TermName, pos: Position = NoPosition, newFlags0: Long = 0L): ModuleSymbol = {
      val newFlags = newFlags0 | MODULE
      val m = newModuleSymbol(name, pos, newFlags)
      val clazz = newModuleClass(name.toTypeName, pos, newFlags & ModuleToClassFlags)
      connectModuleToClass(m, clazz)
    }

    final def newPackage(name: TermName, pos: Position = NoPosition, newFlags: Long = 0L): ModuleSymbol = {
      assert(name == nme.ROOT || isPackageClass, this)
      newModule(name, pos, PackageFlags | newFlags)
    }

    final def newThisSym(name: TermName = nme.this_, pos: Position = NoPosition): TermSymbol =
      newTermSymbol(name, pos, SYNTHETIC)

    final def newImport(pos: Position): TermSymbol =
      newTermSymbol(nme.IMPORT, pos)

    def newModuleVarSymbol(accessor: Symbol): TermSymbol = {
      val newName  = nme.moduleVarName(accessor.name.toTermName)
      val newFlags = MODULEVAR | ( if (this.isClass) PrivateLocal | SYNTHETIC else 0 )
      val newInfo  = accessor.tpe.finalResultType
      val mval     = newVariable(newName, accessor.pos.focus, newFlags.toLong) addAnnotation VolatileAttr

      if (this.isClass)
        mval setInfoAndEnter newInfo
      else
        mval setInfo newInfo
    }

    final def newModuleSymbol(name: TermName, pos: Position = NoPosition, newFlags: Long = 0L): ModuleSymbol =
      newTermSymbol(name, pos, newFlags).asInstanceOf[ModuleSymbol]

    final def newModuleAndClassSymbol(name: Name, pos: Position, flags0: FlagSet): (ModuleSymbol, ClassSymbol) = {
      val flags = flags0 | MODULE
      val m = newModuleSymbol(name.toTermName, pos, flags)
      val c = newModuleClass(name.toTypeName, pos, flags & ModuleToClassFlags)
      connectModuleToClass(m, c)
      (m, c)
    }

    final def newModuleClassSymbol(name: TypeName, pos: Position = NoPosition, newFlags: Long = 0L): ModuleClassSymbol =
      newClassSymbol(name, pos, newFlags).asInstanceOf[ModuleClassSymbol]

    final def newTypeSkolemSymbol(name: TypeName, origin: AnyRef, pos: Position = NoPosition, newFlags: Long = 0L): TypeSkolem =
      createTypeSkolemSymbol(name, origin, pos, newFlags)

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
    final def newOverloaded(pre: Type, alternatives: List[Symbol]): TermSymbol = (
      newTermSymbol(alternatives.head.name.toTermName, alternatives.head.pos, OVERLOADED)
        setInfo OverloadedType(pre, alternatives)
    )

    final def newErrorValue(name: TermName): TermSymbol =
      newTermSymbol(name, pos, SYNTHETIC | IS_ERROR) setInfo ErrorType

    /** Symbol of a type definition  type T = ...
     */
    final def newAliasType(name: TypeName, pos: Position = NoPosition, newFlags: Long = 0L): AliasTypeSymbol =
      createAliasTypeSymbol(name, pos, newFlags)

    /** Symbol of an abstract type  type T >: ... <: ...
     */
    final def newAbstractType(name: TypeName, pos: Position = NoPosition, newFlags: Long = 0L): AbstractTypeSymbol =
      createAbstractTypeSymbol(name, pos, DEFERRED | newFlags)

    /** Symbol of a type parameter
     */
    final def newTypeParameter(name: TypeName, pos: Position = NoPosition, newFlags: Long = 0L): TypeSymbol =
      newAbstractType(name, pos, PARAM | newFlags)

// is defined in SymbolCreations
//    final def newTypeSymbol(name: TypeName, pos: Position = NoPosition, newFlags: Long = 0L): TypeSymbol =
//      (if ((newFlags & DEFERRED) != 0) new AbstractTypeSymbol(this, pos, name)
//       else new AbstractTypeSymbol(this, pos, name)) setFlag newFlags

    /** Symbol of an existential type T forSome { ... }
     */
    final def newExistential(name: TypeName, pos: Position = NoPosition, newFlags: Long = 0L): TypeSymbol =
      newAbstractType(name, pos, EXISTENTIAL | newFlags)

    private def freshNamer: () => TermName = {
      var cnt = 0
      () => { cnt += 1; nme.syntheticParamName(cnt) }
    }

    /** Synthetic value parameters when parameter symbols are not available.
     *  Calling this method multiple times will re-use the same parameter names.
     */
    final def newSyntheticValueParams(argtypes: List[Type]): List[TermSymbol] =
      newSyntheticValueParams(argtypes, freshNamer)

    final def newSyntheticValueParams(argtypes: List[Type], freshName: () => TermName): List[TermSymbol] =
      argtypes map (tp => newSyntheticValueParam(tp, freshName()))

    /** Synthetic value parameter when parameter symbol is not available.
     *  Calling this method multiple times will re-use the same parameter name.
     */
    final def newSyntheticValueParam(argtype: Type, name: TermName = nme.syntheticParamName(1)): TermSymbol =
      newValueParameter(name, owner.pos.focus, SYNTHETIC) setInfo argtype

    def newSyntheticTypeParam(name: String, newFlags: Long): TypeSymbol = newTypeParameter(newTypeName(name), NoPosition, newFlags) setInfo TypeBounds.empty
    def newSyntheticTypeParams(num: Int): List[TypeSymbol]              = (0 until num).toList map (n => newSyntheticTypeParam("T" + n, 0L))

    /** Create a new existential type skolem with this symbol its owner,
     *  based on the given symbol and origin.
     */
    def newExistentialSkolem(basis: Symbol, origin: AnyRef): TypeSkolem =
      newExistentialSkolem(basis.name.toTypeName, basis.info, basis.flags, basis.pos, origin)

    /** Create a new existential type skolem with this symbol its owner, and the given other properties.
     */
    def newExistentialSkolem(name: TypeName, info: Type, flags: Long, pos: Position, origin: AnyRef): TypeSkolem = {
      val skolem = newTypeSkolemSymbol(name.toTypeName, origin, pos, (flags | EXISTENTIAL) & ~PARAM)
      skolem setInfo (info cloneInfo skolem)
    }

    // don't test directly -- use isGADTSkolem
    // used to single out a gadt skolem symbol in deskolemizeGADT
    // gadtskolems are created in adaptConstrPattern and removed at the end of typedCase
    final protected[Symbols] def GADT_SKOLEM_FLAGS = CASEACCESSOR | SYNTHETIC

    // flags set up to maintain TypeSkolem's invariant: origin.isInstanceOf[Symbol] == !hasFlag(EXISTENTIAL)
    // GADT_SKOLEM_FLAGS (== CASEACCESSOR | SYNTHETIC) used to single this symbol out in deskolemizeGADT
    // TODO: it would be better to allocate a new bit in the flag long for GADTSkolem rather than OR'ing together CASEACCESSOR | SYNTHETIC
    def newGADTSkolem(name: TypeName, origin: Symbol, info: Type): TypeSkolem =
      newTypeSkolemSymbol(name, origin, origin.pos, origin.flags & ~(EXISTENTIAL | PARAM) | GADT_SKOLEM_FLAGS) setInfo info

    final def freshExistential(suffix: String): TypeSymbol =
      newExistential(freshExistentialName(suffix), pos)

    /** Type skolems are type parameters ''seen from the inside''
     *  Assuming a polymorphic method m[T], its type is a PolyType which has a TypeParameter
     *  with name `T` in its typeParams list. While type checking the parameters, result type and
     *  body of the method, there's a local copy of `T` which is a TypeSkolem.
     */
    final def newTypeSkolem: TypeSkolem =
      owner.newTypeSkolemSymbol(name.toTypeName, this, pos, flags)

    final def newClass(name: TypeName, pos: Position = NoPosition, newFlags: Long = 0L): ClassSymbol =
      newClassSymbol(name, pos, newFlags)

    /** A new class with its info set to a ClassInfoType with given scope and parents. */
    def newClassWithInfo(name: TypeName, parents: List[Type], scope: Scope, pos: Position = NoPosition, newFlags: Long = 0L): ClassSymbol = {
      val clazz = newClass(name, pos, newFlags)
      clazz setInfo ClassInfoType(parents, scope, clazz)
    }
    final def newErrorClass(name: TypeName): ClassSymbol =
      newClassWithInfo(name, Nil, new ErrorScope(this), pos, SYNTHETIC | IS_ERROR)

    final def newModuleClass(name: TypeName, pos: Position = NoPosition, newFlags: Long = 0L): ModuleClassSymbol =
      newModuleClassSymbol(name, pos, newFlags | MODULE)

    final def newAnonymousFunctionClass(pos: Position = NoPosition, newFlags: Long = 0L): ClassSymbol =
      newClassSymbol(tpnme.ANON_FUN_NAME, pos, FINAL | SYNTHETIC | newFlags)

    final def newAnonymousFunctionValue(pos: Position, newFlags: Long = 0L): TermSymbol =
      newTermSymbol(nme.ANON_FUN_NAME, pos, SYNTHETIC | newFlags) setInfo NoType

    def newImplClass(name: TypeName, pos: Position = NoPosition, newFlags: Long = 0L): ClassSymbol = {
      newClassSymbol(name, pos, newFlags | IMPLCLASS)
    }

    /** Refinement types P { val x: String; type T <: Number }
     *  also have symbols, they are refinementClasses
     */
    final def newRefinementClass(pos: Position): RefinementClassSymbol =
      createRefinementClassSymbol(pos, 0L)

    final def newErrorSymbol(name: Name): Symbol = name match {
      case x: TypeName  => newErrorClass(x)
      case x: TermName  => newErrorValue(x)
    }

    /** Creates a placeholder symbol for when a name is encountered during
     *  unpickling for which there is no corresponding classfile.  This defers
     *  failure to the point when that name is used for something, which is
     *  often to the point of never.
     */
    def newStubSymbol(name: Name, missingMessage: String, isPackage: Boolean = false): Symbol = name match {
      case n: TypeName  => if (isPackage) new StubPackageClassSymbol(this, n, missingMessage) else new StubClassSymbol(this, n, missingMessage)
      case _            => new StubTermSymbol(this, name.toTermName, missingMessage)
    }

    /** Given a field, construct a term symbol that represents the source construct that gave rise the field */
    def sugaredSymbolOrSelf = {
      val getter = getterIn(owner)
      if (getter == NoSymbol) {
        this
      } else {
        val result = owner.newValue(getter.name.toTermName, newFlags = getter.flags & ~Flags.METHOD).setPrivateWithin(getter.privateWithin).setInfo(getter.info.resultType)
        val setter = setterIn(owner)
        if (setter != NoSymbol) result.setFlag(Flags.MUTABLE)
        result
      }
    }

// ----- locking and unlocking ------------------------------------------------------

    // True if the symbol is unlocked.
    // True if the symbol is locked but still below the allowed recursion depth.
    // False otherwise
    private[scala] def lockOK: Boolean = {
      ((_rawflags & LOCKED) == 0L) ||
      ((settings.Yrecursion.value != 0) &&
       (recursionTable get this match {
         case Some(n) => (n <= settings.Yrecursion.value)
         case None => true }))
    }

    // Lock a symbol, using the handler if the recursion depth becomes too great.
    private[scala] def lock(handler: => Unit): Boolean = {
      if ((_rawflags & LOCKED) != 0L) {
        if (settings.Yrecursion.value != 0) {
          recursionTable get this match {
            case Some(n) =>
              if (n > settings.Yrecursion.value) {
                handler
                false
              } else {
                recursionTable += (this -> (n + 1))
                true
              }
            case None =>
              recursionTable += (this -> 1)
              true
          }
        } else { handler; false }
      } else {
        _rawflags |= LOCKED
        true
//        activeLocks += 1
//        lockedSyms += this
      }
    }

    // Unlock a symbol
    private[scala] def unlock() = {
      if ((_rawflags & LOCKED) != 0L) {
//        activeLocks -= 1
//        lockedSyms -= this
        _rawflags &= ~LOCKED
        if (settings.Yrecursion.value != 0)
          recursionTable -= this
      }
    }

// ----- tests ----------------------------------------------------------------------

    def isAliasType    = false
    def isAbstractType = false
    def isSkolem       = false

    /** A Type, but not a Class. */
    def isNonClassType = false

    /** The bottom classes are Nothing and Null, found in Definitions. */
    def isBottomClass  = false

    /** These are all tests for varieties of ClassSymbol, which has these subclasses:
     *  - ModuleClassSymbol
     *  - RefinementClassSymbol
     *  - PackageClassSymbol (extends ModuleClassSymbol)
     */
    def isAbstractClass         = false
    def isAnonOrRefinementClass = false
    def isAnonymousClass        = false
    def isCaseClass             = false
    def isConcreteClass         = false
    def isImplClass             = false   // the implementation class of a trait
    def isJavaInterface         = false
    def isNumericValueClass     = false
    def isPrimitiveValueClass   = false
    def isRefinementClass       = false
    override def isTrait        = false

    /** Qualities of Types, always false for TermSymbols.
     */
    def isContravariant         = false
    def isCovariant             = false
    def isExistentialSkolem     = false
    def isExistentiallyBound    = false
    def isGADTSkolem            = false
    def isTypeParameter         = false
    def isTypeParameterOrSkolem = false
    def isTypeSkolem            = false
    def isInvariant             = !isCovariant && !isContravariant

    /** Qualities of Terms, always false for TypeSymbols.
     */
    def isAccessor          = false
    def isBridge            = false
    def isCapturedVariable  = false
    def isClassConstructor  = false
    def isConstructor       = false
    def isEarlyInitialized  = false
    def isGetter            = false
    def isDefaultGetter     = false
    def isLocalDummy        = false
    def isMixinConstructor  = false
    def isOverloaded        = false
    def isSetter            = false
    def isSetterParameter   = false
    def isValue             = false
    def isValueParameter    = false
    def isVariable          = false
    def isTermMacro         = false

    /** Qualities of MethodSymbols, always false for TypeSymbols
     *  and other TermSymbols.
     */
    def isCaseAccessorMethod = false
    def isLiftedMethod       = false
    def isSourceMethod       = false
    def isVarargsMethod      = false
    override def isLabel     = false

    /** Package/package object tests */
    def isPackageClass         = false
    def isPackageObject        = false
    def isPackageObjectClass   = false
    def isPackageObjectOrClass = isPackageObject || isPackageObjectClass
    def isModuleOrModuleClass  = isModule || isModuleClass

    /** Overridden in custom objects in Definitions */
    def isRoot              = false
    def isRootPackage       = false
    def isRootSymbol        = false   // RootPackage and RootClass.  TODO: also NoSymbol.
    def isEmptyPackage      = false
    def isEmptyPackageClass = false

    /** Is this symbol an effective root for fullname string?
     */
    def isEffectiveRoot = false

    /** Can this symbol only be subclassed by bottom classes? This is assessed
     *  to be the case if it is final, and any type parameters are invariant.
     */
    def hasOnlyBottomSubclasses = {
      def loop(tparams: List[Symbol]): Boolean = tparams match {
        case Nil     => true
        case x :: xs => x.variance.isInvariant && loop(xs)
      }
      isClass && isFinal && loop(typeParams)
    }

    final def isLazyAccessor       = isLazy && lazyAccessor != NoSymbol
    final def isOverridableMember  = !(isClass || isEffectivelyFinal) && safeOwner.isClass

    /** Does this symbol denote a wrapper created by the repl? */
    final def isInterpreterWrapper = (
         (this hasFlag MODULE)
      && isTopLevel
      && nme.isReplWrapperName(name)
    )

    /** In our current architecture, symbols for top-level classes and modules
     *  are created as dummies. Package symbols just call newClass(name) or newModule(name) and
     *  consider their job done.
     *
     *  In order for such a dummy to provide meaningful info (e.g. a list of its members),
     *  it needs to go through unpickling. Unpickling is a process of reading Scala metadata
     *  from ScalaSignature annotations and assigning it to symbols and types.
     *
     *  A single unpickling session takes a top-level class or module, parses the ScalaSignature annotation
     *  and then reads metadata for the unpicklee, its companion (if any) and all their members recursively
     *  (i.e. the pickle not only contains info about directly nested classes/modules, but also about
     *  classes/modules nested into those and so on).
     *
     *  Unpickling is triggered automatically whenever info (info in compiler parlance) is called.
     *  This happens because package symbols assign completer thunks to the dummies they create.
     *  Therefore metadata loading happens lazily and transparently.
     *
     *  Almost transparently. Unfortunately metadata isn't limited to just signatures (i.e. lists of members).
     *  It also includes flags (which determine e.g. whether a class is sealed or not), annotations and privateWithin.
     *  This gives rise to unpleasant effects like in SI-6277, when a flag test called on an uninitialize symbol
     *  produces incorrect results.
     *
     *  One might think that the solution is simple: automatically call the completer
     *  whenever one needs flags, annotations and privateWithin - just like it's done for info.
     *  Unfortunately, this leads to weird crashes in scalac, and currently we can't attempt
     *  to fix the core of the compiler risk stability a few weeks before the final release.
     *  upd. Haha, "a few weeks before the final release". This surely sounds familiar :)
     *
     *  However we do need to fix this for runtime reflection, since this idiosyncrasy is not something
     *  we'd like to expose to reflection users. Therefore a proposed solution is to check whether we're in a
     *  runtime reflection universe, and if yes and if we've not yet loaded the requested info, then to commence initialization.
     */
    final def getFlag(mask: Long): Long = {
      if (!isCompilerUniverse && !isThreadsafe(purpose = FlagOps(mask))) initialize
      flags & mask
    }
    /** Does symbol have ANY flag in `mask` set? */
    final def hasFlag(mask: Long): Boolean = {
      // See `getFlag` to learn more about the `isThreadsafe` call in the body of this method.
      if (!isCompilerUniverse && !isThreadsafe(purpose = FlagOps(mask))) initialize
      (flags & mask) != 0
    }
    def hasFlag(mask: Int): Boolean = hasFlag(mask.toLong)

    /** Does symbol have ALL the flags in `mask` set? */
    final def hasAllFlags(mask: Long): Boolean = {
      // See `getFlag` to learn more about the `isThreadsafe` call in the body of this method.
      if (!isCompilerUniverse && !isThreadsafe(purpose = FlagOps(mask))) initialize
      (flags & mask) == mask
    }

    def setFlag(mask: Long): this.type   = { _rawflags |= mask ; this }
    def resetFlag(mask: Long): this.type = { _rawflags &= ~mask ; this }
    def resetFlags() { rawflags &= TopLevelCreationFlags }

    /** Default implementation calls the generic string function, which
     *  will print overloaded flags as <flag1/flag2/flag3>.  Subclasses
     *  of Symbol refine.
     */
    override def resolveOverloadedFlag(flag: Long): String = Flags.flagToString(flag)

    /** Set the symbol's flags to the given value, asserting
     *  that the previous value was 0.
     */
    def initFlags(mask: Long): this.type = {
      assert(rawflags == 0L, symbolCreationString)
      _rawflags = mask
      this
    }

    final def flags: Long = {
      if (Statistics.hotEnabled) Statistics.incCounter(flagsCount)
      val fs = _rawflags & phase.flagMask
      (fs | ((fs & LateFlags) >>> LateShift)) & ~((fs & AntiFlags) >>> AntiShift)
    }
    def flags_=(fs: Long) = _rawflags = fs
    def rawflags_=(x: Long) { _rawflags = x }

    final def hasGetter = isTerm && nme.isLocalName(name)

    /**
     * Nested modules which have no static owner when ModuleDefs are eliminated (refchecks) are
     * given the lateMETHOD flag, which makes them appear as methods after refchecks.
     *
     * Note: the lateMETHOD flag is added lazily in the info transformer of the RefChecks phase.
     * This means that forcing the `sym.info` may change the value of `sym.isMethod`. Forcing the
     * info is in the responsibility of the caller. Doing it eagerly here was tried (0ccdb151f) but
     * has proven to lead to bugs (SI-8907).
     *
     * Here's an example where one can see all four of FF FT TF TT for (isStatic, isMethod) at
     * various phases.
     *
     *   trait A1 { case class Quux() }
     *   object A2 extends A1 { object Flax }
     *   // --  namer         object Quux in trait A1
     *   // -M  flatten       object Quux in trait A1
     *   // S-  flatten       object Flax in object A2
     *   // -M  posterasure   object Quux in trait A1
     *   // -M  jvm           object Quux in trait A1
     *   // SM  jvm           object Quux in object A2
     *
     * So "isModuleNotMethod" exists not for its achievement in brevity, but to encapsulate the
     * relevant condition.
     */
    def isModuleNotMethod = isModule && !isMethod

    // After RefChecks, the `isStatic` check is mostly redundant: all non-static modules should
    // be methods (and vice versa). There's a corner case on the vice-versa with mixed-in module
    // symbols:
    //   trait T { object A }
    //   object O extends T
    // The module symbol A is cloned into T$impl (addInterfaces), and then cloned into O (mixin).
    // Since the original A is not static, it's turned into a method. The clone in O however is
    // static (owned by a module), but it's also a method.
    def isStaticModule = isModuleNotMethod && isStatic

    final def isInitializedToDefault = !isType && hasAllFlags(DEFAULTINIT | ACCESSOR)
    final def isThisSym = isTerm && owner.thisSym == this
    final def isError = hasFlag(IS_ERROR)
    final def isErroneous = isError || isInitialized && tpe_*.isErroneous

    def isHigherOrderTypeParameter = owner.isTypeParameterOrSkolem

    // class C extends D( { class E { ... } ... } ). Here, E is a class local to a constructor
    def isClassLocalToConstructor = false

    final def isDerivedValueClass =
      isClass && !hasFlag(PACKAGE | TRAIT) &&
      info.firstParent.typeSymbol == AnyValClass && !isPrimitiveValueClass

    final def isMethodWithExtension =
      isMethod && owner.isDerivedValueClass && !isParamAccessor && !isConstructor && !hasFlag(SUPERACCESSOR) && !isMacro && !isSpecialized

    final def isAnonymousFunction = isSynthetic && (name containsName tpnme.ANON_FUN_NAME)
    final def isDelambdafyFunction = isSynthetic && (name containsName tpnme.DELAMBDAFY_LAMBDA_CLASS_NAME)
    final def isDelambdafyTarget  = isArtifact && isMethod && (name containsName tpnme.ANON_FUN_NAME)
    final def isDefinedInPackage  = effectiveOwner.isPackageClass
    final def needsFlatClasses    = phase.flatClasses && rawowner != NoSymbol && !rawowner.isPackageClass

    // TODO introduce a flag for these?
    final def isPatternTypeVariable: Boolean =
      isAbstractType && !isExistential && !isTypeParameterOrSkolem && isLocalToBlock

    /** change name by appending $$<fully-qualified-name-of-class `base`>
     *  Do the same for any accessed symbols or setters/getters.
     *  Implementation in TermSymbol.
     */
    def expandName(base: Symbol) { }

    // In java.lang, Predef, or scala package/package object
    def isInDefaultNamespace = UnqualifiedOwners(effectiveOwner)

    /** The owner, skipping package objects.
     */
    def effectiveOwner = owner.skipPackageObject

    /** If this is a package object or its implementing class, its owner: otherwise this.
     */
    def skipPackageObject: Symbol = this

    /** If this is a constructor, its owner: otherwise this.
     */
    final def skipConstructor: Symbol = if (isConstructor) owner else this

    /** Conditions where we omit the prefix when printing a symbol, to avoid
     *  unpleasantries like Predef.String, $iw.$iw.Foo and <empty>.Bippy.
     */
    final def isOmittablePrefix = /*!settings.debug.value &&*/ (
         UnqualifiedOwners(skipPackageObject)
      || isEmptyPrefix
    )
    def isEmptyPrefix = (
         isEffectiveRoot                      // has no prefix for real, <empty> or <root>
      || isAnonOrRefinementClass              // has uninteresting <anon> or <refinement> prefix
      || nme.isReplWrapperName(name)          // has ugly $iw. prefix (doesn't call isInterpreterWrapper due to nesting)
    )
    def isFBounded = info match {
      case TypeBounds(_, _) => info.baseTypeSeq exists (_ contains this)
      case _                => false
    }

    /** Is symbol a monomorphic type?
     *  assumption: if a type starts out as monomorphic, it will not acquire
     *  type parameters in later phases.
     */
    final def isMonomorphicType =
      isType && {
        val info = originalInfo
        (    (info eq null)
          || (info.isComplete && !info.isHigherKinded)
        )
      }

    def isStrictFP          = hasAnnotation(ScalaStrictFPAttr) || (enclClass hasAnnotation ScalaStrictFPAttr)
    def isSerializable      = info.baseClasses.exists(p => p == SerializableClass || p == JavaSerializableClass)
    def hasBridgeAnnotation = hasAnnotation(BridgeClass)
    def isDeprecated        = hasAnnotation(DeprecatedAttr)
    def deprecationMessage  = getAnnotation(DeprecatedAttr) flatMap (_ stringArg 0)
    def deprecationVersion  = getAnnotation(DeprecatedAttr) flatMap (_ stringArg 1)
    def deprecatedParamName = getAnnotation(DeprecatedNameAttr) flatMap (_ symbolArg 0)
    def hasDeprecatedInheritanceAnnotation
                            = hasAnnotation(DeprecatedInheritanceAttr)
    def deprecatedInheritanceMessage
                            = getAnnotation(DeprecatedInheritanceAttr) flatMap (_ stringArg 0)
    def hasDeprecatedOverridingAnnotation
                            = hasAnnotation(DeprecatedOverridingAttr)
    def deprecatedOverridingMessage
                            = getAnnotation(DeprecatedOverridingAttr) flatMap (_ stringArg 0)

    // !!! when annotation arguments are not literal strings, but any sort of
    // assembly of strings, there is a fair chance they will turn up here not as
    // Literal(const) but some arbitrary AST.  However nothing in the compiler
    // prevents someone from writing a @migration annotation with a calculated
    // string.  So this needs attention.  For now the fact that migration is
    // private[scala] ought to provide enough protection.
    def hasMigrationAnnotation = hasAnnotation(MigrationAnnotationClass)
    def migrationMessage    = getAnnotation(MigrationAnnotationClass) flatMap { _.stringArg(0) }
    def migrationVersion    = getAnnotation(MigrationAnnotationClass) flatMap { _.stringArg(1) }
    def elisionLevel        = getAnnotation(ElidableMethodClass) flatMap { _.intArg(0) }
    def implicitNotFoundMsg = getAnnotation(ImplicitNotFoundClass) flatMap { _.stringArg(0) }

    def isCompileTimeOnly       = hasAnnotation(CompileTimeOnlyAttr)
    def compileTimeOnlyMessage  = getAnnotation(CompileTimeOnlyAttr) flatMap (_ stringArg 0)

    /** Is this symbol an accessor method for outer? */
    final def isOuterAccessor = hasFlag(STABLE | ARTIFACT) && (unexpandedName == nme.OUTER)

    /** Is this symbol an accessor method for outer? */
    final def isOuterField = isArtifact && (unexpandedName == nme.OUTER_LOCAL)

    /** Does this symbol denote a stable value, ignoring volatility?
     *
     * Stability and volatility are checked separately to allow volatile paths in patterns that amount to equality checks. SI-6815
     */
    final def isStable        = isTerm && !isMutable && !(hasFlag(BYNAMEPARAM)) && (!isMethod || hasStableFlag)
    final def hasVolatileType = tpe.isVolatile && !hasAnnotation(uncheckedStableClass)

    /** Does this symbol denote the primary constructor of its enclosing class? */
    final def isPrimaryConstructor =
      isConstructor && owner.primaryConstructor == this

    /** Does this symbol denote an auxiliary constructor of its enclosing class? */
    final def isAuxiliaryConstructor =
      isConstructor && !isPrimaryConstructor

    /** Is this symbol a synthetic apply or unapply method in a companion object of a case class? */
    // xeno-by: why this obscure use of the CASE flag? why not simply compare name with nme.apply and nme.unapply?
    final def isCaseApplyOrUnapply =
      isMethod && isCase && isSynthetic

    /** Is this symbol a synthetic copy method in a case class? */
    final def isCaseCopy =
      isMethod && owner.isCase && isSynthetic && name == nme.copy

    /** Is this symbol a trait which needs an implementation class? */
    final def needsImplClass = (
         isTrait
      && (!isInterface || hasFlag(lateINTERFACE))
      && !isImplClass
    )

    /** Is this a symbol which exists only in the implementation class, not in its trait? */
    final def isImplOnly = isPrivate || (
       (owner.isTrait || owner.isImplClass) && (
            hasAllFlags(LIFTED | MODULE | METHOD)
         || isConstructor
         || hasFlag(notPRIVATE | LIFTED) && !hasFlag(ACCESSOR | SUPERACCESSOR | MODULE)
       )
    )
    final def isModuleVar = hasFlag(MODULEVAR)

    /**
     * Is this symbol static (i.e. with no outer instance)?
     * Q: When exactly is a sym marked as STATIC?
     * A: If it's a member of a toplevel object, or of an object contained in a toplevel object, or
     * any number of levels deep.
     * http://groups.google.com/group/scala-internals/browse_thread/thread/d385bcd60b08faf6
     *
     * TODO: should this only be invoked on class / module symbols? because there's also `isStaticMember`.
     *
     * Note: the result of `isStatic` changes over time.
     *  - Lambdalift local definitions to the class level, the `owner` field is modified.
     *      object T { def foo { object O } }
     *    After lambdalift, the OModule.isStatic is true.
     *
     *  - After flatten, nested classes are moved to the package level. Invoking `owner` on a
     *    class returns a package class, for which `isStaticOwner` is true. For example,
     *      class C { object O }
     *    OModuleClass.isStatic is true after flatten. Using phase travel to get before flatten,
     *    method `owner` returns the class C.
     *
     * Why not make a stable version of `isStatic`? Maybe some parts of the compiler depend on the
     * current implementation. For example
     *   trait T { def foo = 1 }
     * The method `foo` in the implementation class T$impl will be `isStatic`, because trait
     * impl classes get the `lateMODULE` flag (T$impl.isStaticOwner is true).
     */
    def isStatic = (this hasFlag STATIC) || owner.isStaticOwner

    /** Is this symbol a static constructor? */
    final def isStaticConstructor: Boolean =
      isStaticMember && isClassConstructor

    /** Is this symbol a static member of its class? (i.e. needs to be implemented as a Java static?) */
    final def isStaticMember: Boolean =
      hasFlag(STATIC) || owner.isImplClass

    /** Does this symbol denote a class that defines static symbols? */
    final def isStaticOwner: Boolean =
      isPackageClass || isModuleClass && isStatic

    /** A helper function for isEffectivelyFinal. */
    private def isNotOverridden = (
      owner.isClass && (
           owner.isEffectivelyFinal
        || owner.isSealed && owner.children.forall(c => c.isEffectivelyFinal && (overridingSymbol(c) == NoSymbol))
      )
    )

    /** Is this symbol effectively final? I.e, it cannot be overridden */
    final def isEffectivelyFinal: Boolean = (
         (this hasFlag FINAL | PACKAGE)
      || isModuleOrModuleClass && (isTopLevel || !settings.overrideObjects)
      || isTerm && (
             isPrivate
          || isLocalToBlock
         )
    )
    /** Is this symbol effectively final or a concrete term member of sealed class whose children do not override it */
    final def isEffectivelyFinalOrNotOverridden: Boolean = isEffectivelyFinal || (isTerm && !isDeferred && isNotOverridden)

    /** Is this symbol owned by a package? */
    final def isTopLevel = owner.isPackageClass

    /** Is this symbol defined in a block? */
    @deprecated("Use isLocalToBlock instead", "2.11.0")
    final def isLocal: Boolean = owner.isTerm

    /** Is this symbol defined in a block? */
    final def isLocalToBlock: Boolean = owner.isTerm

    /** Is this symbol a constant? */
    final def isConstant: Boolean = isStable && isConstantType(tpe.resultType)

    /** Is this class nested in another class or module (not a package). Includes locally defined classes. */
    def isNestedClass = false

    /** Is this class locally defined?
     *  A class is local, if
     *   - it is anonymous, or
     *   - its owner is a value
     *   - it is defined within a local class
     */
    def isLocalClass = false

    /** Is this class or type defined as a structural refinement type?
     */
    final def isStructuralRefinement: Boolean =
      (isClass || isType || isModule) && info.dealiasWiden/*.underlying*/.isStructuralRefinement

    /** Is this a term symbol only defined in a refinement (so that it needs
     *  to be accessed by reflection)?
     */
    def isOnlyRefinementMember = (
         isTerm                  // Type members are unaffected
      && owner.isRefinementClass // owner must be a refinement class
      && isPossibleInRefinement  // any overridden symbols must also have refinement class owners
      && !isConstant             // Must not be a constant. Question: Can we exclude @inline methods as well?
      && isDeclaredByOwner       // Must be explicitly declared in the refinement (not synthesized from glb)
    )
    // "(owner.info decl name) == this" is inadequate, because "name" might
    // be overloaded in owner - and this might be an overloaded symbol.
    // TODO - make this cheaper and see where else we should be doing something similar.
    private def isDeclaredByOwner = (owner.info decl name).alternatives exists (alternatives contains _)

    final def isStructuralRefinementMember = owner.isStructuralRefinement && isPossibleInRefinement && isPublic
    final def isPossibleInRefinement       = (
         !isConstructor
      && allOverriddenSymbols.forall(_.owner.isRefinementClass) // this includes allOverriddenSymbols.isEmpty
    )

    /** A a member of class `base` is incomplete if
     *  (1) it is declared deferred or
     *  (2) it is abstract override and its super symbol in `base` is
     *      nonexistent or incomplete.
     */
    final def isIncompleteIn(base: Symbol): Boolean =
      this.isDeferred ||
      (this hasFlag ABSOVERRIDE) && {
        val supersym = superSymbolIn(base)
        supersym == NoSymbol || supersym.isIncompleteIn(base)
      }

    def exists: Boolean = !isTopLevel || {
      val isSourceLoader = rawInfo match {
        case sl: SymLoader => sl.fromSource
        case _             => false
      }
      def warnIfSourceLoader() {
        if (isSourceLoader)
          // Predef is completed early due to its autoimport; we used to get here when type checking its
          // parent LowPriorityImplicits. See comment in c5441dc for more elaboration.
          // Since the fix for SI-7335 Predef parents must be defined in Predef.scala, and we should not
          // get here anymore.
          devWarning(s"calling Symbol#exists with sourcefile based symbol loader may give incorrect results.");
      }

      rawInfo load this
      rawInfo != NoType || { warnIfSourceLoader(); false }
    }

    final def isInitialized: Boolean =
      validTo != NoPeriod

    /** We consider a symbol to be thread-safe, when multiple concurrent threads can call its methods
     *  (either directly or indirectly via public reflection or internal compiler infrastructure),
     *  without any locking and everything works as it should work.
     *
     *  In its basic form, `isThreadsafe` always returns false. Runtime reflection augments reflection infrastructure
     *  with threadsafety-tracking mechanism implemented in `SynchronizedSymbol` that communicates with underlying completers
     *  and can sometimes return true if the symbol has been completed to the point of thread safety.
     *
     *  The `purpose` parameter signifies whether we want to just check immutability of certain flags for the given mask.
     *  This is necessary to enable robust auto-initialization of `Symbol.flags` for runtime reflection, and is also quite handy
     *  in avoiding unnecessary initializations when requesting for flags that have already been set.
     */
    def isThreadsafe(purpose: SymbolOps): Boolean = false
    def markFlagsCompleted(mask: Long): this.type = this
    def markAllCompleted(): this.type = this

    /** Can this symbol be loaded by a reflective mirror?
     *
     *  Scalac relies on `ScalaSignature' annotation to retain symbols across compilation runs.
     *  Such annotations (also called "pickles") are applied on top-level classes and include information
     *  about all symbols reachable from the annotee. However, local symbols (e.g. classes or definitions local to a block)
     *  are typically unreachable and information about them gets lost.
     *
     *  This method is useful for macro writers who wish to save certain ASTs to be used at runtime.
     *  With `isLocatable' it's possible to check whether a tree can be retained as is, or it needs special treatment.
     */
    final def isLocatable: Boolean = {
      if (this == NoSymbol) return false
      if (isRoot || isRootPackage) return true

      if (!owner.isLocatable) return false
      if (owner.isTerm) return false
      if (isLocalDummy) return false

      if (isAliasType) return true
      if (isType && isNonClassType) return false
      if (isRefinementClass) return false
      true
    }

    /** The variance of this symbol. */
    def variance: Variance =
      if (isCovariant) Covariant
      else if (isContravariant) Contravariant
      else Invariant

    /** The sequence number of this parameter symbol among all type
     *  and value parameters of symbol's owner. -1 if symbol does not
     *  appear among the parameters of its owner.
     */
    def paramPos: Int = {
      def searchIn(tpe: Type, base: Int): Int = {
        def searchList(params: List[Symbol], fallback: Type): Int = {
          val idx = params indexOf this
          if (idx >= 0) idx + base
          else searchIn(fallback, base + params.length)
        }
        tpe match {
          case PolyType(tparams, res) => searchList(tparams, res)
          case MethodType(params, res) => searchList(params, res)
          case _ => -1
        }
      }
      searchIn(owner.info, 0)
    }

// ------ owner attribute --------------------------------------------------------------

    /**
     * The owner of a symbol. Changes over time to adapt to the structure of the trees:
     *  - Up to lambdalift, the owner is the lexically enclosing definition. For definitions
     *    in a local block, the owner is also the next enclosing definition.
     *  - After lambdalift, all local method and class definitions (those not owned by a class
     *    or package class) change their owner to the enclosing class. This is done through
     *    a destructive "sym.owner = sym.owner.enclClass". The old owner is saved by
     *    saveOriginalOwner.
     *  - After flatten, all classes are owned by a PackageClass. This is done through a
     *    phase check (if after flatten) in the (overridden) method "def owner" in
     *    ModuleSymbol / ClassSymbol. The `rawowner` field is not modified.
     *  - Owners are also changed in other situations, for example when moving trees into a new
     *    lexical context, e.g. in the named/default arguments transformation, or when translating
     *    extension method definitions.
     *
     * In general when seeking the owner of a symbol, one should call `owner`.
     * The other possibilities include:
     *   - call `safeOwner` if it is expected that the target may be NoSymbol
     *   - call `assertOwner` if it is an unrecoverable error if the target is NoSymbol
     *
     * `owner` behaves like `safeOwner`, but logs NoSymbol.owner calls under -Xdev.
     * `assertOwner` aborts compilation immediately if called on NoSymbol.
     */
    def owner: Symbol = {
      if (Statistics.hotEnabled) Statistics.incCounter(ownerCount)
      rawowner
    }
    final def safeOwner: Symbol   = if (this eq NoSymbol) NoSymbol else owner
    final def assertOwner: Symbol = if (this eq NoSymbol) abort("no-symbol does not have an owner") else owner

    /**
     * The initial owner of this symbol.
     */
    def originalOwner: Symbol = originalOwnerMap.getOrElse(this, rawowner)

    // TODO - don't allow the owner to be changed without checking invariants, at least
    // when under some flag. Define per-phase invariants for owner/owned relationships,
    // e.g. after flatten all classes are owned by package classes, there are lots and
    // lots of these to be declared (or more realistically, discovered.)
    def owner_=(owner: Symbol) {
      saveOriginalOwner(this)
      assert(isCompilerUniverse, "owner_= is not thread-safe; cannot be run in reflexive code")
      if (traceSymbolActivity)
        traceSymbols.recordNewSymbolOwner(this, owner)
      _rawowner = owner
    }

    def ownerChain: List[Symbol] = this :: owner.ownerChain

    // Non-classes skip self and return rest of owner chain; overridden in ClassSymbol.
    def enclClassChain: List[Symbol] = owner.enclClassChain

    def ownersIterator: Iterator[Symbol] = new Iterator[Symbol] {
      private var current = Symbol.this
      def hasNext = current ne NoSymbol
      def next = { val r = current; current = current.owner; r }
    }

    /** Same as `ownerChain contains sym` but more efficient, and
     *  with a twist for refinement classes (see RefinementClassSymbol.)
     */
    def hasTransOwner(sym: Symbol): Boolean = {
      var o = this
      while ((o ne sym) && (o ne NoSymbol)) o = o.owner
      (o eq sym)
    }

// ------ name attribute --------------------------------------------------------------

    @deprecated("Use unexpandedName", "2.11.0") def originalName: Name = unexpandedName

    /** If this symbol has an expanded name, its original (unexpanded) name,
     *  otherwise the name itself.
     */
    def unexpandedName: Name = nme.unexpandedName(name)

    /** The name of the symbol before decoding, e.g. `\$eq\$eq` instead of `==`.
     */
    def encodedName: String = name.toString

    /** The decoded name of the symbol, e.g. `==` instead of `\$eq\$eq`.
     */
    def decodedName: String = name.decode

    private def addModuleSuffix(n: Name): Name =
      if (needsModuleSuffix) n append nme.MODULE_SUFFIX_STRING else n

    def moduleSuffix: String = (
      if (needsModuleSuffix) nme.MODULE_SUFFIX_STRING
      else ""
    )
    /** Whether this symbol needs nme.MODULE_SUFFIX_STRING (aka $) appended on the java platform.
     */
    def needsModuleSuffix = (
         hasModuleFlag
      && !isMethod
      && !isImplClass
      && !isJavaDefined
    )
    /** These should be moved somewhere like JavaPlatform.
     */
    def javaSimpleName: Name = addModuleSuffix(simpleName.dropLocal)
    def javaBinaryName: Name = name.newName(javaBinaryNameString)
    def javaBinaryNameString: String = fullName('/', moduleSuffix)
    def javaClassName: String  = fullName('.', moduleSuffix)

    /** The encoded full path name of this symbol, where outer names and inner names
     *  are separated by `separator` characters.
     *  Never translates expansions of operators back to operator symbol.
     *  Never adds id.
     *  Drops package objects.
     */
    final def fullName(separator: Char): String = fullName(separator, "")

    private def fullName(separator: Char, suffix: CharSequence): String = {
      var b: java.lang.StringBuffer = null
      def loop(size: Int, sym: Symbol): Unit = {
        val symName = sym.name
        val nSize = symName.length - (if (symName.endsWith(nme.LOCAL_SUFFIX_STRING)) 1 else 0)
        if (sym.isRoot || sym.isRootPackage || sym == NoSymbol || sym.owner.isEffectiveRoot) {
          val capacity = size + nSize
          b = new java.lang.StringBuffer(capacity)
          b.append(chrs, symName.start, nSize)
        } else {
          loop(size + nSize + 1, sym.effectiveOwner.enclClass)
          b.append(separator)
          b.append(chrs, symName.start, nSize)
        }
      }
      loop(suffix.length(), this)
      b.append(suffix)
      b.toString
    }

    def fullNameAsName(separator: Char): Name = name.newName(fullName(separator, ""))

    /** The encoded full path name of this symbol, where outer names and inner names
     *  are separated by periods.
     */
    final def fullName: String = fullName('.')

    /**
     *  Symbol creation implementations.
     */

    protected def createAbstractTypeSymbol(name: TypeName, pos: Position, newFlags: Long): AbstractTypeSymbol =
      new AbstractTypeSymbol(this, pos, name) initFlags newFlags

    protected def createAliasTypeSymbol(name: TypeName, pos: Position, newFlags: Long): AliasTypeSymbol =
      new AliasTypeSymbol(this, pos, name) initFlags newFlags

    protected def createTypeSkolemSymbol(name: TypeName, origin: AnyRef, pos: Position, newFlags: Long): TypeSkolem =
      new TypeSkolem(this, pos, name, origin) initFlags newFlags

    protected def createClassSymbol(name: TypeName, pos: Position, newFlags: Long): ClassSymbol =
      new ClassSymbol(this, pos, name) initFlags newFlags

    protected def createModuleClassSymbol(name: TypeName, pos: Position, newFlags: Long): ModuleClassSymbol =
      new ModuleClassSymbol(this, pos, name) initFlags newFlags

    protected def createPackageClassSymbol(name: TypeName, pos: Position, newFlags: Long): PackageClassSymbol =
      new PackageClassSymbol(this, pos, name) initFlags newFlags

    protected def createRefinementClassSymbol(pos: Position, newFlags: Long): RefinementClassSymbol =
      new RefinementClassSymbol(this, pos) initFlags newFlags

    protected def createPackageObjectClassSymbol(pos: Position, newFlags: Long): PackageObjectClassSymbol =
      new PackageObjectClassSymbol(this, pos) initFlags newFlags

    protected def createImplClassSymbol(name: TypeName, pos: Position, newFlags: Long): ClassSymbol =
      new ClassSymbol(this, pos, name) with ImplClassSymbol initFlags newFlags

    protected def createMethodSymbol(name: TermName, pos: Position, newFlags: Long): MethodSymbol =
      new MethodSymbol(this, pos, name) initFlags newFlags

    protected def createModuleSymbol(name: TermName, pos: Position, newFlags: Long): ModuleSymbol =
      new ModuleSymbol(this, pos, name) initFlags newFlags

    protected def createPackageSymbol(name: TermName, pos: Position, newFlags: Long): ModuleSymbol =
      new ModuleSymbol(this, pos, name) initFlags newFlags

    protected def createValueParameterSymbol(name: TermName, pos: Position, newFlags: Long): TermSymbol =
      new TermSymbol(this, pos, name) initFlags newFlags

    protected def createValueMemberSymbol(name: TermName, pos: Position, newFlags: Long): TermSymbol =
      new TermSymbol(this, pos, name) initFlags newFlags

    final def newTermSymbol(name: TermName, pos: Position = NoPosition, newFlags: Long = 0L): TermSymbol = {
      // Package before Module, Module before Method, or we might grab the wrong guy.
      if ((newFlags & PACKAGE) != 0)
        createPackageSymbol(name, pos, newFlags | PackageFlags)
      else if ((newFlags & MODULE) != 0)
        createModuleSymbol(name, pos, newFlags)
      else if ((newFlags & METHOD) != 0)
        createMethodSymbol(name, pos, newFlags)
      else if ((newFlags & PARAM) != 0)
        createValueParameterSymbol(name, pos, newFlags)
      else
        createValueMemberSymbol(name, pos, newFlags)
    }

    final def newClassSymbol(name: TypeName, pos: Position = NoPosition, newFlags: Long = 0L): ClassSymbol = {
      if (name == tpnme.REFINE_CLASS_NAME)
        createRefinementClassSymbol(pos, newFlags)
      else if ((newFlags & PACKAGE) != 0)
        createPackageClassSymbol(name, pos, newFlags | PackageFlags)
      else if (name == tpnme.PACKAGE)
        createPackageObjectClassSymbol(pos, newFlags)
      else if ((newFlags & MODULE) != 0)
        createModuleClassSymbol(name, pos, newFlags)
      else if ((newFlags & IMPLCLASS) != 0)
        createImplClassSymbol(name, pos, newFlags)
      else
        createClassSymbol(name, pos, newFlags)
    }

    final def newNonClassSymbol(name: TypeName, pos: Position = NoPosition, newFlags: Long = 0L): TypeSymbol = {
      if ((newFlags & DEFERRED) != 0)
        createAbstractTypeSymbol(name, pos, newFlags)
      else
        createAliasTypeSymbol(name, pos, newFlags)
    }

    def newTypeSymbol(name: TypeName, pos: Position = NoPosition, newFlags: Long = 0L): TypeSymbol =
      newNonClassSymbol(name, pos, newFlags)

    /** The class or term up to which this symbol is accessible,
     *  or RootClass if it is public.  As java protected statics are
     *  otherwise completely inaccessible in scala, they are treated
     *  as public.
     */
    def accessBoundary(base: Symbol): Symbol = {
      if (hasFlag(PRIVATE) || isLocalToBlock) owner
      else if (hasAllFlags(PROTECTED | STATIC | JAVA)) enclosingRootClass
      else if (hasAccessBoundary && !phase.erasedTypes) privateWithin
      else if (hasFlag(PROTECTED)) base
      else enclosingRootClass
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
    def privateWithin = {
      // See `getFlag` to learn more about the `isThreadsafe` call in the body of this method.
      if (!isCompilerUniverse && !isThreadsafe(purpose = AllOps)) initialize
      _privateWithin
    }
    def privateWithin_=(sym: Symbol) { _privateWithin = sym }
    def setPrivateWithin(sym: Symbol): this.type = { privateWithin_=(sym) ; this }

    /** Does symbol have a private or protected qualifier set? */
    final def hasAccessBoundary = (privateWithin != null) && (privateWithin != NoSymbol)

// ------ info and type -------------------------------------------------------------------

    private[Symbols] var infos: TypeHistory = null
    def originalInfo = {
      if (infos eq null) null
      else {
        var is = infos
        while (is.prev ne null) { is = is.prev }
        is.info
      }
    }

    /** The "type" of this symbol.  The type of a term symbol is its usual
     *  type.  A TypeSymbol is more complicated; see that class for elaboration.
     *  Since tpe forwards to tpe_*, if you call it on a type symbol with unapplied
     *  type parameters, the type returned will contain dummies types.  These will
     *  hide legitimate errors or create spurious ones if used as normal types.
     *
     *  For type symbols, `tpe` is different than `info`. `tpe` returns a typeRef
     *  to the type symbol, `info` returns the type information of the type symbol,
     *  e.g. a ClassInfoType for classes or a TypeBounds for abstract types.
     */
    final def tpe: Type = tpe_*

    /** typeConstructor throws an exception when called on term
     *  symbols; this is a more forgiving alternative.  Calls
     *  typeConstructor on TypeSymbols, returns info otherwise.
     */
    def tpeHK: Type = info

    /** Only applicable to TypeSymbols, it is the type corresponding
     *  to the symbol itself.  For instance, the type of a List might
     *  be List[Int] - the same symbol's typeConstructor is simply List.
     *  One might be tempted to write that as List[_], and in some
     *  contexts this is possible, but it is discouraged because it is
     *  syntactically indistinguishable from and easily confused with the
     *  type List[T] forSome { type T; }, which can also be written List[_].
     */
    def typeConstructor: Type = (
      // Avoiding a third override in NoSymbol to preserve bimorphism
      if (this eq NoSymbol)
        abort("no-symbol does not have a type constructor (this may indicate scalac cannot find fundamental classes)")
      else
        abort("typeConstructor inapplicable for " + this)
    )

    /** The type of this symbol, guaranteed to be of kind *.
     *  If there are unapplied type parameters, they will be
     *  substituted with dummy type arguments derived from the
     *  type parameters.  Such types are not valid in a general
     *  sense and will cause difficult-to-find bugs if allowed
     *  to roam free.
     *
     *  If you call tpe_* explicitly to obtain these types,
     *  you are responsible for them as if it they were your own
     *  minor children.
     */
    def tpe_* : Type = info

    // Alternate implementation of def tpe for warning about misuse,
    // disabled to keep the method maximally hotspot-friendly:
    // def tpe: Type = {
    //   val result = tpe_*
    //   if (settings.debug.value && result.typeArgs.nonEmpty)
    //     printCaller(s"""Call to ${this.tpe} created $result: call tpe_* or tpeHK""")("")
    //   result
    // }

    /** Get type info associated with symbol at current phase, after
     *  ensuring that symbol is initialized (i.e. type is completed).
     */
    def info: Type = try {
      var cnt = 0
      while (validTo == NoPeriod) {
        assert(infos ne null, this.name)
        assert(infos.prev eq null, this.name)
        val tp = infos.info

        if ((_rawflags & LOCKED) != 0L) { // rolled out once for performance
          lock {
            setInfo(ErrorType)
            throw CyclicReference(this, tp)
          }
        } else {
          _rawflags |= LOCKED
          // TODO another commented out lines - this should be solved in one way or another
//          activeLocks += 1
 //         lockedSyms += this
        }
        val current = phase
        try {
          assertCorrectThread()
          phase = phaseOf(infos.validFrom)
          tp.complete(this)
        } finally {
          unlock()
          phase = current
        }
        cnt += 1
        // allow for two completions:
        //   one: sourceCompleter to LazyType, two: LazyType to completed type
        if (cnt == 3) abort(s"no progress in completing $this: $tp")
      }
      rawInfo
    }
    catch {
      case ex: CyclicReference =>
        devWarning("... hit cycle trying to complete " + this.fullLocationString)
        throw ex
    }

    def info_=(info: Type) {
      assert(info ne null)
      infos = TypeHistory(currentPeriod, info, null)
      unlock()
      _validTo = if (info.isComplete) currentPeriod else NoPeriod
    }

    /** Set initial info. */
    def setInfo(info: Type): this.type  = { info_=(info); this }
    /** Modifies this symbol's info in place. */
    def modifyInfo(f: Type => Type): this.type = setInfo(f(info))
    /** Substitute second list of symbols for first in current info. */
    def substInfo(syms0: List[Symbol], syms1: List[Symbol]): this.type =
      if (syms0.isEmpty) this
      else modifyInfo(_.substSym(syms0, syms1))

    def setInfoOwnerAdjusted(info: Type): this.type = setInfo(info atOwner this)

    /** Set the info and enter this symbol into the owner's scope. */
    def setInfoAndEnter(info: Type): this.type = {
      setInfo(info)
      owner.info.decls enter this
      this
    }

    /** Set new info valid from start of this phase. */
    def updateInfo(info: Type): Symbol = {
      val pid = phaseId(infos.validFrom)
      assert(pid <= phase.id, (pid, phase.id))
      if (pid == phase.id) infos = infos.prev
      infos = TypeHistory(currentPeriod, info, infos)
      _validTo = if (info.isComplete) currentPeriod else NoPeriod
      this
    }

    def hasRawInfo: Boolean = infos ne null
    def hasCompleteInfo = hasRawInfo && rawInfo.isComplete

    // does not run adaptToNewRun, which is prone to trigger cycles (SI-8029)
    // TODO: give this a better name if you understand the intent of the caller.
    //       Is it something to do with `reallyExists` or `isStale`?
    final def rawInfoIsNoType: Boolean = {
      hasRawInfo && (infos.info eq NoType)
    }

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
          assertCorrectThread()
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
                _validTo = currentPeriod + 1 // to enable reads from same symbol during info-transform
                itr = itr.next
              }
              _validTo = if (itr.pid == NoPhase.id) curPeriod
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
    private def adaptInfos(infos: TypeHistory): TypeHistory = {
      assert(isCompilerUniverse)
      if (infos == null || runId(infos.validFrom) == currentRunId) {
        infos
      } else if (infos ne infos.oldest) {
        // SI-8871 Discard all but the first element of type history. Specialization only works in the resident
        // compiler / REPL if re-run its info transformer in this run to correctly populate its
        // per-run caches, e.g. typeEnv
        adaptInfos(infos.oldest)
      } else {
        val prev1 = adaptInfos(infos.prev)
        if (prev1 ne infos.prev) prev1
        else {
          val pid = phaseId(infos.validFrom)

          _validTo = period(currentRunId, pid)
          phase   = phaseWithId(pid)

          val info1 = adaptToNewRunMap(infos.info)
          if (info1 eq infos.info) {
            infos.validFrom = validTo
            infos
          } else {
            this.infos = TypeHistory(validTo, info1, prev1)
            this.infos
          }
        }
      }
    }

    /** Raises a `MissingRequirementError` if this symbol is a `StubSymbol` */
    def failIfStub() {}

    /** Initialize the symbol */
    final def initialize: this.type = {
      if (!isInitialized) info
      this
    }
    def maybeInitialize = {
      try   { initialize ; true }
      catch { case _: CyclicReference => debuglog("Hit cycle in maybeInitialize of $this") ; false }
    }

    /** Was symbol's type updated during given phase? */
    final def hasTypeAt(pid: Phase#Id): Boolean = {
      assert(isCompilerUniverse)
      var infos = this.infos
      while ((infos ne null) && phaseId(infos.validFrom) > pid) infos = infos.prev
      infos ne null
    }

    /** Modify term symbol's type so that a raw type C is converted to an existential C[_]
     *
     * This is done in checkAccessible and overriding checks in refchecks
     * We can't do this on class loading because it would result in infinite cycles.
     */
    def cookJavaRawInfo(): this.type = {
      // only try once...
      if (phase.erasedTypes || (this hasFlag TRIEDCOOKING))
        return this

      this setFlag TRIEDCOOKING
      info  // force the current info
      if (isJavaDefined || isType && owner.isJavaDefined)
        this modifyInfo rawToExistential
      else if (isOverloaded)
        alternatives withFilter (_.isJavaDefined) foreach (_ modifyInfo rawToExistential)

      this
    }

    /** The logic approximately boils down to finding the most recent phase
     *  which immediately follows any of parser, namer, typer, or erasure.
     *  In effect that means this will return one of:
     *
     *    - packageobjects (follows namer)
     *    - superaccessors (follows typer)
     *    - lazyvals       (follows erasure)
     *    - null
     */
    private def unsafeTypeParamPhase = {
      var ph = phase
      while (ph.prev.keepsTypeParams)
        ph = ph.prev

      ph
    }
    /** The type parameters of this symbol, without ensuring type completion.
     *  assumption: if a type starts out as monomorphic, it will not acquire
     *  type parameters later.
     */
    // NOTE: overridden in SynchronizedSymbols with the code copy/pasted
    // don't forget to modify the code over there if you modify this method
    def unsafeTypeParams: List[Symbol] =
      if (isMonomorphicType) Nil
      else enteringPhase(unsafeTypeParamPhase)(rawInfo.typeParams)

    /** The type parameters of this symbol.
     *  assumption: if a type starts out as monomorphic, it will not acquire
     *  type parameters later.
     */
    // NOTE: overridden in SynchronizedSymbols with the code copy/pasted
    // don't forget to modify the code over there if you modify this method
    def typeParams: List[Symbol] =
      if (isMonomorphicType) Nil
      else {
        // analogously to the "info" getter, here we allow for two completions:
        //   one: sourceCompleter to LazyType, two: LazyType to completed type
        if (validTo == NoPeriod)
          enteringPhase(phaseOf(infos.validFrom))(rawInfo load this)
        if (validTo == NoPeriod)
          enteringPhase(phaseOf(infos.validFrom))(rawInfo load this)

        rawInfo.typeParams
      }

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
      // SI-4589 refinedType only creates a new refinement class symbol before erasure; afterwards
      //         the first parent class is returned, to which we must not add members.
      if (!phase.erasedTypes) {
        val thistp = tp.typeSymbol.thisType
        val oldsymbuf = new ListBuffer[Symbol]
        val newsymbuf = new ListBuffer[Symbol]
        for (sym <- info.decls) {
          // todo: what about public references to private symbols?
          if (sym.isPublic && !sym.isConstructor) {
            oldsymbuf += sym
            newsymbuf += (
              if (sym.isClass)
                tp.typeSymbol.newAbstractType(sym.name.toTypeName, sym.pos).setInfo(sym.existentialBound)
              else
                sym.cloneSymbol(tp.typeSymbol))
          }
        }
        val oldsyms = oldsymbuf.toList
        val newsyms = newsymbuf.toList
        for (sym <- newsyms) {
          addMember(thistp, tp, sym modifyInfo (_ substThisAndSym(this, thistp, oldsyms, newsyms)))
        }
      }
      tp
    }

    /** If we quantify existentially over this symbol,
     *  the bound of the type variable that stands for it
     *  pre: symbol is a term, a class, or an abstract type (no alias type allowed)
     */
    def existentialBound: Type

    /** Reset symbol to initial state
     */
    def reset(completer: Type): this.type = {
      resetFlags()
      infos = null
      _validTo = NoPeriod
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
          setInfo(ci.copy(parents = ci.parents :+ SerializableTpe))
        case i =>
          abort("Only ClassInfoTypes can be made serializable: "+ i)
      }
    }

// ----- setters implemented in selected subclasses -------------------------------------

    def typeOfThis_=(tp: Type)       { throw new UnsupportedOperationException("typeOfThis_= inapplicable for " + this) }
    def sourceModule_=(sym: Symbol)  { throw new UnsupportedOperationException("sourceModule_= inapplicable for " + this) }
    def addChild(sym: Symbol)        { throw new UnsupportedOperationException("addChild inapplicable for " + this) }

// ----- annotations ------------------------------------------------------------

    // null is a marker that they still need to be obtained.
    private[this] var _annotations: List[AnnotationInfo] = Nil

    def annotationsString = if (annotations.isEmpty) "" else annotations.mkString("(", ", ", ")")

    /** After the typer phase (before, look at the definition's Modifiers), contains
     *  the annotations attached to member a definition (class, method, type, field).
     */
    def annotations: List[AnnotationInfo] = {
      // See `getFlag` to learn more about the `isThreadsafe` call in the body of this method.
      if (!isCompilerUniverse && !isThreadsafe(purpose = AllOps)) initialize
      _annotations
    }

    def setAnnotations(annots: List[AnnotationInfo]): this.type = {
      _annotations = annots
      this
    }

    def withAnnotations(annots: List[AnnotationInfo]): this.type =
      setAnnotations(annots ::: annotations)

    def withoutAnnotations: this.type =
      setAnnotations(Nil)

    def filterAnnotations(p: AnnotationInfo => Boolean): this.type =
      setAnnotations(annotations filter p)

    def addAnnotation(annot: AnnotationInfo): this.type =
      setAnnotations(annot :: annotations)

    // Convenience for the overwhelmingly common case
    def addAnnotation(sym: Symbol, args: Tree*): this.type = {
      // The assertion below is meant to prevent from issues like SI-7009 but it's disabled
      // due to problems with cycles while compiling Scala library. It's rather shocking that
      // just checking if sym is monomorphic type introduces nasty cycles. We are definitively
      // forcing too much because monomorphism is a local property of a type that can be checked
      // syntactically
      // assert(sym.initialize.isMonomorphicType, sym)
      addAnnotation(AnnotationInfo(sym.tpe, args.toList, Nil))
    }

    /** Use that variant if you want to pass (for example) an applied type */
    def addAnnotation(tp: Type, args: Tree*): this.type = {
      assert(tp.typeParams.isEmpty, tp)
      addAnnotation(AnnotationInfo(tp, args.toList, Nil))
    }

// ------ comparisons ----------------------------------------------------------------

    /** A total ordering between symbols that refines the class
     *  inheritance graph (i.e. subclass.isLess(superclass) always holds).
     *  the ordering is given by: (_.isType, -_.baseTypeSeq.length) for type symbols, followed by `id`.
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

    /** Is this class symbol a subclass of that symbol,
     *  and is this class symbol also different from Null or Nothing? */
    def isNonBottomSubClass(that: Symbol): Boolean = false

    /** Is this class symbol Null or Nothing,
     *  and (if Null) is `that` inhabited by null?
     *  If this is Nothing, of course, it is a
     *  subclass of `that` by definition.
     *
     *  TODO - what is implied by the fact that AnyVal now has
     *  infinitely many non-bottom subclasses, not only 9?
     */
    def isBottomSubClass(that: Symbol) = (
         (this eq NothingClass)
      || (this eq NullClass) && that.isClass && (that ne NothingClass) && !(that isNonBottomSubClass AnyValClass)
    )

    /** Overridden in NullClass and NothingClass for custom behavior.
     */
    def isSubClass(that: Symbol) = isNonBottomSubClass(that)

    final def isNumericSubClass(that: Symbol): Boolean =
      definitions.isNumericSubClass(this, that)

    final def isWeakSubClass(that: Symbol) =
      isSubClass(that) || isNumericSubClass(that)

// ------ overloaded alternatives ------------------------------------------------------

    def alternatives: List[Symbol] =
      if (isOverloaded) info.asInstanceOf[OverloadedType].alternatives
      else this :: Nil

    def filter(cond: Symbol => Boolean): Symbol =
      if (isOverloaded) {
        var changed = false
        var alts0: List[Symbol] = alternatives
        var alts1: List[Symbol] = Nil

        while (alts0.nonEmpty) {
          if (cond(alts0.head))
            alts1 ::= alts0.head
          else
            changed = true

          alts0 = alts0.tail
        }

        if (!changed) this
        else if (alts1.isEmpty) NoSymbol
        else if (alts1.tail.isEmpty) alts1.head
        else owner.newOverloaded(info.prefix, alts1.reverse)
      }
      else if (cond(this)) this
      else NoSymbol

    def suchThat(cond: Symbol => Boolean): Symbol = {
      val result = filter(cond)
      assert(!result.isOverloaded, result.alternatives)
      result
    }

// ------ cloneing -------------------------------------------------------------------

    /** A clone of this symbol. */
    final def cloneSymbol: TypeOfClonedSymbol =
      cloneSymbol(owner)

    /** A clone of this symbol, but with given owner. */
    final def cloneSymbol(newOwner: Symbol): TypeOfClonedSymbol =
      cloneSymbol(newOwner, _rawflags)
    final def cloneSymbol(newOwner: Symbol, newFlags: Long): TypeOfClonedSymbol =
      cloneSymbol(newOwner, newFlags, null)
    final def cloneSymbol(newOwner: Symbol, newFlags: Long, newName: Name): TypeOfClonedSymbol = {
      val clone = cloneSymbolImpl(newOwner, newFlags)
      ( clone
          setPrivateWithin privateWithin
          setInfo (this.info cloneInfo clone)
          setAnnotations this.annotations
      )
      this.attachments.all.foreach(clone.updateAttachment)
      if (clone.thisSym != clone)
        clone.typeOfThis = (clone.typeOfThis cloneInfo clone)

      if (newName ne null)
        clone setName asNameType(newName)

      clone
    }

    /** Internal method to clone a symbol's implementation with the given flags and no info. */
    def cloneSymbolImpl(owner: Symbol, newFlags: Long): TypeOfClonedSymbol

// ------ access to related symbols --------------------------------------------------

    /** The next enclosing class. */
    def enclClass: Symbol = if (isClass) this else owner.enclClass

    /** The next enclosing method. */
    def enclMethod: Symbol = if (isSourceMethod) this else owner.enclMethod

    /** The primary constructor of a class. */
    def primaryConstructor: Symbol = NoSymbol

    /** The self symbol (a TermSymbol) of a class with explicit self type, or else the
     *  symbol itself (a TypeSymbol).
     *
     *  WARNING: you're probably better off using typeOfThis, as it's more uniform across classes with and without self variables.
     *
     *  Example by Paul:
     *   scala> trait Foo1 { }
     *   scala> trait Foo2 { self => }
     *   scala> intp("Foo1").thisSym
     *   res0: $r.intp.global.Symbol = trait Foo1
     *
     *   scala> intp("Foo2").thisSym
     *   res1: $r.intp.global.Symbol = value self
     *
     *  Martin says: The reason `thisSym' is `this' is so that thisType can be this.thisSym.tpe.
     *  It's a trick to shave some cycles off.
     *
     *  Morale: DO:    if (clazz.typeOfThis.typeConstructor ne clazz.typeConstructor) ...
     *          DON'T: if (clazz.thisSym ne clazz) ...
     *
     */
    def thisSym: Symbol = this

    def hasSelfType = thisSym.tpeHK != this.tpeHK

    /** The type of `this` in a class, or else the type of the symbol itself. */
    def typeOfThis = thisSym.tpe_*

    /** If symbol is a class, the type `this.type` in this class,
     * otherwise `NoPrefix`.
     * We always have: thisType <:< typeOfThis
     */
    def thisType: Type = NoPrefix

    /** For a case class, the symbols of the accessor methods, one for each
     *  argument in the first parameter list of the primary constructor.
     *  The empty list for all other classes.
     *
     * This list will be sorted to correspond to the declaration order
     * in the constructor parameter
     */
    final def caseFieldAccessors: List[Symbol] = {
      // We can't rely on the ordering of the case field accessors within decls --
      // handling of non-public parameters seems to change the order (see SI-7035.)
      //
      // Luckily, the constrParamAccessors are still sorted properly, so sort the field-accessors using them
      // (need to undo name-mangling, including the sneaky trailing whitespace)
      //
      // The slightly more principled approach of using the paramss of the
      // primary constructor leads to cycles in, for example, pos/t5084.scala.
      val primaryNames = constrParamAccessors map (_.name.dropLocal)
      caseFieldAccessorsUnsorted.sortBy { acc =>
        primaryNames indexWhere { orig =>
          (acc.name == orig) || (acc.name startsWith (orig append "$"))
        }
      }
    }
    private final def caseFieldAccessorsUnsorted: List[Symbol] =
      (info.decls filter (_.isCaseAccessorMethod)).toList

    final def constrParamAccessors: List[Symbol] =
      info.decls.filter(sym => !sym.isMethod && sym.isParamAccessor).toList

    /** The symbol accessed by this accessor (getter or setter) function. */
    final def accessed: Symbol = {
      assert(hasAccessorFlag, this)
      val localField = owner.info decl localName

      if (localField == NoSymbol && this.hasFlag(MIXEDIN)) {
        // SI-8087: private[this] fields don't have a `localName`. When searching the accessed field
        // for a mixin accessor of such a field, we need to look for `name` instead.
        // The phase travel ensures that the field is found (`owner` is the trait class symbol, the
        // field gets removed from there in later phases).
        enteringPhase(picklerPhase)(owner.info).decl(name).suchThat(!_.isAccessor)
      } else {
        localField
      }
    }

    /** The module corresponding to this module class (note that this
     *  is not updated when a module is cloned), or NoSymbol if this is not a ModuleClass.
     */
    def sourceModule: Symbol = NoSymbol

    /** The implementation class of a trait.  If available it will be the
     *  symbol with the same owner, and the name of this symbol with $class
     *  appended to it.
     */
    final def implClass: Symbol = owner.info.decl(tpnme.implClassName(name))

    /** The class that is logically an outer class of given `clazz`.
     *  This is the enclosing class, except for classes defined locally to constructors,
     *  where it is the outer class of the enclosing class.
     */
    final def outerClass: Symbol =
      if (this == NoSymbol) {
        // ideally we shouldn't get here, but it's better to harden against this than suffer the infinite loop in SI-9133
        devWarningDumpStack("NoSymbol.outerClass", 15)
        NoSymbol
      } else if (owner.isClass) owner
      else if (isClassLocalToConstructor) owner.enclClass.outerClass
      else owner.outerClass

    /** For a paramaccessor: a superclass paramaccessor for which this symbol
     *  is an alias, NoSymbol for all others.
     */
    def alias: Symbol = NoSymbol

    /** For a lazy value, its lazy accessor. NoSymbol for all others. */
    def lazyAccessor: Symbol = NoSymbol

    /** If this is a lazy value, the lazy accessor; otherwise this symbol. */
    def lazyAccessorOrSelf: Symbol = if (isLazy) lazyAccessor else this

    /** If this is an accessor, the accessed symbol.  Otherwise, this symbol. */
    def accessedOrSelf: Symbol = if (hasAccessorFlag) accessed else this

    /** For an outer accessor: The class from which the outer originates.
     *  For all other symbols: NoSymbol
     */
    def outerSource: Symbol = NoSymbol

    /** The superclass of this class. */
    def superClass: Symbol = if (info.parents.isEmpty) NoSymbol else info.parents.head.typeSymbol
    def parentSymbols: List[Symbol] = info.parents map (_.typeSymbol)

    /** The directly or indirectly inherited mixins of this class
     *  except for mixin classes inherited by the superclass. Mixin classes appear
     *  in linearization order.
     */
    def mixinClasses: List[Symbol] = {
      val sc = superClass
      ancestors takeWhile (sc ne _)
    }

    /** All directly or indirectly inherited classes. */
    def ancestors: List[Symbol] = info.baseClasses drop 1

    @inline final def enclosingSuchThat(p: Symbol => Boolean): Symbol = {
      var sym = this
      while (sym != NoSymbol && !p(sym))
        sym = sym.owner
      sym
    }

    /** The package class containing this symbol, or NoSymbol if there
     *  is not one.
     *  TODO: formulate as enclosingSuchThat, after making sure
     *        we can start with current symbol rather than owner.
     *  TODO: Also harmonize with enclClass, enclMethod etc.
     */
    def enclosingPackageClass: Symbol = {
      var sym = this.owner
      while (sym != NoSymbol && !sym.isPackageClass)
        sym = sym.owner
      sym
    }

    /** The package class containing this symbol, or NoSymbol if there
     *  is not one. */
    def enclosingRootClass: Symbol = enclosingSuchThat(_.isRoot)

    /** The package containing this symbol, or NoSymbol if there
     *  is not one. */
    def enclosingPackage: Symbol = enclosingPackageClass.companionModule

    /** The method or class which logically encloses the current symbol.
     *  If the symbol is defined in the initialization part of a template
     *  this is the template's primary constructor, otherwise it is
     *  the physically enclosing method or class.
     *
     *  Example 1:
     *
     *  def f() { val x = { def g() = ...; g() } }
     *
     *  In this case the owner chain of `g` is `x`, followed by `f` and
     *  `g.logicallyEnclosingMember == f`.
     *
     *  Example 2:
     *
     *  class C {
     *    def <init> = { ... }
     *    val x = { def g() = ...; g() } }
     *  }
     *
     *  In this case the owner chain of `g` is `x`, followed by `C` but
     *  g.logicallyEnclosingMember is the primary constructor symbol `<init>`
     *  (or, for traits: `$init`) of `C`.
     *
     */
    final def logicallyEnclosingMember: Symbol =
      if (isLocalDummy) enclClass.primaryConstructor
      else if (isMethod || isClass || this == NoSymbol) this
      else if (this == NoSymbol) { devWarningDumpStack("NoSymbol.logicallyEnclosingMember", 15); this }
      else owner.logicallyEnclosingMember

    /** The top-level class containing this symbol. */
    def enclosingTopLevelClass: Symbol =
      if (isTopLevel) {
        if (isClass) this else moduleClass
      } else owner.enclosingTopLevelClass

    /** The top-level class or local dummy symbol containing this symbol. */
    def enclosingTopLevelClassOrDummy: Symbol =
      if (isTopLevel) {
        if (isClass) this else moduleClass.orElse(this)
      } else owner.enclosingTopLevelClassOrDummy

    /** Is this symbol defined in the same scope and compilation unit as `that` symbol? */
    def isCoDefinedWith(that: Symbol) = (
         !rawInfoIsNoType
      && (this.effectiveOwner == that.effectiveOwner)
      && (   !this.effectiveOwner.isPackageClass
          || (this.associatedFile eq NoAbstractFile)
          || (that.associatedFile eq NoAbstractFile)
          || (this.associatedFile.path == that.associatedFile.path)  // Cheap possibly wrong check, then expensive normalization
          || (this.associatedFile.canonicalPath == that.associatedFile.canonicalPath)
         )
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

    /** For a module: the class with the same name in the same package.
     *  For all others: NoSymbol
     *  Note: does not work for classes owned by methods, see Namers.companionClassOf
     *
     *  object Foo  .  companionClass -->  class Foo
     *
     *  !!! linkedClassOfClass depends on companionClass on the module class getting
     *  to the class.  As presently implemented this potentially returns class for
     *  any symbol except NoSymbol.
     */
    def companionClass: Symbol = flatOwnerInfo.decl(name.toTypeName).suchThat(_ isCoDefinedWith this)

    /** For a class: the module or case class factory with the same name in the same package.
     *  For all others: NoSymbol
     *  Note: does not work for modules owned by methods, see Namers.companionModuleOf
     *
     *  class Foo  .  companionModule -->  object Foo
     */
    def companionModule: Symbol = NoSymbol

    /** For a module: its linked class
     *  For a plain class: its linked module or case factory.
     *  Note: does not work for modules owned by methods, see Namers.companionSymbolOf
     *
     *  class Foo  <-- companionSymbol -->  object Foo
     */
    def companionSymbol: Symbol = NoSymbol

    /** For a module class: its linked class
     *   For a plain class: the module class of its linked module.
     *
     *  class Foo  <-- linkedClassOfClass -->  class Foo$
     */
    def linkedClassOfClass: Symbol = NoSymbol

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
    protected final def flatOwnerInfo: Type = {
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
     *     !!! Why?
     */
    def toInterface: Symbol = this

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
      matchingSymbolInternal(site, ofclazz.info nonPrivateDecl name)

    /** The non-private member of `site` whose type and name match the type of this symbol. */
    final def matchingSymbol(site: Type, admit: Long = 0L): Symbol =
      matchingSymbolInternal(site, site.nonPrivateMemberAdmitting(name, admit))

    private def matchingSymbolInternal(site: Type, candidate: Symbol): Symbol = {
      def qualifies(sym: Symbol) = !sym.isTerm || ((site memberType this) matches (site memberType sym))
      //OPT cut down on #closures by special casing non-overloaded case
      if (candidate.isOverloaded) candidate filter qualifies
      else if (qualifies(candidate)) candidate
      else NoSymbol
    }

    /** The symbol, in class `baseClass`, that is overridden by this symbol.
     *
     *  @param baseClass is a base class of this symbol's owner.
     */
    final def overriddenSymbol(baseClass: Symbol): Symbol = (
      // concrete always overrides abstract, so don't let an abstract definition
      // claim to be overriding an inherited concrete one.
      matchingInheritedSymbolIn(baseClass) filter (res => res.isDeferred || !this.isDeferred)
    )

    private def matchingInheritedSymbolIn(baseClass: Symbol): Symbol =
      if (canMatchInheritedSymbols) matchingSymbol(baseClass, owner.thisType) else NoSymbol

    /** The symbol overriding this symbol in given subclass `ofclazz`.
     *
     *  @param ofclazz is a subclass of this symbol's owner
     */
    final def overridingSymbol(ofclazz: Symbol): Symbol = (
      if (canMatchInheritedSymbols)
        matchingSymbol(ofclazz, ofclazz.thisType)
      else
        NoSymbol
    )

    /** If false, this symbol cannot possibly participate in an override,
     *  either as overrider or overridee. For internal use; you should consult
     *  with isOverridingSymbol. This is used by isOverridingSymbol to escape
     *  the recursive knot.
     */
    private def canMatchInheritedSymbols = (
         owner.isClass
      && !this.isClass
      && !this.isConstructor
    )

    // All the symbols overridden by this symbol and this symbol at the head,
    // or Nil if this is NoSymbol.
    def overrideChain = (
      if (this eq NoSymbol) Nil
      else if (isOverridingSymbol) this :: allOverriddenSymbols
      else this :: Nil
    )

    /** Returns all symbols overridden by this symbol. */
    final def allOverriddenSymbols: List[Symbol] = {
      def loop(xs: List[Symbol]): List[Symbol] = xs match {
        case Nil     => Nil
        case x :: xs =>
          overriddenSymbol(x) match {
            case NoSymbol => loop(xs)
            case sym      => sym :: loop(xs)
          }
      }
      if (isOverridingSymbol) loop(owner.ancestors) else Nil
    }

    /** Equivalent to allOverriddenSymbols.nonEmpty, but more efficient. */
    lazy val isOverridingSymbol = (
         canMatchInheritedSymbols
      && owner.ancestors.exists(base => overriddenSymbol(base) != NoSymbol)
    )

    /** Equivalent to allOverriddenSymbols.head (or NoSymbol if no overrides) but more efficient. */
    def nextOverriddenSymbol: Symbol = {
      @tailrec def loop(bases: List[Symbol]): Symbol = bases match {
        case Nil          => NoSymbol
        case base :: rest =>
          val sym = overriddenSymbol(base)
          if (sym == NoSymbol) loop(rest) else sym
      }
      if (isOverridingSymbol) loop(owner.ancestors) else NoSymbol
    }

    /** Returns all symbols overridden by this symbol, plus all matching symbols
     *  defined in parents of the selftype.
     */
    final def extendedOverriddenSymbols: List[Symbol] = (
      if (canMatchInheritedSymbols)
        owner.thisSym.ancestors map overriddenSymbol filter (_ != NoSymbol)
      else
        Nil
    )

    @deprecated("Use `superSymbolIn` instead", "2.11.0")
    final def superSymbol(base: Symbol): Symbol = superSymbolIn(base)

    /** The symbol accessed by a super in the definition of this symbol when
     *  seen from class `base`. This symbol is always concrete.
     *  pre: `this.owner` is in the base class sequence of `base`.
     */
    final def superSymbolIn(base: Symbol): Symbol = {
      var bcs = base.info.baseClasses dropWhile (owner != _) drop 1
      var sym: Symbol = NoSymbol
      while (!bcs.isEmpty && sym == NoSymbol) {
        if (!bcs.head.isImplClass)
          sym = matchingSymbol(bcs.head, base.thisType).suchThat(!_.isDeferred)
        bcs = bcs.tail
      }
      sym
    }

    @deprecated("Use `getterIn` instead", "2.11.0")
    final def getter(base: Symbol): Symbol = getterIn(base)

    /** The getter of this value or setter definition in class `base`, or NoSymbol if none exists. */
    final def getterIn(base: Symbol): Symbol =
      base.info decl getterName filter (_.hasAccessorFlag)

    def getterName: TermName = name.getterName
    def setterName: TermName = name.setterName
    def localName: TermName  = name.localName

    @deprecated("Use `setterIn` instead", "2.11.0")
    final def setter(base: Symbol, hasExpandedName: Boolean = needsExpandedSetterName): Symbol =
      setterIn(base, hasExpandedName)

    /** The setter of this value or getter definition, or NoSymbol if none exists. */
    final def setterIn(base: Symbol, hasExpandedName: Boolean = needsExpandedSetterName): Symbol =
      base.info decl setterNameInBase(base, hasExpandedName) filter (_.hasAccessorFlag)

    def needsExpandedSetterName = (
      if (isMethod) hasStableFlag && !isLazy
      else hasNoFlags(LAZY | MUTABLE)
    )
    def setterNameInBase(base: Symbol, expanded: Boolean): TermName =
      if (expanded) nme.expandedSetterName(setterName, base) else setterName

    /** If this is a derived value class, return its unbox method
     *  or NoSymbol if it does not exist.
     */
    def derivedValueClassUnbox: Symbol = NoSymbol

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

    /** Remove private modifier from symbol `sym`s definition. If `sym` is a
     *  is not a constructor nor a static module rename it by expanding its name to avoid name clashes
     *  @param base  the fully qualified name of this class will be appended if name expansion is needed
     */
    final def makeNotPrivate(base: Symbol) {
      if (this.isPrivate) {
        setFlag(notPRIVATE)
        // Marking these methods final causes problems for proxies which use subclassing. If people
        // write their code with no usage of final, we probably shouldn't introduce it ourselves
        // unless we know it is safe. ... Unfortunately if they aren't marked final the inliner
        // thinks it can't inline them. So once again marking lateFINAL, and in genjvm we no longer
        // generate ACC_FINAL on "final" methods which are actually lateFINAL.
        if (isMethod && !isDeferred)
          setFlag(lateFINAL)
        if (!isStaticModule && !isClassConstructor) {
          expandName(base)
          if (isModule) moduleClass.makeNotPrivate(base)
        }
      }
    }

    /** Remove any access boundary and clear flags PROTECTED | PRIVATE.
     */
    def makePublic = this setPrivateWithin NoSymbol resetFlag AccessFlags

    /** The first parameter to the first argument list of this method,
     *  or NoSymbol if inapplicable.
     */
    def firstParam = info.params match {
      case p :: _ => p
      case _      => NoSymbol
    }

    // Desire to re-use the field in ClassSymbol which stores the source
    // file to also store the classfile, but without changing the behavior
    // of sourceFile (which is expected at least in the IDE only to
    // return actual source code.) So sourceFile has classfiles filtered out.
    final def sourceFile: AbstractFile =
      if ((associatedFile eq NoAbstractFile) || (associatedFile.path endsWith ".class")) null else associatedFile

    /** Overridden in ModuleSymbols to delegate to the module class.
     *  Never null; if there is no associated file, returns NoAbstractFile.
     */
    def associatedFile: AbstractFile = enclosingTopLevelClass.associatedFile
    def associatedFile_=(f: AbstractFile) { abort("associatedFile_= inapplicable for " + this) }

    /** If this is a sealed class, its known direct subclasses.
     *  Otherwise, the empty set.
     */
    def children: Set[Symbol] = Set()

    /** Recursively assemble all children of this symbol.
     */
    def sealedDescendants: Set[Symbol] = children.flatMap(_.sealedDescendants) + this

    @inline final def orElse(alt: => Symbol): Symbol = if (this ne NoSymbol) this else alt
    @inline final def andAlso(f: Symbol => Unit): Symbol = { if (this ne NoSymbol) f(this) ; this }
    @inline final def fold[T](none: => T)(f: Symbol => T): T = if (this ne NoSymbol) f(this) else none
    @inline final def map(f: Symbol => Symbol): Symbol = if (this eq NoSymbol) this else f(this)

    final def toOption: Option[Symbol] = if (exists) Some(this) else None


// ------ toString -------------------------------------------------------------------

    /** The simple name of this Symbol */
    final def simpleName: Name = name

    /** The String used to order otherwise identical sealed symbols.
     *  This uses data which is stable across runs and variable classpaths
     *  (the initial Name) before falling back on id, which varies depending
     *  on exactly when a symbol is loaded.
     */
    final def sealedSortName: String = initName + "#" + id

    /** String representation of symbol's definition key word */
    final def keyString: String =
      if (isJavaInterface) "interface"
      else if (isTrait && !isImplClass) "trait"
      else if (isClass) "class"
      else if (isType && !isParameter) "type"
      else if (isVariable) "var"
      else if (hasPackageFlag) "package"
      else if (isModule) "object"
      else if (isSourceMethod) "def"
      else if (isTerm && (!isParameter || isParamAccessor)) "val"
      else ""

    private def symbolKind: SymbolKind = {
      var kind =
        if (isTermMacro) ("term macro", "macro method", "MACM")
        else if (isInstanceOf[FreeTermSymbol]) ("free term", "free term", "FTE")
        else if (isInstanceOf[FreeTypeSymbol]) ("free type", "free type", "FTY")
        else if (isPackageClass) ("package class", "package", "PKC")
        else if (hasPackageFlag) ("package", "package", "PK")
        else if (isPackageObject) ("package object", "package", "PKO")
        else if (isPackageObjectClass) ("package object class", "package", "PKOC")
        else if (isAnonymousClass) ("anonymous class", "anonymous class", "AC")
        else if (isRefinementClass) ("refinement class", "", "RC")
        else if (isModule) ("module", "object", "MOD")
        else if (isModuleClass) ("module class", "object", "MODC")
        else if (isGetter) ("getter", if (isSourceMethod) "method" else "value", "GET")
        else if (isSetter) ("setter", if (isSourceMethod) "method" else "value", "SET")
        else if (isTerm && isLazy) ("lazy value", "lazy value", "LAZ")
        else if (isVariable) ("field", "variable", "VAR")
        else if (isImplClass) ("implementation class", "class", "IMPL")
        else if (isTrait) ("trait", "trait", "TRT")
        else if (isClass) ("class", "class", "CLS")
        else if (isType) ("type", "type", "TPE")
        else if (isClassConstructor && (owner.hasCompleteInfo && isPrimaryConstructor)) ("primary constructor", "constructor", "PCTOR")
        else if (isClassConstructor) ("constructor", "constructor", "CTOR")
        else if (isSourceMethod) ("method", "method", "METH")
        else if (isTerm) ("value", "value", "VAL")
        else ("", "", "???")
      if (isSkolem) kind = (kind._1, kind._2, kind._3 + "#SKO")
      SymbolKind(kind._1, kind._2, kind._3)
    }

    /** Accurate string representation of symbols' kind, suitable for developers. */
    final def accurateKindString: String =
      symbolKind.accurate

    /** String representation of symbol's kind, suitable for the masses. */
    private def sanitizedKindString: String =
      symbolKind.sanitized

    /** String representation of symbol's kind, suitable for the masses. */
    protected[scala] def abbreviatedKindString: String =
      symbolKind.abbreviation

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
         isSetterParameter        // x$1
      || isClassConstructor       // this
      || isRefinementClass        // <refinement>
      || (name == nme.PACKAGE)    // package
    )

    /** String representation of symbol's simple name.
     *  If !settings.debug translates expansions of operators back to operator symbol.
     *  E.g. $eq => =.
     *  If settings.uniqid, adds id.
     *  If settings.Yshowsymowners, adds owner's id
     *  If settings.Yshowsymkinds, adds abbreviated symbol kind.
     */
    def nameString: String = {
      val name_s = if (settings.debug.value) "" + unexpandedName else unexpandedName.dropLocal.decode
      val kind_s = if (settings.Yshowsymkinds.value) "#" + abbreviatedKindString else ""

      name_s + idString + kind_s
    }

    def fullNameString: String = {
      def recur(sym: Symbol): String = {
        if (sym.isRootSymbol || sym == NoSymbol) sym.nameString
        else if (sym.owner.isEffectiveRoot) sym.nameString
        else recur(sym.effectiveOwner.enclClass) + "." + sym.nameString
      }

      recur(this)
    }

    /** If settings.uniqid is set, the symbol's id, else "" */
    final def idString = {
      val id_s = if (settings.uniqid.value) "#"+id else ""
      val owner_s = if (settings.Yshowsymowners.value) "@"+owner.id else ""
      id_s + owner_s
    }

    /** String representation, including symbol's kind e.g., "class Foo", "method Bar".
     *  If hasMeaninglessName is true, uses the owner's name to disambiguate identity.
     */
    override def toString: String = {
      if (isPackageObjectOrClass && !settings.debug)
        s"package object ${owner.decodedName}"
      else compose(
        kindString,
        if (hasMeaninglessName) owner.decodedName + idString else nameString
      )
    }

    /** String representation of location.
     */
    def ownsString: String = {
      val owns = effectiveOwner
      if (owns.isClass && !owns.isEmptyPrefix) "" + owns else ""
    }

    /** String representation of location, plus a preposition.  Doesn't do much,
     *  for backward compatibility reasons.
     */
    def locationString: String = ownsString match {
      case ""   => ""
      case s    => " in " + s
    }
    def fullLocationString: String = toString + locationString
    def signatureString: String    = if (hasRawInfo) infoString(rawInfo) else "<_>"

    /** String representation of symbol's definition following its name */
    final def infoString(tp: Type): String = {
      def parents = (
        if (settings.debug.value) parentsString(tp.parents)
        else briefParentsString(tp.parents)
      )
      def isStructuralThisType = (
        // prevents disasters like SI-8158
        owner.isInitialized && owner.isStructuralRefinement && tp == owner.tpe
      )
      if (isType) typeParamsString(tp) + (
        if (isClass) " extends " + parents
        else if (isAliasType) " = " + tp.resultType
        else tp.resultType match {
          case rt @ TypeBounds(_, _) => "" + rt
          case rt                    => " <: " + rt
        }
      )
      else if (isModule) "" //  avoid "object X of type X.type"
      else tp match {
        case PolyType(tparams, res)    => typeParamsString(tp) + infoString(res)
        case NullaryMethodType(res)    => infoString(res)
        case MethodType(params, res)   => valueParamsString(tp) + infoString(res)
        case _ if isStructuralThisType => ": " + owner.name
        case _                         => ": " + tp
      }
    }

    def infosString = infos.toString
    def debugLocationString = {
      val pre = flagString match {
        case ""                  => ""
        case s if s contains ' ' => "(" + s + ") "
        case s                   => s + " "
      }
      pre + fullLocationString
    }

    private def defStringCompose(infoString: String) = compose(
      flagString,
      keyString,
      varianceString + nameString + infoString + flagsExplanationString
    )
    /** String representation of symbol's definition.  It uses the
     *  symbol's raw info to avoid forcing types.
     */
    def defString = defStringCompose(signatureString)

    /** String representation of symbol's definition, using the supplied
     *  info rather than the symbol's.
     */
    def defStringSeenAs(info: Type) = defStringCompose(infoString(info))

    /** Concatenate strings separated by spaces */
    private def compose(ss: String*) = ss filter (_ != "") mkString " "

    def isSingletonExistential =
      nme.isSingletonName(name) && (info.bounds.hi.typeSymbol isSubClass SingletonClass)

    /** String representation of existentially bound variable */
    def existentialToString =
      if (isSingletonExistential && !settings.debug.value)
        "val " + tpnme.dropSingletonName(name) + ": " + dropSingletonType(info.bounds.hi)
      else defString
  }
  implicit val SymbolTag = ClassTag[Symbol](classOf[Symbol])

  /** A class for term symbols */
  class TermSymbol protected[Symbols] (initOwner: Symbol, initPos: Position, initName: TermName)
  extends Symbol(initOwner, initPos, initName) with TermSymbolApi {
    private[this] var _referenced: Symbol = NoSymbol
    privateWithin = NoSymbol

    type TypeOfClonedSymbol = TermSymbol

    private[this] var _rawname: TermName = initName
    def rawname = _rawname
    def name = {
      if (Statistics.hotEnabled) Statistics.incCounter(nameCount)
      _rawname
    }
    override def name_=(name: Name) {
      if (name != rawname) {
        super.name_=(name)   // logging
        changeNameInOwners(name)
        _rawname = name.toTermName
      }
    }
    final def asNameType(n: Name) = n.toTermName

    /** Term symbols with the exception of static parts of Java classes and packages.
     */
    override def isValue     = !(isModule && hasFlag(PACKAGE | JAVA))
    override def isVariable  = isMutable && !isMethod
    override def isTermMacro = hasFlag(MACRO)

    // interesting only for lambda lift. Captured variables are accessed from inner lambdas.
    override def isCapturedVariable = hasAllFlags(MUTABLE | CAPTURED) && !hasFlag(METHOD)

    override def companionSymbol: Symbol = companionClass
    override def moduleClass = if (isModule) referenced else NoSymbol

    override def isBridge           = this hasFlag BRIDGE
    override def isEarlyInitialized = this hasFlag PRESUPER
    override def isMethod           = this hasFlag METHOD
    override def isModule           = this hasFlag MODULE
    override def isOverloaded       = this hasFlag OVERLOADED
    /*** !!! TODO: shouldn't we do something like the following:
    override def isOverloaded       = (
      if (this.isInitialized)
        this hasFlag OVERLOADED
      else
        (infos ne null) && infos.info.isInstanceOf[OverloadedType]
    )
    ***/
    override def isValueParameter   = this hasFlag PARAM

    override def isSetterParameter  = isValueParameter && owner.isSetter
    override def isAccessor         = this hasFlag ACCESSOR
    override def isGetter           = isAccessor && !isSetter
    override def isDefaultGetter    = name containsName nme.DEFAULT_GETTER_STRING
    override def isSetter           = isAccessor && nme.isSetterName(name)  // todo: make independent of name, as this can be forged.
    override def isLocalDummy       = nme.isLocalDummyName(name)
    override def isClassConstructor = name == nme.CONSTRUCTOR
    override def isMixinConstructor = name == nme.MIXIN_CONSTRUCTOR
    override def isConstructor      = nme.isConstructorName(name)

    override def isPackageObject = isModule && (name == nme.PACKAGE)

    // The name in comments is what it is being disambiguated from.
    // TODO - rescue CAPTURED from BYNAMEPARAM so we can see all the names.
    override def resolveOverloadedFlag(flag: Long) = flag match {
      case DEFAULTPARAM => "<defaultparam>" // TRAIT
      case MIXEDIN      => "<mixedin>"      // EXISTENTIAL
      case LABEL        => "<label>"        // CONTRAVARIANT / INCONSTRUCTOR
      case PRESUPER     => "<presuper>"     // IMPLCLASS
      case BYNAMEPARAM  => if (this.isValueParameter) "<bynameparam>" else "<captured>" // COVARIANT
      case _            => super.resolveOverloadedFlag(flag)
    }

    def referenced: Symbol = _referenced
    def referenced_=(x: Symbol) { _referenced = x }

    def existentialBound = singletonBounds(this.tpe)

    def cloneSymbolImpl(owner: Symbol, newFlags: Long): TermSymbol =
      owner.newTermSymbol(name, pos, newFlags).copyAttrsFrom(this)

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
      // SI-6888 Approximate the name to workaround the deficiencies in `nme.originalName`
      //         in the face of classes named '$'. SI-2806 remains open to address the deeper problem.
      if (unexpandedName endsWith (nme.OUTER)) initialize.referenced
      else NoSymbol

    def setModuleClass(clazz: Symbol): TermSymbol = {
      assert(isModule, this)
      referenced = clazz
      this
    }

    def setLazyAccessor(sym: Symbol): TermSymbol = {
      assert(isLazy && (referenced == NoSymbol || referenced == sym), (this, debugFlagString, referenced, sym))
      referenced = sym
      this
    }

    override def lazyAccessor: Symbol = {
      assert(isLazy, this)
      referenced
    }

    /** change name by appending $$<fully-qualified-name-of-class `base`>
     *  Do the same for any accessed symbols or setters/getters
     */
    override def expandName(base: Symbol) {
      if (!hasFlag(EXPANDEDNAME)) {
        setFlag(EXPANDEDNAME)
        if (hasAccessorFlag && !isDeferred) {
          accessed.expandName(base)
        }
        else if (hasGetter) {
          getterIn(owner).expandName(base)
          setterIn(owner).expandName(base)
        }
        name = nme.expandedName(name.toTermName, base)
      }
    }
  }
  implicit val TermSymbolTag = ClassTag[TermSymbol](classOf[TermSymbol])

  /** A class for module symbols */
  class ModuleSymbol protected[Symbols] (initOwner: Symbol, initPos: Position, initName: TermName)
  extends TermSymbol(initOwner, initPos, initName) with ModuleSymbolApi {
    private var flatname: TermName = null

    override def associatedFile = moduleClass.associatedFile
    override def associatedFile_=(f: AbstractFile) { moduleClass.associatedFile = f }

    override def moduleClass = referenced
    override def companionClass =
      flatOwnerInfo.decl(name.toTypeName).suchThat(sym => sym.isClass && (sym isCoDefinedWith this))

    override def owner = {
      if (Statistics.hotEnabled) Statistics.incCounter(ownerCount)
      // a module symbol may have the lateMETHOD flag after refchecks, see isModuleNotMethod
      if (!isMethod && needsFlatClasses) rawowner.owner
      else rawowner
    }
    override def name: TermName = {
      if (Statistics.hotEnabled) Statistics.incCounter(nameCount)
      if (!isMethod && needsFlatClasses) {
        if (flatname eq null)
          flatname = nme.flattenedName(rawowner.name, rawname)

        flatname
      }
      else rawname
    }
  }
  implicit val ModuleSymbolTag = ClassTag[ModuleSymbol](classOf[ModuleSymbol])

  /** A class for method symbols */
  class MethodSymbol protected[Symbols] (initOwner: Symbol, initPos: Position, initName: TermName)
  extends TermSymbol(initOwner, initPos, initName) with MethodSymbolApi {
    private[this] var mtpePeriod       = NoPeriod
    private[this] var mtpePre: Type    = _
    private[this] var mtpeResult: Type = _
    private[this] var mtpeInfo: Type   = _

    override def isLabel         = this hasFlag LABEL
    override def isVarargsMethod = this hasFlag VARARGS
    override def isLiftedMethod  = this hasFlag LIFTED

    // TODO - this seems a strange definition for "isSourceMethod", given that
    // it does not make any specific effort to exclude synthetics.  Figure out what
    // this method is really for and what logic makes sense.
    override def isSourceMethod  = !(this hasFlag STABLE)  // exclude all accessors
    // unfortunately having the CASEACCESSOR flag does not actually mean you
    // are a case accessor (you can also be a field.)
    override def isCaseAccessorMethod = isCaseAccessor

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

    override def isVarargs: Boolean = definitions.isVarArgsList(paramss.flatten)

    override def returnType: Type = {
      def loop(tpe: Type): Type =
        tpe match {
          case NullaryMethodType(ret) => loop(ret)
          case MethodType(_, ret) => loop(ret)
          case PolyType(_, tpe) => loop(tpe)
          case tpe => tpe
        }
      loop(info)
    }

    override def exceptions = annotations flatMap ThrownException.unapply
  }
  implicit val MethodSymbolTag = ClassTag[MethodSymbol](classOf[MethodSymbol])

  class AliasTypeSymbol protected[Symbols] (initOwner: Symbol, initPos: Position, initName: TypeName)
  extends TypeSymbol(initOwner, initPos, initName) {
    type TypeOfClonedSymbol = TypeSymbol
    override def variance = if (isLocalToThis) Bivariant else info.typeSymbol.variance
    override def isContravariant = variance.isContravariant
    override def isCovariant     = variance.isCovariant
    final override def isAliasType = true
    override def cloneSymbolImpl(owner: Symbol, newFlags: Long): TypeSymbol =
      owner.newNonClassSymbol(name, pos, newFlags)
  }

  /** Let's say you have a type definition
   *
   *  {{{
   *    type T <: Number
   *  }}}
   *
   *  and tsym is the symbol corresponding to T. Then
   *
   *  {{{
   *    tsym is an instance of AbstractTypeSymbol
   *    tsym.info == TypeBounds(Nothing, Number)
   *    tsym.tpe  == TypeRef(NoPrefix, T, List())
   *  }}}
   */
  class AbstractTypeSymbol protected[Symbols] (initOwner: Symbol, initPos: Position, initName: TypeName)
  extends TypeSymbol(initOwner, initPos, initName) {
    type TypeOfClonedSymbol = TypeSymbol
    final override def isAbstractType = true
    override def existentialBound = this.info
    override def cloneSymbolImpl(owner: Symbol, newFlags: Long): TypeSymbol =
      owner.newNonClassSymbol(name, pos, newFlags)
  }

  /** A class of type symbols. Alias and abstract types are direct instances
   *  of this class. Classes are instances of a subclass.
   */
  abstract class TypeSymbol protected[Symbols] (initOwner: Symbol, initPos: Position, initName: TypeName)
  extends Symbol(initOwner, initPos, initName) with TypeSymbolApi {
    privateWithin = NoSymbol
    private[this] var _rawname: TypeName = initName

    type TypeOfClonedSymbol >: Null <: TypeSymbol
    // cloneSymbolImpl still abstract in TypeSymbol.

    def rawname = _rawname
    def name = {
      if (Statistics.hotEnabled) Statistics.incCounter(nameCount)
      _rawname
    }
    final def asNameType(n: Name) = n.toTypeName

    override def isNonClassType = true

    override def resolveOverloadedFlag(flag: Long) = flag match {
      case TRAIT         => "<trait>"         // DEFAULTPARAM
      case EXISTENTIAL   => "<existential>"   // MIXEDIN
      case COVARIANT     => "<covariant>"     // BYNAMEPARAM / CAPTURED
      case CONTRAVARIANT => "<contravariant>" // LABEL / INCONSTRUCTOR (overridden again in ClassSymbol)
      case _             => super.resolveOverloadedFlag(flag)
    }

    private var tyconCache: Type = null
    private var tyconRunId = NoRunId
    private var tpeCache: Type = _
    private var tpePeriod = NoPeriod

    override def isAbstractType          = this hasFlag DEFERRED
    override def isContravariant         = this hasFlag CONTRAVARIANT
    override def isCovariant             = this hasFlag COVARIANT
    override def isExistentiallyBound    = this hasFlag EXISTENTIAL
    override def isTypeParameter         = isTypeParameterOrSkolem && !isSkolem
    override def isTypeParameterOrSkolem = this hasFlag PARAM

    /** Overridden in subclasses for which it makes sense.
     */
    def existentialBound: Type = abort("unexpected type: "+this.getClass+ " "+debugLocationString)

    // TODO - don't allow names to be renamed in this unstructured a fashion.
    // Rename as little as possible.  Enforce invariants on all renames.
    override def name_=(name: Name) {
      if (name != rawname) {
        super.name_=(name)  // logging
        changeNameInOwners(name)
        _rawname = name.toTypeName
      }
    }

    private def newPrefix = if (this hasFlag EXISTENTIAL | PARAM) NoPrefix else owner.thisType
    private def newTypeRef(targs: List[Type]) = typeRef(newPrefix, this, targs)

    /** A polymorphic type symbol has two distinct "types":
     *
     *  tpe_*  a TypeRef with: dummy type args, no unapplied type parameters, and kind *
     *  tpeHK  a TypeRef with: no type args, unapplied type parameters, and
     *           kind (*,*,...,*) => * depending on the number of tparams.
     *
     * The dummy type args in tpe_* are created by wrapping a TypeRef
     * around the type parameter symbols.  Types containing dummies will
     * hide errors or introduce spurious ones if they are passed around
     * as if normal types.  They should only be used in local operations
     * where they will either be discarded immediately after, or will
     * undergo substitution in which the dummies are replaced by actual
     * type arguments.
     */
    override def tpe_* : Type = {
      maybeUpdateTypeCache()
      tpeCache
    }
    override def typeConstructor: Type = {
      if (tyconCacheNeedsUpdate)
        setTyconCache(newTypeRef(Nil))
      tyconCache
    }
    override def tpeHK: Type = typeConstructor

    private def tyconCacheNeedsUpdate = (tyconCache eq null) || tyconRunId != currentRunId
    private def setTyconCache(tycon: Type) {
      tyconCache = tycon
      tyconRunId = currentRunId
      assert(tyconCache ne null, this)
    }

    private def maybeUpdateTypeCache() {
      if (tpePeriod != currentPeriod) {
        if (isValid(tpePeriod))
          tpePeriod = currentPeriod
        else
          updateTypeCache()   // perform the actual update
      }
    }
    private def updateTypeCache() {
      if (tpeCache eq NoType)
        throw CyclicReference(this, typeConstructor)

      if (isInitialized)
        tpePeriod = currentPeriod

      tpeCache = NoType // cycle marker
      val noTypeParams = phase.erasedTypes && this != ArrayClass || unsafeTypeParams.isEmpty
      tpeCache = newTypeRef(
        if (noTypeParams) Nil
        else unsafeTypeParams map (_.typeConstructor)
      )
      // Avoid carrying around different types in tyconCache and tpeCache
      // for monomorphic types.
      if (noTypeParams && tyconCacheNeedsUpdate)
        setTyconCache(tpeCache)
    }

    override def info_=(tp: Type) {
      tpePeriod = NoPeriod
      tyconCache = null
      super.info_=(tp)
    }

    final override def isNonBottomSubClass(that: Symbol): Boolean = (
      (this eq that) || this.isError || that.isError ||
      info.baseTypeIndex(that) >= 0
    )

    override def reset(completer: Type): this.type = {
      super.reset(completer)
      tpePeriod = NoPeriod
      tyconRunId = NoRunId
      this
    }

    /*** example:
     * public class Test3<T> {}
     * public class Test1<T extends Test3> {}
     * info for T in Test1 should be >: Nothing <: Test3[_]
     */

    if (Statistics.hotEnabled) Statistics.incCounter(typeSymbolCount)
  }
  implicit val TypeSymbolTag = ClassTag[TypeSymbol](classOf[TypeSymbol])

  /** A class for type parameters viewed from inside their scopes
   *
   *  @param origin  Can be either a tree, or a symbol, or null.
   *  If skolem got created from newTypeSkolem (called in Namers), origin denotes
   *  the type parameter from which the skolem was created. If it got created from
   *  skolemizeExistential, origin is either null or a Tree. If it is a Tree, it indicates
   *  where the skolem was introduced (this is important for knowing when to pack it
   *  again into ab Existential). origin is `null` only in skolemizeExistentials called
   *  from <:< or isAsSpecific, because here its value does not matter.
   *  I believe the following invariant holds:
   *
   *     origin.isInstanceOf[Symbol] == !hasFlag(EXISTENTIAL)
   */
  class TypeSkolem protected[Symbols] (initOwner: Symbol, initPos: Position, initName: TypeName, origin: AnyRef)
  extends TypeSymbol(initOwner, initPos, initName) {
    type TypeOfClonedSymbol = TypeSkolem
    /** The skolemization level in place when the skolem was constructed */
    val level = skolemizationLevel

    final override def isSkolem = true

    // a type symbol bound by an existential type, for instance the T in
    // List[T] forSome { type T }
    override def isExistentialSkolem = this hasFlag EXISTENTIAL
    override def isGADTSkolem        = this hasAllFlags GADT_SKOLEM_FLAGS.toLong
    override def isTypeSkolem        = this hasFlag PARAM
    override def isAbstractType      = this hasFlag DEFERRED

    override def existentialBound = if (isAbstractType) this.info else super.existentialBound

    /** If typeskolem comes from a type parameter, that parameter, otherwise skolem itself */
    override def deSkolemize = origin match {
      case s: Symbol => s
      case _ => this
    }

    /** If type skolem comes from an existential, the tree where it was created */
    override def unpackLocation = origin

    //@M! (not deSkolemize.typeParams!!), also can't leave superclass definition: use info, not rawInfo
    override def typeParams = info.typeParams

    override def cloneSymbolImpl(owner: Symbol, newFlags: Long): TypeSkolem =
      owner.newTypeSkolemSymbol(name, origin, pos, newFlags)

    override def nameString: String =
      if (settings.debug.value) (super.nameString + "&" + level)
      else super.nameString
  }

  /** A class for class symbols */
  class ClassSymbol protected[Symbols] (initOwner: Symbol, initPos: Position, initName: TypeName)
  extends TypeSymbol(initOwner, initPos, initName) with ClassSymbolApi {
    type TypeOfClonedSymbol = ClassSymbol

    private[this] var flatname: TypeName            = _
    private[this] var _associatedFile: AbstractFile = _
    private[this] var thissym: Symbol               = this

    private[this] var thisTypeCache: Type      = _
    private[this] var thisTypePeriod           = NoPeriod

    override def resolveOverloadedFlag(flag: Long) = flag match {
      case INCONSTRUCTOR => "<inconstructor>" // INCONSTRUCTOR / CONTRAVARIANT / LABEL
      case EXISTENTIAL   => "<existential>"   // EXISTENTIAL / MIXEDIN
      case IMPLCLASS     => "<implclass>"     // IMPLCLASS / PRESUPER
      case _             => super.resolveOverloadedFlag(flag)
    }

    final override def isNonClassType = false
    final override def isAbstractType = false
    final override def isAliasType = false
    final override def isContravariant = false

    override def isAbstractClass           = this hasFlag ABSTRACT
    override def isCaseClass               = this hasFlag CASE
    override def isClassLocalToConstructor = this hasFlag INCONSTRUCTOR
    override def isImplClass               = this hasFlag IMPLCLASS
    override def isModuleClass             = this hasFlag MODULE
    override def isPackageClass            = this hasFlag PACKAGE
    override def isTrait                   = this hasFlag TRAIT

    override def isAnonOrRefinementClass = isAnonymousClass || isRefinementClass
    override def isAnonymousClass        = name containsName tpnme.ANON_CLASS_NAME
    override def isConcreteClass         = !(this hasFlag ABSTRACT | TRAIT)
    override def isJavaInterface         = hasAllFlags(JAVA | TRAIT)
    override def isNestedClass           = !isTopLevel
    override def isNumericValueClass     = definitions.isNumericValueClass(this)
    override def isNumeric               = isNumericValueClass
    override def isPackageObjectClass    = isModuleClass && (name == tpnme.PACKAGE)
    override def isPrimitiveValueClass   = definitions.isPrimitiveValueClass(this)
    override def isPrimitive             = isPrimitiveValueClass

    // The corresponding interface is the last parent by convention.
    private def lastParent = if (tpe.parents.isEmpty) NoSymbol else tpe.parents.last.typeSymbol
    override def toInterface: Symbol = (
      if (isImplClass) {
        if (phase.next.erasedTypes) lastParent
        else owner.info.decl(tpnme.interfaceName(name))
      }
      else super.toInterface
    )

    /** Is this class locally defined?
     *  A class is local, if
     *   - it is anonymous, or
     *   - its owner is a value
     *   - it is defined within a local class
     */
    override def isLocalClass = (
         isAnonOrRefinementClass
      || isLocalToBlock
      || !isTopLevel && owner.isLocalClass
    )

    override def enclClassChain = this :: owner.enclClassChain

    /** A helper method that factors the common code used the discover a
     *  companion module of a class. If a companion module exists, its symbol is
     *  returned, otherwise, `NoSymbol` is returned.
     */
    protected final def companionModule0: Symbol =
      flatOwnerInfo.decl(name.toTermName).suchThat(sym => sym.isModuleNotMethod && (sym isCoDefinedWith this))

    override def companionModule    = companionModule0
    override def companionSymbol    = companionModule0
    override def linkedClassOfClass = companionModule.moduleClass

    override def sourceModule       = if (isModuleClass) companionModule else NoSymbol

    override def existentialBound = GenPolyType(this.typeParams, TypeBounds.upper(this.classBound))

    def primaryConstructorName = if (this hasFlag TRAIT | IMPLCLASS) nme.MIXIN_CONSTRUCTOR else nme.CONSTRUCTOR

    override def primaryConstructor = {
      val c = info decl primaryConstructorName
      if (c.isOverloaded) c.alternatives.head else c
    }

    override def associatedFile = (
      if (!isTopLevel) super.associatedFile
      else {
        if (_associatedFile eq null) NoAbstractFile // guarantee not null, but save cost of initializing the var
        else _associatedFile
      }
    )
    override def associatedFile_=(f: AbstractFile) { _associatedFile = f }

    override def reset(completer: Type): this.type = {
      super.reset(completer)
      thissym = this
      this
    }

    /** the type this.type in this class */
    override def thisType: Type = {
      val period = thisTypePeriod
      if (period != currentPeriod) {
        if (!isValid(period)) thisTypeCache = ThisType(this)
        thisTypePeriod = currentPeriod
      }
      thisTypeCache
    }

    override def owner: Symbol = {
      if (Statistics.hotEnabled) Statistics.incCounter(ownerCount)
      if (needsFlatClasses) rawowner.owner else rawowner
    }

    override def name: TypeName = {
      if (Statistics.canEnable) Statistics.incCounter(nameCount)
      if (needsFlatClasses) {
        if (flatname eq null)
          flatname = tpnme.flattenedName(rawowner.name, rawname)

        flatname
      }
      else rawname
    }

    /** A symbol carrying the self type of the class as its type */
    override def thisSym: Symbol = thissym

    /** Sets the self type of the class */
    override def typeOfThis_=(tp: Type) {
      thissym = newThisSym(nme.this_, pos).setInfo(tp)
    }

    override def cloneSymbolImpl(owner: Symbol, newFlags: Long): ClassSymbol = {
      val clone = owner.newClassSymbol(name, pos, newFlags)
      if (thisSym != this) {
        clone.typeOfThis = typeOfThis
        clone.thisSym setName thisSym.name
      }
      clone.associatedFile = _associatedFile
      clone
    }

    override def derivedValueClassUnbox =
      // (info.decl(nme.unbox)) orElse      uncomment once we accept unbox methods
      (info.decls.find(_ hasAllFlags PARAMACCESSOR | METHOD) getOrElse
       NoSymbol)

    private[this] var childSet: Set[Symbol] = Set()
    override def children = childSet
    override def addChild(sym: Symbol) { childSet = childSet + sym }

    def anonOrRefinementString = {
      if (hasCompleteInfo) {
        val label   = if (isAnonymousClass) "$anon:" else "refinement of"
        val parents = parentsString(info.parents map functionNBaseType filterNot (_.typeSymbol == SerializableClass))
        s"<$label $parents>"
      }
      else if (isAnonymousClass) "$anon"
      else nameString
    }
    override def toString = (
      if (isAnonOrRefinementClass) anonOrRefinementString
      else super.toString
    )

    if (Statistics.hotEnabled) Statistics.incCounter(classSymbolCount)
  }
  implicit val ClassSymbolTag = ClassTag[ClassSymbol](classOf[ClassSymbol])

  /** A class for module class symbols
   *  Note: Not all module classes are of this type; when unpickled, we get
   *  plain class symbols!
   */
  class ModuleClassSymbol protected[Symbols] (owner: Symbol, pos: Position, name: TypeName)
  extends ClassSymbol(owner, pos, name) {
    private[this] var module: Symbol        = _
    private[this] var typeOfThisCache: Type = _
    private[this] var typeOfThisPeriod      = NoPeriod

    private var implicitMembersCacheValue: Scope = EmptyScope
    private var implicitMembersCacheKey1: Type = NoType
    private var implicitMembersCacheKey2: ScopeEntry = null

    override def isModuleClass = true
    override def linkedClassOfClass = companionClass

    /** the self type of an object foo is foo.type, not class<foo>.this.type
     */
    override def typeOfThis = {
      val period = typeOfThisPeriod
      if (period != currentPeriod) {
        if (!isValid(period))
          typeOfThisCache = singleType(owner.thisType, sourceModule)
        typeOfThisPeriod = currentPeriod
      }
      typeOfThisCache
    }

    def implicitMembers: Scope = {
      val tp = info
      if ((implicitMembersCacheKey1 ne tp) || (implicitMembersCacheKey2 ne tp.decls.elems)) {
        // Skip a package object class, because the members are also in
        // the package and we wish to avoid spurious ambiguities as in pos/t3999.
        if (!isPackageObjectClass) {
          implicitMembersCacheValue = tp.implicitMembers
          implicitMembersCacheKey1 = tp
          implicitMembersCacheKey2 = tp.decls.elems
        }
      }
      implicitMembersCacheValue
    }
    // The null check seems to be necessary for the reifier.
    override def sourceModule = if (module ne null) module else companionModule
    override def sourceModule_=(module: Symbol) { this.module = module }
  }

  class PackageObjectClassSymbol protected[Symbols] (owner0: Symbol, pos0: Position)
  extends ModuleClassSymbol(owner0, pos0, tpnme.PACKAGE) {
    final override def isPackageObjectClass   = true
    final override def isPackageObjectOrClass = true
    final override def skipPackageObject      = owner
    final override def setName(name: Name): this.type = {
      abort("Can't rename a package object to " + name)
    }
  }

  trait ImplClassSymbol extends ClassSymbol {
    override def sourceModule = companionModule
    // override def isImplClass = true
    override def typeOfThis  = thisSym.tpe // don't use the ModuleClassSymbol typeOfThisCache.
  }

  class PackageClassSymbol protected[Symbols] (owner0: Symbol, pos0: Position, name0: TypeName)
  extends ModuleClassSymbol(owner0, pos0, name0) {
    override def sourceModule = companionModule
    override def enclClassChain = Nil
    override def isPackageClass = true
  }

  class RefinementClassSymbol protected[Symbols] (owner0: Symbol, pos0: Position)
  extends ClassSymbol(owner0, pos0, tpnme.REFINE_CLASS_NAME) {
    override def name_=(name: Name) {
      abort("Cannot set name of RefinementClassSymbol to " + name)
      super.name_=(name)
    }
    override def isRefinementClass       = true
    override def isAnonOrRefinementClass = true
    override def isLocalClass            = true
    override def hasMeaninglessName      = true
    override def companionModule: Symbol = NoSymbol

    /** The mentioned twist.  A refinement class has transowner X
     *  if any of its parents has transowner X.
     */
    override def hasTransOwner(sym: Symbol) = (
         super.hasTransOwner(sym)
      || info.parents.exists(_.typeSymbol hasTransOwner sym)
    )
  }
  trait StubSymbol extends Symbol {
    devWarning("creating stub symbol to defer error: " + missingMessage)

    def missingMessage: String

    /** Fail the stub by throwing a [[scala.reflect.internal.MissingRequirementError]]. */
    override final def failIfStub() =
      MissingRequirementError.signal(missingMessage)

    /** Fail the stub by reporting an error to the reporter, setting the IS_ERROR flag
      * on this symbol, and returning the dummy value `alt`.
      */
    private def fail[T](alt: T): T = {
      // Avoid issuing lots of redundant errors
      if (!hasFlag(IS_ERROR)) {
        globalError(missingMessage)
        if (settings.debug.value)
          (new Throwable).printStackTrace

        this setFlag IS_ERROR
      }
      alt
    }
    // This one doesn't call fail because SpecializeTypes winds up causing
    // isMonomorphicType to be called, which calls this, which would fail us
    // in all the scenarios we're trying to keep from failing.
    override def originalInfo    = NoType
    override def associatedFile  = owner.associatedFile
    override def info            = fail(NoType)
    override def rawInfo         = fail(NoType)
    override def companionSymbol = fail(NoSymbol)
  }
  class StubClassSymbol(owner0: Symbol, name0: TypeName, val missingMessage: String) extends ClassSymbol(owner0, owner0.pos, name0) with StubSymbol
  class StubPackageClassSymbol(owner0: Symbol, name0: TypeName, val missingMessage: String) extends PackageClassSymbol(owner0, owner0.pos, name0) with StubSymbol
  class StubTermSymbol(owner0: Symbol, name0: TermName, val missingMessage: String) extends TermSymbol(owner0, owner0.pos, name0) with StubSymbol

  trait FreeSymbol extends Symbol {
    def origin: String
  }
  class FreeTermSymbol(name0: TermName, value0: => Any, val origin: String) extends TermSymbol(NoSymbol, NoPosition, name0) with FreeSymbol with FreeTermSymbolApi {
    final override def isFreeTerm = true
    final override def asFreeTerm = this
    def value = value0
  }
  implicit val FreeTermSymbolTag = ClassTag[FreeTermSymbol](classOf[FreeTermSymbol])

  class FreeTypeSymbol(name0: TypeName, val origin: String) extends TypeSkolem(NoSymbol, NoPosition, name0, NoSymbol) with FreeSymbol with FreeTypeSymbolApi {
    final override def isFreeType = true
    final override def asFreeType = this
  }
  implicit val FreeTypeSymbolTag = ClassTag[FreeTypeSymbol](classOf[FreeTypeSymbol])

  /** An object representing a missing symbol */
  class NoSymbol protected[Symbols]() extends Symbol(null, NoPosition, nme.NO_NAME) {
    final type NameType = TermName
    type TypeOfClonedSymbol = NoSymbol

    def asNameType(n: Name) = n.toTermName
    def rawname = nme.NO_NAME
    def name = nme.NO_NAME
    override def name_=(n: Name) = abort("Cannot set NoSymbol's name to " + n)

    // Syncnote: no need to synchronize this, because NoSymbol's initialization is triggered by JavaUniverse.init
    // which is called in universe's constructor - something that's inherently single-threaded
    setInfo(NoType)
    privateWithin = this

    override def info_=(info: Type) = {
      infos = TypeHistory(1, NoType, null)
      unlock()
      validTo = currentPeriod
    }
    override def flagMask = AllFlags
    override def exists = false
    override def isHigherOrderTypeParameter = false
    override def companionClass = NoSymbol
    override def companionModule = NoSymbol
    override def companionSymbol = NoSymbol
    override def isSubClass(that: Symbol) = false
    override def filter(cond: Symbol => Boolean) = this
    override def defString: String = toString
    override def locationString: String = ""
    override def enclClassChain = Nil
    override def enclClass: Symbol = this
    override def enclosingTopLevelClass: Symbol = this
    override def enclosingTopLevelClassOrDummy: Symbol = this
    override def enclosingPackageClass: Symbol = this
    override def enclMethod: Symbol = this
    override def associatedFile = NoAbstractFile
    override def owner: Symbol = {
      devWarningDumpStack("NoSymbol.owner", 15)
      this
    }
    override def ownerChain: List[Symbol] = Nil
    override def ownersIterator: Iterator[Symbol] = Iterator.empty
    override def alternatives: List[Symbol] = List()
    override def reset(completer: Type): this.type = this
    override def info: Type = NoType
    override def existentialBound: Type = NoType
    override def rawInfo: Type = NoType
    override def accessBoundary(base: Symbol): Symbol = enclosingRootClass
    def cloneSymbolImpl(owner: Symbol, newFlags: Long) = abort("NoSymbol.clone()")
  }

  protected def makeNoSymbol: NoSymbol = new NoSymbol

  lazy val NoSymbol: NoSymbol = makeNoSymbol

  /** Derives a new list of symbols from the given list by mapping the given
   *  list across the given function.  Then fixes the info of all the new symbols
   *  by substituting the new symbols for the original symbols.
   *
   *  @param    syms    the prototypical symbols
   *  @param    symFn   the function to create new symbols
   *  @return           the new list of info-adjusted symbols
   */
  def deriveSymbols(syms: List[Symbol], symFn: Symbol => Symbol): List[Symbol] = {
    val syms1 = mapList(syms)(symFn)
    mapList(syms1)(_ substInfo (syms, syms1))
  }

  /** Derives a new list of symbols from the given list by mapping the given
   *  list of `syms` and `as` across the given function.
   *  Then fixes the info of all the new symbols
   *  by substituting the new symbols for the original symbols.
   *
   *  @param    syms    the prototypical symbols
   *  @param    as      arguments to be passed to symFn together with symbols from syms (must be same length)
   *  @param    symFn   the function to create new symbols
   *  @return           the new list of info-adjusted symbols
   */
  def deriveSymbols2[A](syms: List[Symbol], as: List[A], symFn: (Symbol, A) => Symbol): List[Symbol] = {
    val syms1 = map2(syms, as)(symFn)
    mapList(syms1)(_ substInfo (syms, syms1))
  }

  /** Derives a new Type by first deriving new symbols as in deriveSymbols,
   *  then performing the same oldSyms => newSyms substitution on `tpe` as is
   *  performed on the symbol infos in deriveSymbols.
   *
   *  @param    syms    the prototypical symbols
   *  @param    symFn   the function to create new symbols
   *  @param    tpe     the prototypical type
   *  @return           the new symbol-substituted type
   */
  def deriveType(syms: List[Symbol], symFn: Symbol => Symbol)(tpe: Type): Type = {
    val syms1 = deriveSymbols(syms, symFn)
    tpe.substSym(syms, syms1)
  }

  /** Derives a new Type by first deriving new symbols as in deriveSymbols2,
   *  then performing the same oldSyms => newSyms substitution on `tpe` as is
   *  performed on the symbol infos in deriveSymbols.
   *
   *  @param    syms    the prototypical symbols
   *  @param    as      arguments to be passed to symFn together with symbols from syms (must be same length)
   *  @param    symFn   the function to create new symbols based on `as`
   *  @param    tpe     the prototypical type
   *  @return           the new symbol-substituted type
   */
  def deriveType2[A](syms: List[Symbol], as: List[A], symFn: (Symbol, A) => Symbol)(tpe: Type): Type = {
    val syms1 = deriveSymbols2(syms, as, symFn)
    tpe.substSym(syms, syms1)
  }

  /** Derives a new Type by instantiating the given list of symbols as
   *  WildcardTypes.
   *
   *  @param    syms    the symbols to replace
   *  @return           the new type with WildcardType replacing those syms
   */
  def deriveTypeWithWildcards(syms: List[Symbol])(tpe: Type): Type = {
    if (syms.isEmpty) tpe
    else tpe.instantiateTypeParams(syms, syms map (_ => WildcardType))
  }
  /** Convenience functions which derive symbols by cloning.
   */
  def cloneSymbols(syms: List[Symbol]): List[Symbol] =
    deriveSymbols(syms, _.cloneSymbol)
  def cloneSymbolsAtOwner(syms: List[Symbol], owner: Symbol): List[Symbol] =
    deriveSymbols(syms, _ cloneSymbol owner)

  /** Clone symbols and apply the given function to each new symbol's info.
   *
   *  @param    syms    the prototypical symbols
   *  @param    infoFn  the function to apply to the infos
   *  @return           the newly created, info-adjusted symbols
   */
  def cloneSymbolsAndModify(syms: List[Symbol], infoFn: Type => Type): List[Symbol] =
    mapList(cloneSymbols(syms))(_ modifyInfo infoFn)
  def cloneSymbolsAtOwnerAndModify(syms: List[Symbol], owner: Symbol, infoFn: Type => Type): List[Symbol] =
    mapList(cloneSymbolsAtOwner(syms, owner))(_ modifyInfo infoFn)

  /** Functions which perform the standard clone/substituting on the given symbols and type,
   *  then call the creator function with the new symbols and type as arguments.
   */
  def createFromClonedSymbols[T](syms: List[Symbol], tpe: Type)(creator: (List[Symbol], Type) => T): T = {
    val syms1 = cloneSymbols(syms)
    creator(syms1, tpe.substSym(syms, syms1))
  }
  def createFromClonedSymbolsAtOwner[T](syms: List[Symbol], owner: Symbol, tpe: Type)(creator: (List[Symbol], Type) => T): T = {
    val syms1 = cloneSymbolsAtOwner(syms, owner)
    creator(syms1, tpe.substSym(syms, syms1))
  }

  /** A deep map on a symbol's paramss.
   */
  def mapParamss[T](sym: Symbol)(f: Symbol => T): List[List[T]] = mmap(sym.info.paramss)(f)

  def existingSymbols(syms: List[Symbol]): List[Symbol] =
    syms filter (s => (s ne null) && (s ne NoSymbol))

  /** Return closest enclosing method, unless shadowed by an enclosing class. */
  // TODO Move back to ExplicitOuter when the other call site is removed.
  // no use of closures here in the interest of speed.
  final def closestEnclMethod(from: Symbol): Symbol =
    if (from.isSourceMethod) from
    else if (from.isClass) NoSymbol
    else closestEnclMethod(from.owner)

  /** An exception for cyclic references of symbol definitions */
  case class CyclicReference(sym: Symbol, info: Type)
  extends TypeError("illegal cyclic reference involving " + sym) {
    if (settings.debug.value) printStackTrace()
  }

  /** A class for type histories */
  private case class TypeHistory(var validFrom: Period, info: Type, prev: TypeHistory) {
    assert((prev eq null) || phaseId(validFrom) > phaseId(prev.validFrom), this)
    assert(validFrom != NoPeriod, this)

    private def phaseString = "%s: %s".format(phaseOf(validFrom), info)
    override def toString = toList reverseMap (_.phaseString) mkString ", "

    def toList: List[TypeHistory] = this :: ( if (prev eq null) Nil else prev.toList )

    def oldest: TypeHistory = if (prev == null) this else prev.oldest
  }

// ----- Hoisted closures and convenience methods, for compile time reductions -------

  private[scala] final val symbolIsPossibleInRefinement = (sym: Symbol) => sym.isPossibleInRefinement

  @tailrec private[scala] final
  def allSymbolsHaveOwner(syms: List[Symbol], owner: Symbol): Boolean = syms match {
    case sym :: rest => sym.owner == owner && allSymbolsHaveOwner(rest, owner)
    case _           => true
  }


// -------------- Statistics --------------------------------------------------------

  Statistics.newView("#symbols")(ids)


// -------------- Completion --------------------------------------------------------

  // is used to differentiate levels of thread-safety in `Symbol.isThreadsafe`
  case class SymbolOps(isFlagRelated: Boolean, mask: Long)
  val AllOps = SymbolOps(isFlagRelated = false, mask = 0L)
  def FlagOps(mask: Long) = SymbolOps(isFlagRelated = true, mask = mask)

  private def relevantSymbols(syms: Seq[Symbol]) = syms.flatMap(sym => List(sym, sym.moduleClass, sym.sourceModule))
  def markFlagsCompleted(syms: Symbol*)(mask: Long): Unit = relevantSymbols(syms).foreach(_.markFlagsCompleted(mask))
  def markAllCompleted(syms: Symbol*): Unit = relevantSymbols(syms).foreach(_.markAllCompleted)
}

object SymbolsStats {
  val typeSymbolCount     = Statistics.newCounter("#type symbols")
  val classSymbolCount    = Statistics.newCounter("#class symbols")
  val flagsCount          = Statistics.newCounter("#flags ops")
  val ownerCount          = Statistics.newCounter("#owner ops")
  val nameCount           = Statistics.newCounter("#name ops")
}

package scala.reflect
package runtime

import scala.tools.nsc.io.AbstractFile
import scala.reflect.internal.Flags._

/** Decorates selected methods of symbols that belong to runtime reflection universes
 *  with synchronization and auto-initialization facilities.
 */
trait ReflectedSymbols extends internal.Symbols { thisUniverse: SymbolTable =>

  override protected def nextId() = synchronized { super.nextId() }

  override protected def freshExistentialName(suffix: String) =
    synchronized { super.freshExistentialName(suffix) }

  // Set the fields which point companions at one another.  Returns the module.
  override def connectModuleToClass(m: ModuleSymbol, moduleClass: ClassSymbol): ModuleSymbol =
    synchronized { super.connectModuleToClass(m, moduleClass) }

  protected override def createAbstractTypeSymbol(owner: Symbol, name: TypeName, pos: Position, newFlags: Long): AbstractTypeSymbol =
    new AbstractTypeSymbol(owner, pos, name) with ReflectedTypeSymbol initFlags newFlags

  protected override def createAliasTypeSymbol(owner: Symbol, name: TypeName, pos: Position, newFlags: Long): AliasTypeSymbol =
    new AliasTypeSymbol(owner, pos, name) with ReflectedTypeSymbol initFlags newFlags

  protected override def createTypeSkolemSymbol(owner: Symbol, name: TypeName, origin: AnyRef, pos: Position, newFlags: Long): TypeSkolem =
    new TypeSkolem(owner, pos, name, origin) with ReflectedTypeSymbol initFlags newFlags

  protected override def createClassSymbol(owner: Symbol, name: TypeName, pos: Position, newFlags: Long): ClassSymbol =
    new ClassSymbol(owner, pos, name) with ReflectedClassSymbol initFlags newFlags

  protected override def createModuleClassSymbol(owner: Symbol, name: TypeName, pos: Position, newFlags: Long): ModuleClassSymbol =
    new ModuleClassSymbol(owner, pos, name) with ReflectedModuleClassSymbol initFlags newFlags

  protected override def createPackageClassSymbol(owner: Symbol, name: TypeName, pos: Position, newFlags: Long): PackageClassSymbol =
    new PackageClassSymbol(owner, pos, name) with ReflectedModuleClassSymbol initFlags newFlags

  protected override def createRefinementClassSymbol(owner: Symbol, pos: Position, newFlags: Long): RefinementClassSymbol =
    new RefinementClassSymbol(owner, pos) with ReflectedClassSymbol initFlags newFlags

  protected override def createImplClassSymbol(owner: Symbol, name: TypeName, pos: Position, newFlags: Long): ClassSymbol =
    new ClassSymbol(owner, pos, name) with ImplClassSymbol with ReflectedClassSymbol initFlags newFlags

  protected override def createPackageObjectClassSymbol(owner: Symbol, pos: Position, newFlags: Long): PackageObjectClassSymbol =
    new PackageObjectClassSymbol(owner, pos) with ReflectedClassSymbol initFlags newFlags

  protected override def createTermSymbol(owner: Symbol, name: TermName, pos: Position, newFlags: Long): TermSymbol =
    new TermSymbol(owner, pos, name) with ReflectedTermSymbol initFlags newFlags

  protected override def createMethodSymbol(owner: Symbol, name: TermName, pos: Position, newFlags: Long): MethodSymbol =
    new MethodSymbol(owner, pos, name) with ReflectedMethodSymbol initFlags newFlags

  protected override def createModuleSymbol(owner: Symbol, name: TermName, pos: Position, newFlags: Long): ModuleSymbol =
    new ModuleSymbol(owner, pos, name) with ReflectedTermSymbol initFlags newFlags

  protected override def createValueParameterSymbol(owner: Symbol, name: TermName, pos: Position, newFlags: Long): TermSymbol =
    new TermSymbol(owner, pos, name) with ReflectedTermSymbol initFlags newFlags

  protected override def createValueMemberSymbol(owner: Symbol, name: TermName, pos: Position, newFlags: Long): TermSymbol =
    new TermSymbol(owner, pos, name) with ReflectedTermSymbol initFlags newFlags

  protected override def createPackageSymbol(owner: Symbol, name: TermName, pos: Position, newFlags: Long): ModuleSymbol =
    createModuleSymbol(owner, name, pos, newFlags)

  override def newFreeTermSymbol(name: TermName, info: Type, value: => Any, flags: Long = 0L, origin: String = null): FreeTermSymbol =
    new FreeTermSymbol(name, value, origin) with ReflectedTermSymbol initFlags flags setInfo info

  override def newFreeTypeSymbol(name: TypeName, info: Type, value: => Any, flags: Long = 0L, origin: String = null): FreeTypeSymbol =
    new FreeTypeSymbol(name, value, origin) with ReflectedTypeSymbol initFlags flags setInfo info

  override protected def makeNoSymbol: NoSymbol = new NoSymbol with ReflectedSymbol

  trait ReflectedSymbol extends Symbol {

    protected def smartInitialize = {
      val isInitializing = this hasFlag LOCKED
      if (!isCompilerUniverse && !isInitialized && !isInitializing) initialize
    }

    override def rawflags = synchronized { super.rawflags }
    override def rawflags_=(x: Long) = synchronized { super.rawflags_=(x) }

    override def rawowner = synchronized { super.rawowner }
    override def owner_=(owner: Symbol) = synchronized { super.owner_=(owner) }

    override def validTo = synchronized { super.validTo }
    override def validTo_=(x: Period) = synchronized { super.validTo_=(x) }

    override def pos = synchronized { super.pos }
    override def setPos(pos: Position): this.type = { synchronized { super.setPos(pos) }; this }

    override def privateWithin = synchronized { smartInitialize; super.privateWithin }
    override def privateWithin_=(sym: Symbol) = synchronized { super.privateWithin_=(sym) }

    override def info = synchronized { super.info }
    override def info_=(info: Type) = synchronized { super.info_=(info) }
    override def updateInfo(info: Type): Symbol = synchronized { super.updateInfo(info) }
    override def rawInfo: Type = synchronized { super.rawInfo }

    override def typeParams: List[Symbol] = synchronized { super.typeParams }

    override def reset(completer: Type): this.type = synchronized { super.reset(completer) }

    override def infosString: String = synchronized { super.infosString }

    override def annotations: List[AnnotationInfo] = synchronized { smartInitialize; super.annotations }
    override def setAnnotations(annots: List[AnnotationInfo]): this.type = { synchronized { super.setAnnotations(annots) }; this }

    override def isSynthetic = { smartInitialize; super.isSynthetic }
    override def isImplementationArtifact = { smartInitialize; super.isImplementationArtifact }
    // not determined by flags
    // override def isLocal = { smartInitialize; super.isLocal }
    override def isPrivate = { smartInitialize; super.isPrivate }
    override def isProtected = { smartInitialize; super.isProtected }
    override def isPublic = { smartInitialize; super.isPublic }
    // this flag doesn't need unpickling
    // override def isPackage = { smartInitialize; super.isPackage }
    // this flag doesn't need unpickling
    // override def isPackageClass = { smartInitialize; super.isPackageClass }
    // is transient, erroneous symbols can't be pickled
    // override def isErroneous = { smartInitialize; super.isErroneous }
    override def isStatic = { smartInitialize; super.isStatic }
    override def isFinal = { smartInitialize; super.isFinal }
    override def isOverride = { smartInitialize; super.isOverride }
    override def isAbstractOverride = { smartInitialize; super.isAbstractOverride }
    override def isMacro = { smartInitialize; super.isMacro }
    // this flag doesn't need unpickling
    // override def isParameter = { smartInitialize; super.isParameter }
    override def isSpecialized = { smartInitialize; super.isSpecialized }
    override def isJava = { smartInitialize; super.isJava }
  }

// ------- subclasses ---------------------------------------------------------------------

  trait ReflectedTermSymbol extends TermSymbol with ReflectedSymbol {
    override def name_=(x: Name) = synchronized { super.name_=(x) }
    override def rawname = synchronized { super.rawname }
    override def referenced: Symbol = synchronized { super.referenced }
    override def referenced_=(x: Symbol) = synchronized { super.referenced_=(x) }

    override def isVal = { smartInitialize; super.isVal }
    override def isStable = { smartInitialize; super.isStable }
    override def isVar = { smartInitialize; super.isVar }
    override def isAccessor = { smartInitialize; super.isAccessor }
    override def isGetter = { smartInitialize; super.isGetter }
    override def isSetter = { smartInitialize; super.isSetter }
    override def isOverloaded = { smartInitialize; super.isOverloaded }
    override def isImplicit = { smartInitialize; super.isImplicit }
    override def isLazy = { smartInitialize; super.isLazy }
    // this flag doesn't need unpickling
    // override def isParamAccessor = { smartInitialize; super.isParamAccessor }
    override def isCaseAccessor = { smartInitialize; super.isCaseAccessor }
    override def isParamWithDefault = { smartInitialize; super.isParamWithDefault }
    override def isByNameParam = { smartInitialize; super.isByNameParam }
  }

  trait ReflectedTypeSymbol extends TypeSymbol with ReflectedSymbol {
    override def name_=(x: Name) = synchronized { super.name_=(x) }
    override def rawname = synchronized { super.rawname }
    override def typeConstructor: Type = synchronized { super.typeConstructor }
    override def tpe: Type = synchronized { super.tpe }

    override def isContravariant = { smartInitialize; super.isContravariant }
    override def isCovariant = { smartInitialize; super.isCovariant }
    // is transient, skolems are only created with a factory, not unpickled
    // override def isSkolem = { smartInitialize; super.isSkolem }
    // this flag doesn't need unpickling
    // override def isAliasType = { smartInitialize; super.isAliasType }
    // this flag doesn't need unpickling
    // override def isAbstractType = { smartInitialize; super.isAbstractType }
    override def isExistential = { smartInitialize; super.isExistential }
  }

  trait ReflectedMethodSymbol extends MethodSymbol with ReflectedTermSymbol {
    override def typeAsMemberOf(pre: Type): Type = synchronized { super.typeAsMemberOf(pre) }
    override def params: List[List[Symbol]] = synchronized { super.params }
    override def returnType: Type = synchronized { super.returnType }

    override def isConstructor = { smartInitialize; super.isConstructor }
    // not determined by flags
    // override def isPrimaryConstructor = { smartInitialize; super.isPrimaryConstructor }
    override def isVarargs = { smartInitialize; super.isVarargs }
  }

  trait ReflectedClassSymbol extends ClassSymbol with ReflectedTypeSymbol {
    override def associatedFile = synchronized { super.associatedFile }
    override def associatedFile_=(f: AbstractFile) = synchronized { super.associatedFile_=(f) }
    override def thisSym: Symbol = synchronized { super.thisSym }
    override def thisType: Type = synchronized { super.thisType }
    override def typeOfThis: Type = synchronized { super.typeOfThis }
    override def typeOfThis_=(tp: Type) = synchronized { super.typeOfThis_=(tp) }
    override def children = synchronized { super.children }
    override def addChild(sym: Symbol) = synchronized { super.addChild(sym) }

    // not determined by flags
    // override def isPrimitive = { smartInitialize; super.isPrimitive }
    // not determined by flags
    // override def isNumeric = { smartInitialize; super.isNumeric }
    override def isDerivedValueClass = { smartInitialize; super.isDerivedValueClass }
    override def isTrait = { smartInitialize; super.isTrait }
    override def isAbstractClass = { smartInitialize; super.isAbstractClass }
    override def isCaseClass = { smartInitialize; super.isCaseClass }
    override def isSealed = { smartInitialize; super.isSealed }
  }

  trait ReflectedModuleClassSymbol extends ModuleClassSymbol with ReflectedClassSymbol {
    override def sourceModule = synchronized { super.sourceModule }
    override def implicitMembers: Scope = synchronized { super.implicitMembers }
  }
}


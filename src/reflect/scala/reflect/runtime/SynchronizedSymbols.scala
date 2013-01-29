package scala
package reflect
package runtime

import scala.reflect.io.AbstractFile

private[reflect] trait SynchronizedSymbols extends internal.Symbols { self: SymbolTable =>

  // we can keep this lock fine-grained, because nextId is just a simple increment, which makes deadlocks impossible
  private lazy val nextIdLock = new Object
  override protected def nextId() = nextIdLock.synchronized { super.nextId() }

  // we can keep this lock fine-grained, because freshExistentialName is just a simple increment, which makes deadlocks impossible
  private lazy val freshExistentialNameLock = new Object
  override protected def freshExistentialName(suffix: String) =
    freshExistentialNameLock.synchronized { super.freshExistentialName(suffix) }

  // Set the fields which point companions at one another.  Returns the module.
  override def connectModuleToClass(m: ModuleSymbol, moduleClass: ClassSymbol): ModuleSymbol =
    gilSynchronized { super.connectModuleToClass(m, moduleClass) }

  override def newFreeTermSymbol(name: TermName, value: => Any, flags: Long = 0L, origin: String = null): FreeTermSymbol =
    new FreeTermSymbol(name, value, origin) with SynchronizedTermSymbol initFlags flags

  override def newFreeTypeSymbol(name: TypeName, flags: Long = 0L, origin: String = null): FreeTypeSymbol =
    new FreeTypeSymbol(name, origin) with SynchronizedTypeSymbol initFlags flags

  override protected def makeNoSymbol: NoSymbol = new NoSymbol with SynchronizedSymbol

  trait SynchronizedSymbol extends Symbol {

    override def rawflags = gilSynchronized { super.rawflags }
    override def rawflags_=(x: Long) = gilSynchronized { super.rawflags_=(x) }

    override def rawowner = gilSynchronized { super.rawowner }
    override def owner_=(owner: Symbol) = gilSynchronized { super.owner_=(owner) }

    override def validTo = gilSynchronized { super.validTo }
    override def validTo_=(x: Period) = gilSynchronized { super.validTo_=(x) }

    override def pos = gilSynchronized { super.pos }
    override def setPos(pos: Position): this.type = { gilSynchronized { super.setPos(pos) }; this }

    override def privateWithin = gilSynchronized { super.privateWithin }
    override def privateWithin_=(sym: Symbol) = gilSynchronized { super.privateWithin_=(sym) }

    override def info = gilSynchronized { super.info }
    override def info_=(info: Type) = gilSynchronized { super.info_=(info) }
    override def updateInfo(info: Type): Symbol = gilSynchronized { super.updateInfo(info) }
    override def rawInfo: Type = gilSynchronized { super.rawInfo }

    override def typeParams: List[Symbol] = gilSynchronized {
      if (isCompilerUniverse) super.typeParams
      else {
        if (isMonomorphicType) Nil
        else {
          // analogously to the "info" getter, here we allow for two completions:
          //   one: sourceCompleter to LazyType, two: LazyType to completed type
          if (validTo == NoPeriod)
            rawInfo load this
          if (validTo == NoPeriod)
            rawInfo load this

          rawInfo.typeParams
        }
      }
    }
    override def unsafeTypeParams: List[Symbol] = gilSynchronized {
      if (isCompilerUniverse) super.unsafeTypeParams
      else {
        if (isMonomorphicType) Nil
        else rawInfo.typeParams
      }
    }

    override def reset(completer: Type): this.type = gilSynchronized { super.reset(completer) }

    override def infosString: String = gilSynchronized { super.infosString }

    override def annotations: List[AnnotationInfo] = gilSynchronized { super.annotations }
    override def setAnnotations(annots: List[AnnotationInfo]): this.type = { gilSynchronized { super.setAnnotations(annots) }; this }


// ------ creators -------------------------------------------------------------------

    override protected def createAbstractTypeSymbol(name: TypeName, pos: Position, newFlags: Long): AbstractTypeSymbol =
      new AbstractTypeSymbol(this, pos, name) with SynchronizedTypeSymbol initFlags newFlags

    override protected def createAliasTypeSymbol(name: TypeName, pos: Position, newFlags: Long): AliasTypeSymbol =
      new AliasTypeSymbol(this, pos, name) with SynchronizedTypeSymbol initFlags newFlags

    override protected def createTypeSkolemSymbol(name: TypeName, origin: AnyRef, pos: Position, newFlags: Long): TypeSkolem =
      new TypeSkolem(this, pos, name, origin) with SynchronizedTypeSymbol initFlags newFlags

    override protected def createClassSymbol(name: TypeName, pos: Position, newFlags: Long): ClassSymbol =
      new ClassSymbol(this, pos, name) with SynchronizedClassSymbol initFlags newFlags

    override protected def createModuleClassSymbol(name: TypeName, pos: Position, newFlags: Long): ModuleClassSymbol =
      new ModuleClassSymbol(this, pos, name) with SynchronizedModuleClassSymbol initFlags newFlags

    override protected def createPackageClassSymbol(name: TypeName, pos: Position, newFlags: Long): PackageClassSymbol =
      new PackageClassSymbol(this, pos, name) with SynchronizedModuleClassSymbol initFlags newFlags

    override protected def createRefinementClassSymbol(pos: Position, newFlags: Long): RefinementClassSymbol =
      new RefinementClassSymbol(this, pos) with SynchronizedClassSymbol initFlags newFlags

    override protected def createImplClassSymbol(name: TypeName, pos: Position, newFlags: Long): ClassSymbol =
      new ClassSymbol(this, pos, name) with ImplClassSymbol with SynchronizedClassSymbol initFlags newFlags

    override protected def createPackageObjectClassSymbol(pos: Position, newFlags: Long): PackageObjectClassSymbol =
      new PackageObjectClassSymbol(this, pos) with SynchronizedClassSymbol initFlags newFlags

    override protected def createMethodSymbol(name: TermName, pos: Position, newFlags: Long): MethodSymbol =
      new MethodSymbol(this, pos, name) with SynchronizedMethodSymbol initFlags newFlags

    override protected def createModuleSymbol(name: TermName, pos: Position, newFlags: Long): ModuleSymbol =
      new ModuleSymbol(this, pos, name) with SynchronizedTermSymbol initFlags newFlags

    override protected def createPackageSymbol(name: TermName, pos: Position, newFlags: Long): ModuleSymbol =
      createModuleSymbol(name, pos, newFlags)

    override protected def createValueParameterSymbol(name: TermName, pos: Position, newFlags: Long) =
      new TermSymbol(this, pos, name) with SynchronizedTermSymbol initFlags newFlags

    override protected def createValueMemberSymbol(name: TermName, pos: Position, newFlags: Long) =
      new TermSymbol(this, pos, name) with SynchronizedTermSymbol initFlags newFlags
  }

// ------- subclasses ---------------------------------------------------------------------

  trait SynchronizedTermSymbol extends TermSymbol with SynchronizedSymbol {
    override def name_=(x: Name) = gilSynchronized { super.name_=(x) }
    override def rawname = gilSynchronized { super.rawname }
    override def referenced: Symbol = gilSynchronized { super.referenced }
    override def referenced_=(x: Symbol) = gilSynchronized { super.referenced_=(x) }
  }

  trait SynchronizedMethodSymbol extends MethodSymbol with SynchronizedTermSymbol {
    override def typeAsMemberOf(pre: Type): Type = gilSynchronized { super.typeAsMemberOf(pre) }
    override def paramss: List[List[Symbol]] = gilSynchronized { super.paramss }
    override def returnType: Type = gilSynchronized { super.returnType }
  }

  trait SynchronizedTypeSymbol extends TypeSymbol with SynchronizedSymbol {
    override def name_=(x: Name) = gilSynchronized { super.name_=(x) }
    override def rawname = gilSynchronized { super.rawname }
    override def typeConstructor: Type = gilSynchronized { super.typeConstructor }
    override def tpe_* : Type = gilSynchronized { super.tpe_* }
    override def tpeHK : Type = gilSynchronized { super.tpeHK }
  }

  trait SynchronizedClassSymbol extends ClassSymbol with SynchronizedTypeSymbol {
    override def associatedFile = gilSynchronized { super.associatedFile }
    override def associatedFile_=(f: AbstractFile) = gilSynchronized { super.associatedFile_=(f) }
    override def thisSym: Symbol = gilSynchronized { super.thisSym }
    override def thisType: Type = gilSynchronized { super.thisType }
    override def typeOfThis: Type = gilSynchronized { super.typeOfThis }
    override def typeOfThis_=(tp: Type) = gilSynchronized { super.typeOfThis_=(tp) }
    override def children = gilSynchronized { super.children }
    override def addChild(sym: Symbol) = gilSynchronized { super.addChild(sym) }
  }

  trait SynchronizedModuleClassSymbol extends ModuleClassSymbol with SynchronizedClassSymbol {
    override def sourceModule = gilSynchronized { super.sourceModule }
    override def implicitMembers: Scope = gilSynchronized { super.implicitMembers }
  }
}


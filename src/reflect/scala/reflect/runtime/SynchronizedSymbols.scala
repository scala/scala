package scala.reflect
package runtime

import internal.Flags.DEFERRED

trait SynchronizedSymbols extends internal.Symbols { self: SymbolTable =>

  override protected def nextId() = synchronized { super.nextId() }

  override protected def freshExistentialName(suffix: String) =
    synchronized { super.freshExistentialName(suffix) }

  // Set the fields which point companions at one another.  Returns the module.
  override def connectModuleToClass(m: ModuleSymbol, moduleClass: ClassSymbol): ModuleSymbol =
    synchronized { super.connectModuleToClass(m, moduleClass) }

  override def newFreeTermSymbol(name: TermName, info: Type, value: => Any, flags: Long = 0L, origin: String = null): FreeTermSymbol =
    new FreeTermSymbol(name, value, origin) with SynchronizedTermSymbol initFlags flags setInfo info

  override def newFreeTypeSymbol(name: TypeName, info: Type, value: => Any, flags: Long = 0L, origin: String = null): FreeTypeSymbol =
    new FreeTypeSymbol(name, value, origin) with SynchronizedTypeSymbol initFlags flags setInfo info

  override protected def makeNoSymbol: NoSymbol = new NoSymbol with SynchronizedSymbol

  trait SynchronizedSymbol extends Symbol {

    override def rawflags = synchronized { super.rawflags }
    override def rawflags_=(x: Long) = synchronized { super.rawflags_=(x) }

    override def rawowner = synchronized { super.rawowner }
    override def owner_=(owner: Symbol) = synchronized { super.owner_=(owner) }

    override def validTo = synchronized { super.validTo }
    override def validTo_=(x: Period) = synchronized { super.validTo_=(x) }

    override def pos = synchronized { super.pos }
    override def setPos(pos: Position): this.type = { synchronized { super.setPos(pos) }; this }

    override def privateWithin = synchronized { super.privateWithin }
    override def privateWithin_=(sym: Symbol) = synchronized { super.privateWithin_=(sym) }

    override def info = synchronized { super.info }
    override def info_=(info: Type) = synchronized { super.info_=(info) }
    override def updateInfo(info: Type): Symbol = synchronized { super.updateInfo(info) }
    override def rawInfo: Type = synchronized { super.rawInfo }

    override def typeParams: List[Symbol] = synchronized { super.typeParams }

    override def reset(completer: Type): this.type = synchronized { super.reset(completer) }

    override def infosString: String = synchronized { super.infosString }

    override def annotations: List[AnnotationInfo] = synchronized { super.annotations }
    override def setAnnotations(annots: List[AnnotationInfo]): this.type = { synchronized { super.setAnnotations(annots) }; this }


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

    override protected def createTermSymbol(name: TermName, pos: Position, newFlags: Long): TermSymbol =
      new TermSymbol(this, pos, name) with SynchronizedTermSymbol initFlags newFlags

    override protected def createMethodSymbol(name: TermName, pos: Position, newFlags: Long): MethodSymbol =
      new MethodSymbol(this, pos, name) with SynchronizedMethodSymbol initFlags newFlags

    override protected def createModuleSymbol(name: TermName, pos: Position, newFlags: Long): ModuleSymbol =
      new ModuleSymbol(this, pos, name) with SynchronizedTermSymbol initFlags newFlags

    override protected def createPackageSymbol(name: TermName, pos: Position, newFlags: Long): ModuleSymbol = createModuleSymbol(name, pos, newFlags)

    // TODO
    // override protected def createValueParameterSymbol(name: TermName, pos: Position, newFlags: Long)
    // override protected def createValueMemberSymbol(name: TermName, pos: Position, newFlags: Long)
  }

// ------- subclasses ---------------------------------------------------------------------

  trait SynchronizedTermSymbol extends TermSymbol with SynchronizedSymbol {
    override def name_=(x: Name) = synchronized { super.name_=(x) }
    override def rawname = synchronized { super.rawname }
    override def referenced: Symbol = synchronized { super.referenced }
    override def referenced_=(x: Symbol) = synchronized { super.referenced_=(x) }
  }

  trait SynchronizedMethodSymbol extends MethodSymbol with SynchronizedTermSymbol {
    override def typeAsMemberOf(pre: Type): Type = synchronized { super.typeAsMemberOf(pre) }
  }

  trait SynchronizedTypeSymbol extends TypeSymbol with SynchronizedSymbol {
    override def name_=(x: Name) = synchronized { super.name_=(x) }
    override def rawname = synchronized { super.rawname }
    override def typeConstructor: Type = synchronized { super.typeConstructor }
    override def tpe: Type = synchronized { super.tpe }
  }

  trait SynchronizedClassSymbol extends ClassSymbol with SynchronizedTypeSymbol {
    override def associatedFile = synchronized { super.associatedFile }
    override def associatedFile_=(f: AbstractFileType) = synchronized { super.associatedFile_=(f) }
    override def thisSym: Symbol = synchronized { super.thisSym }
    override def thisType: Type = synchronized { super.thisType }
    override def typeOfThis: Type = synchronized { super.typeOfThis }
    override def typeOfThis_=(tp: Type) = synchronized { super.typeOfThis_=(tp) }
    override def children = synchronized { super.children }
    override def addChild(sym: Symbol) = synchronized { super.addChild(sym) }
  }

  trait SynchronizedModuleClassSymbol extends ModuleClassSymbol with SynchronizedClassSymbol {
    override def sourceModule = synchronized { super.sourceModule }
    // [Eugene++ to Martin] doesn't override anything. no longer necessary?
    // def sourceModule_=(module: ModuleSymbol) = synchronized { super.sourceModule_=(module) }
    override def implicitMembers: List[Symbol] = synchronized { super.implicitMembers }
  }
}


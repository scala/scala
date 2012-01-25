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
    
  override def newFreeVar(name: TermName, tpe: Type, value: Any, newFlags: Long = 0L): FreeVar =
    new FreeVar(name, value) with SynchronizedTermSymbol initFlags newFlags setInfo tpe

  override protected def makeNoSymbol = new NoSymbol with SynchronizedSymbol
    
  trait SynchronizedSymbol extends Symbol {
    
    override def rawowner = synchronized { super.rawowner }
    override def rawname = synchronized { super.rawname }
    override def rawflags = synchronized { super.rawflags }
    
    override def rawflags_=(x: FlagsType) = synchronized { super.rawflags_=(x) }
    override def name_=(x: Name) = synchronized { super.name_=(x) }
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

    override def reset(completer: Type) = synchronized { super.reset(completer) } 

    override def infosString: String = synchronized { super.infosString } 

    override def annotations: List[AnnotationInfo] = synchronized { super.annotations }
    override def setAnnotations(annots: List[AnnotationInfo]): this.type = { synchronized { super.setAnnotations(annots) }; this } 


// ------ creators -------------------------------------------------------------------

    override def newTermSymbol(name: TermName, pos: Position = NoPosition, newFlags: Long = 0L): TermSymbol =
      new TermSymbol(this, pos, name) with SynchronizedTermSymbol initFlags newFlags
  
    override def newAbstractTypeSymbol(name: TypeName, pos: Position = NoPosition, newFlags: Long = 0L): AbstractTypeSymbol =
      new AbstractTypeSymbol(this, pos, name) with SynchronizedTypeSymbol initFlags newFlags
    
    override def newAliasTypeSymbol(name: TypeName, pos: Position = NoPosition, newFlags: Long = 0L): AliasTypeSymbol =
      new AliasTypeSymbol(this, pos, name) with SynchronizedTypeSymbol initFlags newFlags

    override def newModuleSymbol(name: TermName, pos: Position = NoPosition, newFlags: Long = 0L): ModuleSymbol =
      new ModuleSymbol(this, pos, name) with SynchronizedTermSymbol initFlags newFlags

    override def newMethodSymbol(name: TermName, pos: Position = NoPosition, newFlags: Long = 0L): MethodSymbol =
      new MethodSymbol(this, pos, name) with SynchronizedMethodSymbol initFlags newFlags

    override def newClassSymbol(name: TypeName, pos: Position = NoPosition, newFlags: Long = 0L): ClassSymbol =
      new ClassSymbol(this, pos, name) with SynchronizedClassSymbol initFlags newFlags
      
    override def newModuleClassSymbol(name: TypeName, pos: Position = NoPosition, newFlags: Long = 0L): ModuleClassSymbol =
      new ModuleClassSymbol(this, pos, name) with SynchronizedModuleClassSymbol initFlags newFlags
  
    override def newTypeSkolemSymbol(name: TypeName, origin: AnyRef, pos: Position = NoPosition, newFlags: Long = 0L): TypeSkolem =
      if ((newFlags & DEFERRED) == 0L)
        new TypeSkolem(this, pos, name, origin) with SynchronizedTypeSymbol initFlags newFlags
      else
        new TypeSkolem(this, pos, name, origin) with AbstractTypeMixin with SynchronizedTypeSymbol initFlags newFlags
  }

// ------- subclasses ---------------------------------------------------------------------

  trait SynchronizedTermSymbol extends TermSymbol with SynchronizedSymbol {
    override def referenced: Symbol = synchronized { super.referenced }
    override def referenced_=(x: Symbol) = synchronized { super.referenced_=(x) }
  }

  trait SynchronizedMethodSymbol extends MethodSymbol with SynchronizedTermSymbol {
    override def typeAsMemberOf(pre: Type): Type = synchronized { super.typeAsMemberOf(pre) }
  }

  trait SynchronizedTypeSymbol extends TypeSymbol with SynchronizedSymbol {
    override def typeConstructor: Type = synchronized { super.typeConstructor }
    override def tpe: Type = synchronized { super.tpe }
  }

  trait SynchronizedClassSymbol extends ClassSymbol with SynchronizedTypeSymbol {
    override def sourceFile = synchronized { super.sourceFile }
    override def sourceFile_=(f: AbstractFileType) = synchronized { super.sourceFile_=(f) }
    override def thisSym: Symbol = synchronized { super.thisSym }
    override def thisType: Type = synchronized { super.thisType }
    override def typeOfThis: Type = synchronized { super.typeOfThis }
    override def typeOfThis_=(tp: Type) = synchronized { super.typeOfThis_=(tp) }
    override def children = synchronized { super.children }
    override def addChild(sym: Symbol) = synchronized { super.addChild(sym) }
  }

  trait SynchronizedModuleClassSymbol extends ModuleClassSymbol with SynchronizedClassSymbol {
    override def sourceModule = synchronized { super.sourceModule }
    override def sourceModule_=(module: Symbol) = synchronized { super.sourceModule_=(module: Symbol) }
    override def implicitMembers: List[Symbol] = synchronized { super.implicitMembers }
  }
}
 

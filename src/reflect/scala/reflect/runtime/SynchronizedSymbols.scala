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

    def gilSynchronizedIfNotInited[T](body: => T): T = {
      if (isFullyInitialized) body
      else gilSynchronized { body }
    }

    override def validTo = gilSynchronizedIfNotInited { super.validTo }
    override def info = gilSynchronizedIfNotInited { super.info }
    override def rawInfo: Type = gilSynchronizedIfNotInited { super.rawInfo }

    override def typeParams: List[Symbol] = gilSynchronizedIfNotInited {
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
    override def unsafeTypeParams: List[Symbol] = gilSynchronizedIfNotInited {
      if (isCompilerUniverse) super.unsafeTypeParams
      else {
        if (isMonomorphicType) Nil
        else rawInfo.typeParams
      }
    }

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

  trait SynchronizedTermSymbol extends SynchronizedSymbol

  trait SynchronizedMethodSymbol extends MethodSymbol with SynchronizedTermSymbol {
    // we can keep this lock fine-grained, because it's just a cache over asSeenFrom, which makes deadlocks impossible
    // unfortunately we cannot elide this lock, because the cache depends on `pre`
    private lazy val typeAsMemberOfLock = new Object
    override def typeAsMemberOf(pre: Type): Type = gilSynchronizedIfNotInited { typeAsMemberOfLock.synchronized { super.typeAsMemberOf(pre) } }
  }

  trait SynchronizedModuleSymbol extends ModuleSymbol with SynchronizedTermSymbol

  trait SynchronizedTypeSymbol extends TypeSymbol with SynchronizedSymbol {
    // unlike with typeConstructor, a lock is necessary here, because tpe calculation relies on
    // temporarily assigning NoType to tpeCache to detect cyclic reference errors
    private lazy val tpeLock = new Object
    override def tpe_* : Type = gilSynchronizedIfNotInited { tpeLock.synchronized { super.tpe_* } }
  }

  trait SynchronizedClassSymbol extends ClassSymbol with SynchronizedTypeSymbol

  trait SynchronizedModuleClassSymbol extends ModuleClassSymbol with SynchronizedClassSymbol
}


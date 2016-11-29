package scala
package reflect
package runtime

import scala.collection.immutable
import scala.reflect.internal.Flags._

private[reflect] trait SynchronizedSymbols extends internal.Symbols { self: SymbolTable =>

  private lazy val atomicIds = new java.util.concurrent.atomic.AtomicInteger(0)
  override protected def nextId() = atomicIds.incrementAndGet()

  private lazy val atomicExistentialIds = new java.util.concurrent.atomic.AtomicInteger(0)
  override protected def nextExistentialId() = atomicExistentialIds.incrementAndGet()

  private lazy val _recursionTable = mkThreadLocalStorage(immutable.Map.empty[Symbol, Int])
  override def recursionTable = _recursionTable.get
  override def recursionTable_=(value: immutable.Map[Symbol, Int]) = _recursionTable.set(value)

  // Set the fields which point companions at one another.  Returns the module.
  override def connectModuleToClass(m: ModuleSymbol, moduleClass: ClassSymbol): ModuleSymbol =
    gilSynchronized { super.connectModuleToClass(m, moduleClass) }

  override def newFreeTermSymbol(name: TermName, value: => Any, flags: Long = 0L, origin: String = null): FreeTermSymbol =
    new FreeTermSymbol(name, value, origin) with SynchronizedTermSymbol initFlags flags

  override def newFreeTypeSymbol(name: TypeName, flags: Long = 0L, origin: String = null): FreeTypeSymbol =
    new FreeTypeSymbol(name, origin) with SynchronizedTypeSymbol initFlags flags

  override protected def makeNoSymbol: NoSymbol = new NoSymbol with SynchronizedSymbol

  trait SynchronizedSymbol extends Symbol {

    /** (Things written in this comment only applies to runtime reflection. Compile-time reflection,
     *  especially across phases and runs, is somewhat more complicated, but we won't be touching it,
     *  because at the moment we only care about synchronizing runtime reflection).
     *
     *  As it has been noted on multiple occasions, generally speaking, reflection artifacts aren't thread-safe.
     *  Reasons for that differ from artifact to artifact. In some cases it's quite bad (e.g. types use a number
     *  of non-concurrent compiler caches, so we need to serialize certain operations on types in order to make
     *  sure that things stay deterministic). However, in case of symbols there's hope, because it's only during
     *  initialization that symbols are thread-unsafe. After everything's set up, symbols become immutable
     *  (sans a few deterministic caches that can be populated simultaneously by multiple threads) and therefore thread-safe.
     *
     *  Note that by saying "symbols become immutable" I mean literally that. In a very common case of PackageClassSymbol's,
     *  even when a symbol finishes its initialization and becomes immutable, its info forever remains mutable.
     *  Therefore even if we no longer need to synchronize a PackageClassSymbol after it's initialized, we still have to take
     *  care of its ClassInfoType (or, more precisely, of the underlying Scope), but that's done elsewhere, and
     *  here we don't need to worry about that.
     *
     *  Okay, so now we simply check `Symbol.isInitialized` and if it's true, then everything's fine? Haha, nope!
     *  The thing is that some completers call sym.setInfo when still in-flight and then proceed with initialization
     *  (e.g. see LazyPackageType). Consequently, setInfo sets _validTo to current period, which means that after
     *  a call to setInfo isInitialized will start returning true. Unfortunately, this doesn't mean that info becomes
     *  ready to be used, because subsequent initialization might change the info.
     *
     *  Therefore we need to somehow distinguish between initialized and really initialized symbol states.
     *  Okay, let's do it on per-completer basis. We have seven kinds of completers to worry about:
     *    1) LazyPackageType that initializes packages and their underlying package classes
     *    2) TopClassCompleter that initializes top-level Scala-based class-module companion pairs of static definitions
     *    3) LazyTypeRef and LazyTypeRefAndAlias set up by TopClassCompleter that initialize (transitive members) of top-level classes/modules
     *    4) FromJavaClassCompleter that does the same for both top-level and non-toplevel Java-based classes/modules
     *    5) Fully-initialized signatures of non-class/module Java-based reflection artifacts
     *    6) Importing completer that transfers metadata from one universe to another
     *    7) Signatures of special symbols such as roots and symbolsNotPresentInBytecode
     *
     *  The mechanisms underlying completion are quite complex, and it'd be only natural to suppose that over time we're going to overlook something.
     *  Wrt isThreadsafe we could have two wrong situations: false positives (isThreadsafe = true, but the symbol isn't actually threadsafe)
     *  and false negatives (isThreadsafe = false, but the symbol is actually threadsafe). However, even though both are wrong, only the former
     *  is actively malicious. Indeed, false positives might lead to races, inconsistent state and crashes, while the latter would only cause
     *  `initialize` to be called and a gil to be taken on every potentially auto-initializable operation. Unpleasant yes, but still robust.
     *
     *  What makes me hopeful is that:
     *    1) By default (e.g. if some new completion mechanism gets introduced for a special flavor of symbols and we forget to call markCompleted)
     *       isThreadsafe is always in false negative state, which is unpleasant but safe.
     *    2) Calls to `markCompleted` which are the only potential source of erroneous behavior are few and are relatively easy to place:
     *       just put them just before your completer's `complete` returns, and you should be fine.
     *
     *  upd. Actually, there's another problem of not keeping initialization mask up-to-date. If we're not careful enough,
     *  then it might so happen that getting a certain flag that the compiler assumes to be definitively set will spuriously
     *  return isThreadsafe(purpose = FlagsOp(<flag>)) = false and that will lead to spurious auto-initialization,
     *  which will cause an SO or a cyclic reference or some other crash. I've done my best to go through all possible completers
     *  and call `markFlagsCompleted` where appropriate, but again over time something might be overlooked, so to guard against that
     *  I'm only considering TopLevelPickledFlags to be sources of potential initialization. This ensures that such system flags as
     *  isMethod, isModule or isPackage are never going to auto-initialize.
     */
    override def isThreadsafe(purpose: SymbolOps) = {
      if (isCompilerUniverse) false
      else if (_initialized) true
      else purpose.isFlagRelated && (_initializationMask & purpose.mask & TopLevelPickledFlags) == 0
    }

    /** Communicates with completers declared in scala.reflect.runtime.SymbolLoaders
     *  about the status of initialization of the underlying symbol.
     *
     *  Unfortunately, it's not as easy as just introducing the `markThreadsafe` method that would be called
     *  by the completers when they are really done (as opposed to `setInfo` that, as mentioned above, doesn't mean anything).
     *
     *  Since we also want to auto-initialize symbols when certain methods are being called (`Symbol.hasFlag` for example),
     *  we need to track the identity of the initializer, so as to block until initialization is complete if the caller
     *  comes from a different thread, but to skip auto-initialization if we're the initializing thread.
     *
     *  Just a volatile var is fine, because:
     *    1) Status can only be changed in a single-threaded fashion (this is enforced by gilSynchronized
     *       that effectively guards `Symbol.initialize`), which means that there can't be update conflicts.
     *    2) If someone reads a stale value of status, then the worst thing that might happen is that this someone
     *       is going to spuriously call `initialize`, which is either a gil-protected operation (if the symbol isn't initialized yet)
     *       or a no-op (if the symbol is already initialized), and that is fine in both cases.
     *
     *  upd. It looks like we also need to keep track of a mask of initialized flags to make sure
     *  that normal symbol initialization routines don't trigger auto-init in Symbol.flags-related routines (e.g. Symbol.getFlag).
     *  Due to the same reasoning as above, a single volatile var is enough for to store the mask.
     */
    @volatile private[this] var _initialized = false
    @volatile private[this] var _initializationMask = TopLevelPickledFlags
    override def markFlagsCompleted(mask: Long): this.type = { _initializationMask = _initializationMask & ~mask; this }
    override def markAllCompleted(): this.type = { _initializationMask = 0L; _initialized = true; this }

    def gilSynchronizedIfNotThreadsafe[T](body: => T): T = {
      // TODO: debug and fix the race that doesn't allow us uncomment this optimization
      // if (isCompilerUniverse || isThreadsafe(purpose = AllOps)) body
      // else gilSynchronized { body }
      gilSynchronized { body }
    }

    override def validTo = gilSynchronizedIfNotThreadsafe { super.validTo }
    override def info = gilSynchronizedIfNotThreadsafe { super.info }
    override def rawInfo: Type = gilSynchronizedIfNotThreadsafe { super.rawInfo }
    override def typeSignature: Type = gilSynchronizedIfNotThreadsafe { super.typeSignature }
    override def typeSignatureIn(site: Type): Type = gilSynchronizedIfNotThreadsafe { super.typeSignatureIn(site) }

    override def typeParams: List[Symbol] = gilSynchronizedIfNotThreadsafe {
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
    override def unsafeTypeParams: List[Symbol] = gilSynchronizedIfNotThreadsafe {
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

  trait SynchronizedMethodSymbol extends MethodSymbol with SynchronizedTermSymbol

  trait SynchronizedModuleSymbol extends ModuleSymbol with SynchronizedTermSymbol

  trait SynchronizedTypeSymbol extends TypeSymbol with SynchronizedSymbol {
    // unlike with typeConstructor, a lock is necessary here, because tpe calculation relies on
    // temporarily assigning NoType to tpeCache to detect cyclic reference errors
    private lazy val tpeLock = new Object
    override def tpe_* : Type = gilSynchronizedIfNotThreadsafe { tpeLock.synchronized { super.tpe_* } }
  }

  trait SynchronizedClassSymbol extends ClassSymbol with SynchronizedTypeSymbol

  trait SynchronizedModuleClassSymbol extends ModuleClassSymbol with SynchronizedClassSymbol
}


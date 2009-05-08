package scala.tools.nsc.typechecker;
import scala.collection.mutable.{WeakHashMap, LinkedHashSet}
trait IdeSupport extends Analyzer {
  val global : Global with symtab.IdeSupport
  import global._

  private class ContextInternMap extends WeakHashMap[Context,ref.WeakReference[Context]] {
    var lastContext : Context = _
    override def default(txt : Context) : ref.WeakReference[Context] = {
      if (txt eq NoContext) new ref.WeakReference(NoContext)
      val txt0 = txt.intern0
      lastContext = txt0 // to prevent collection
      val ret = new ref.WeakReference(txt0)
      this(txt0) = ret
      ret
    }
    def intern(txt : Context) = this(txt).get.get
  }
  private val internMap = new ContextInternMap
  override def intern(txt : Context) =
    if (false) super.intern(txt)
    else if (txt.outer eq txt) txt
    else internMap.intern(txt)

  override def newNamer(context : Context) : Namer = new Namer(context)
  class Namer(context: Context) extends super.Namer(context) {
    override protected def setInfo[Sym <: Symbol](sym : Sym)(tpe : LazyType) : Sym = {
      assert(!sym.hasRawInfo || sym.rawInfo == NoType) // type information has already been reset.
      if (currentClient.makeNoChanges) {
        sym.setInfo(tpe)
        try {
          sym.info  // force completion.
        } catch {
          case e =>
        }
        return sym
      }
      object tpe0 extends LazyType with SimpleTypeProxy {
        override def underlying = tpe
        override def complete(sym0 : Symbol) : Unit = {
          if (sym ne sym0) {
            logError("DUPLICATE: " + sym.fullNameString + " "+sym.id + " vs. " + sym0.id, null)
            toComplete -= sym0
          }
          toComplete -= sym
          val pos = sym0.pos match {
          case pos : TrackedPosition => pos
          }
          val oldType = oldTypeFor(sym0)
          oldType match {
            case PolyType(xxx,_) =>
              val i = xxx.elements
              var pause = false
              while (i.hasNext) {
                if (i.next.pos == util.NoPosition) pause = true
              }
              if (pause) {
                assert(true)
              }
            case _=>
          }
          assert(sym0.rawInfo == this)
          val hadTypeErrors = pos.owner != null && pos.owner.hasTypeErrors
          if (pos.owner == null) underlying.complete(sym0) else pos.owner.activate(try {
            underlying.complete(sym0)
          } catch {
          case te : TypeError =>
            pos.owner.typeError(te.getMessage)
            sym0.setInfo(ErrorType)
          })
          (oldType,sym0.info) match {
            case (PolyType(xxx,_),PolyType(yyy,_)) if xxx != yyy =>
              val oldc = xxx
              val newc = yyy
              Console.print("DIFF old=" + oldc.map(sym => sym0 + ":" + sym0.pos).mkString("",",",""))
              Console.println(" new=" + newc.map(sym => sym0+ ":" + sym0.pos).mkString("",",",""))
            case _ =>
          }

          //if (!hadTypeErrors && pos.owner != null && pos.owner.hasTypeErrors) pos.owner.dirtyTyped
          if (pos.owner != null && pos.owner.hasTypeErrors && (!sym0.rawInfo.isComplete || sym0.info == NoType || sym0.info == ErrorType)) {
            // go back to original type.
            val oldType = oldTypeFor(sym0)
            if (oldType != NoType)
              sym0.setInfo(oldType)
          }
        }
      }
      toComplete += sym
      super.setInfo(sym)(tpe0)
    }
    override def enterSym(tree : Tree) : Context = tree match {
    case tree : StubTree =>
      if (tree.symbol == NoSymbol) // reset stub symbol on refresh.
        tree.symbol = tree.underlying.updateNamer(this)
      context
    case tree => super.enterSym(tree)
    }
  }
  override def newTyper(txt : Context) : Typer = new Typer(txt)
  class Typer(context : Context) extends super.Typer(context) {
    override def qualifyingClassContext(tree: Tree, qual: Name, packageOK: Boolean): Context = {
      if (qual.isEmpty) super.qualifyingClassContext(tree, qual, packageOK)
      else {
        var c = context.enclClass
        val client = currentClient
        while (c != NoContext && {
          // register dependency.
          client.notify(qual, c.owner)
          c.owner.owner.info.decls match {
          case scope : HookedScope => scope.record(client, qual)
          case _ =>
          }
          true
        } && c.owner.name != qual) c = c.outer.enclClass
        c
      }
    }
    // no adapting.
    override protected def adapt(tree: Tree, mode: Int, pt: Type): Tree = super.adapt(tree,mode,pt)
    override def typed1(tree: Tree, mode: Int, pt: Type): Tree = tree match {
    case tree : StubTree =>
      if (tree.tpe == null)
        tree.tpe = tree.underlying.updateTyper(this, mode, pt)
      tree
    case tree =>
      try {
        super.typed1(tree, mode, pt)
      } catch {
        case e : TypeError => throw e
        case e : Error => global.check(false, "tree: " + tree + " " + e); throw e
      }
    }
  }
  private val toComplete = new LinkedHashSet[Symbol]
  def finishTyping = while (!toComplete.isEmpty) {
    toComplete.toList.foreach(sym => if (sym.pos match {
    case pos : TrackedPosition if !pos.isValid => toComplete.remove(sym); false
    case _ => true
    }){
      if (sym.info.isComplete) toComplete.remove(sym)
      else {
        sym.info
        if (!sym.info.isComplete) {
          Console.println("not-completing: " + sym)
          toComplete remove sym
        }
      }
    })
  }

  trait TrackedPosition extends global.TrackedPosition {
    def owner : MemoizedTree
    def isValid : Boolean
  }
  trait MemoizedTree {
    def kind : TreeKind
    def pos : TrackedPosition
    def typeIsDirty : Boolean
    def dirtyTyped : Unit
    def useTrees : List[Tree]
    def setUseTrees(uses : List[Tree]) : Unit
    def lastTyped : List[Tree]
    def activate(f : => Unit) : Unit
    def typeError(msg : String) : Unit
    def hasTypeErrors : Boolean
    def shouldBeTyped : Boolean = true
    // probably invalidate parent if its not being validated now
    // most type changes detected via setType.
    protected def typeChanged : Unit
    protected def highlightChanged : Unit
    def lastSymbol = if (lastTyped.isEmpty) NoSymbol else lastTyped.last.symbol
    def lastType = if (lastTyped.isEmpty) null else lastTyped.last.tpe
    protected var namerTxt : Context = NoContext
    protected var typerTxt : Context = NoContext
    protected var mode : Int = 0
    protected var pt : Type = NoType

    def doNamer = if (namerTxt ne NoContext) updateNamer(newNamer(namerTxt))
    def updateNamer(namer : Namer) : Symbol = {
      val makeNoChanges = currentClient.makeNoChanges
      val namerTxt = intern(namer.context)
      if (!makeNoChanges && (this.namerTxt ne namerTxt)) {
        assert(namerTxt.scope ne EmptyScope)
        assert(namerTxt.owner ne NoSymbol)
        this.namerTxt = namerTxt
        dirtyTyped
      }
      val lastSymbol = this.lastSymbol

      def fakeUpdate(trees : List[Tree]) : Symbol = { trees.foreach{
      case tree : DefTree if (tree.symbol != NoSymbol && tree.symbol != null) =>
        // becareful, the symbol could have been rentered!
        var e = namer.context.scope.lookupEntry(tree.symbol.name)
        while (e != null && e.sym != tree.symbol) e = namer.context.scope.lookupNextEntry(e)
        if (e == null) {
          //Console.println("FK-ENTER: " + tree.symbol)
          val sym = namer.enterInScope(tree.symbol)
          if (sym != tree.symbol) {
            Console.println("BAD: " + sym + " " + sym.id + " vs. " + tree.symbol.id)
          }
          import symtab.Flags._
          val set = reuseMap.get(namer.context.scope.asInstanceOf[PersistentScope])
          // could be getter or local, then we need to re-add getter/setter
          if (sym.isGetter && set.isDefined)
            set.get.find(sym0 => sym0.name == nme.getterToSetter(sym.name) && sym0.isSetter) match {
          case None =>
          case Some(setter) =>
            val setter0 = namer.enterInScope(setter)
            assert(setter0 == setter)
          } else if (sym.hasGetter && set.isDefined)
            set.get.find(sym0 => {
              sym0.name == nme.getterName(sym.name) &&
                sym0.isGetter
            }) match {
          case None =>
          case Some(getter) =>
            val getter0 = namer.enterInScope(getter)
            assert(getter0 == getter)
            if (set.isDefined)
              set.get.find(sym => sym.name == nme.getterToSetter(getter.name) && sym.isSetter) match {
            case None =>
            case Some(setter) =>
              val setter0 = namer.enterInScope(setter)
              assert(setter0 == setter)
            }
          } else if (sym.hasFlag(symtab.Flags.LAZY) && sym.lazyAccessor != NoSymbol) {
              if (set.get.find(sym0 => sym0 == sym.lazyAccessor).isDefined) {
                namer.enterInScope(sym.lazyAccessor)
              }
          }
        }
      case _ =>
      }; if (trees.isEmpty) NoSymbol else trees.last.symbol }
      import symtab.Flags._
      if (!typeIsDirty) lastTyped.foreach{
      case tree :DefTree if tree.symbol != null && tree.symbol != NoSymbol && tree.symbol.isClass && tree.symbol.hasFlag(CASE) =>
        var e = namer.context.scope.lookupEntry(tree.symbol.name.toTermName)
        while (e != null && !e.sym.hasFlag(MODULE)) e = namer.context.scope.lookupNextEntry(e)
        Console.println("CHECKING: " + e + " " + (if (e != null) caseClassOfModuleClass.contains(e.sym.moduleClass)))
        if (e == null) dirtyTyped
        // we don't clear caseClassOfModuleClass unless we have to.
        else if (!caseClassOfModuleClass.contains(e.sym.moduleClass)) dirtyTyped
      case tree : DefTree if tree.symbol != null && tree.symbol != NoSymbol =>
      case _ =>
      }

      if (makeNoChanges) {}
      else if (!typeIsDirty && !lastTyped.isEmpty)
        return fakeUpdate(lastTyped)
      else if (namerTxt != NoContext && shouldBeTyped) {} else return fakeUpdate(lastTyped)
      val use = useTrees
      if (makeNoChanges) {}
      else if (use.isEmpty || use.last.symbol != NoSymbol) {
        return fakeUpdate(use) // already named
      }

      if (kind.isTop) namer.context.unit.source.file match {
      case file : io.PlainFile => reloadSource(file)
      case _ =>
      }
      // before we retype, unlink/recycle our previously defined symbols.
      if (!makeNoChanges) lastTyped.foreach{tree =>
        if (tree.symbol != NoSymbol && tree.symbol != null) (namer.context.scope,tree) match {
        case (scope : PersistentScope,tree : DefTree) => if (!tree.symbol.isPackage) reuse(scope, tree.symbol)
        case _ =>
        }
      }
      activate(try {
        use.foreach{tree =>
          namer.enterSym(tree)
        }
      } catch {
        case te : TypeError => typeError(te.getMessage)
      })
      if (!makeNoChanges) use.foreach{tree=>
        if (tree.symbol != null &&
            tree.symbol.isClass &&
            tree.symbol.hasFlag(symtab.Flags.CASE) &&
            tree.symbol.owner != null &&
            tree.symbol.owner.rawInfo.isComplete) {
          var e = tree.symbol.owner.info.decls.lookupEntry(tree.symbol.name.toTermName)
          if (e != null) e.sym.pos match { // retype the object if its in the scope.
          case pos : TrackedPosition if pos.owner != null && pos.owner != MemoizedTree.this =>
            pos.owner.dirtyTyped // hope this works!
          case _ =>
          }
          ()
        }
      }
      if (makeNoChanges) {}
      else if (hasTypeErrors && lastSymbol != null && lastSymbol != NoSymbol && use.last.symbol != lastSymbol) {
        if (use.last.symbol != null && use.last.symbol != NoSymbol) {
          namer.context.scope unlink use.last.symbol
        }
        Console.println("ER-LINK: " + lastSymbol)
        val sym = namer.enterInScope(lastSymbol)
        assert(sym == lastSymbol)
        use.last.symbol = lastSymbol
      }
      if (lastSymbol != NoSymbol && lastSymbol != use.last.symbol) {
        assert(true)
      }
      use.last.symbol
    }
    def doTyper = try {
      if (typerTxt ne NoContext) updateTyper(newTyper(typerTxt), mode, pt)
    } catch {
      case e => logError("doTyper crash", e)
    }
    def updateTyper(typer : Typer, mode : Int, pt : Type) : Type = {
      val typerTxt = intern(typer.context)
      val makeNoChanges = currentClient.makeNoChanges
      if (!makeNoChanges && ((this.typerTxt ne typerTxt) || (this.pt != pt) || (this.mode != mode))) {
        this.typerTxt = typerTxt
        this.pt = pt
        this.mode = mode
        dirtyTyped
      }
      val lastType = this.lastType
      if (makeNoChanges) {}
      else if (typeIsDirty && shouldBeTyped && typerTxt != NoContext) {

      } else if (lastType == null) {
        return NoType
      } else return lastType
      var use = useTrees
      if (use.isEmpty) return lastType;
      if ((use.last.tpe != null)) return use.last.tpe
      if (use.last.symbol == NoSymbol && namerTxt != NoContext)
        updateNamer(newNamer(namerTxt))
      if (makeNoChanges) {
        assert(true)
      }
      activate(try {
        setUseTrees{use = use.map{typer.typed(_,mode,pt)}; use}
      } catch {
        case te : TypeError => typeError(te.getMessage)
      })
      if (!makeNoChanges && hasTypeErrors && lastType != null) {
        use.last.tpe = lastType
      }
      if (!makeNoChanges && !hasTypeErrors && use.last.tpe != null && lastType != null &&
          !compareTypes(use.last.tpe, lastType,Nil)(_.info)) {
        // the type changed in a good way.
        typeChanged
      }
      if (!makeNoChanges && (use.length != lastTyped.length || !use.zip(lastTyped).forall{
        case (t0,t1) => t0.equalsStructure0(t1){
        case (t0:StubTree,t1:StubTree) if t0.underlying == t0.underlying || true => true
        case _ => false
        }
      })) {
        highlightChanged
      }
      if (use.last.tpe == null) ErrorType else use.last.tpe
    }
  }
  trait StubTree extends global.StubTree {
    def underlying : MemoizedTree
    override var symbol : Symbol = NoSymbol
    override def duplicate : this.type = this //throw new Error("not supported")
    override def isType = underlying.kind.isType
    override def isTerm = underlying.kind.isTerm
    override def isDef = underlying.kind.isDef
    override def hasSymbol = underlying.kind.hasSymbol
    override def hashCode = underlying.hashCode
    override def equals(that : Any) = that match {
    case that : StubTree => that.underlying == underlying
    case _ => false
    }
    override def toString = "st-" + underlying.toString
    override def pos = util.NoPosition
  }
}

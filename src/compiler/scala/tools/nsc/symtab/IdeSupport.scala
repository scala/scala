package scala.tools.nsc
package symtab
import scala.tools.nsc.util._
import scala.collection.mutable._
import scala.tools.nsc.io._

trait IdeSupport extends SymbolTable { // added to global, not analyzers.
  trait ScopeClient {
    def changed : Unit = {}
    def addTo(set : => LinkedHashSet[ScopeClient]): Unit = set += this
    def notify(name : Name, scope : HookedScope) : Boolean = false
    def notify(name : Name, sym : Symbol) : Unit = {}
    def verifyAndPrioritize[T](verify : Symbol => Symbol)(pt : Type)(f : => T) : T = f
    def makeNoChanges : Boolean = false
  }
  def check(condition : Boolean, msg : => String) = {
    assert(condition)
    condition
  }

  import CompatibleResult._
  trait TrackedPosition extends Position with ReallyHasClients {
    // symbols without scopes!
    def asOffset : Option[(Int,AbstractFile)]
    private var recycled : List[Symbol] = Nil
    def recycle(sym : Symbol) : Symbol = {
      recycled.foreach{existing => compatible(existing,sym) match {
      case NotCompatible => false
      case GoResult(existing) => return existing
      }}
      recycled = sym :: recycled; sym
    }
    private var scopes : List[((ScopeKind,AnyRef),PersistentScope)] = Nil
    def scopeFor(key : (ScopeKind,AnyRef)) : PersistentScope =
      scopes.find{
      case (key0,scope) if key == key0 => true
      case _ => false
      } match {
      case Some((_,scope)) => reuse(scope)
      case None =>
        val scope = new PersistentScope(key,this)
        scopes = (key,scope) :: scopes
        scope
      }
  }
  // dynamic context
  private object NullClient extends ScopeClient {
    override def addTo(clients : => LinkedHashSet[ScopeClient]) = {}
  }

  def currentClient : ScopeClient = NullClient
  abstract class HookedScope(entry : ScopeEntry) extends Scope(entry) {
    def record(client : ScopeClient, name : Name) = {}
    override def lookupEntry(name : Name) = {
      val client = currentClient
      if (client.notify(name, this)) null // not found
      else {
        record(client, name)
        super.lookupEntry(name)
      }
    }
  }
  private val topDefs = new LinkedHashMap[AbstractFile,LinkedHashSet[ClassSymbol]] {
    override def default(what : AbstractFile) = {
      val set = new LinkedHashSet[ClassSymbol]
      this(what) = set; set
    }
  }
  private val emptySet = new ListBuffer[Symbol]
  val reuseMap = new LinkedHashMap[PersistentScope,ListBuffer[Symbol]] {
    override def default(key : PersistentScope) = emptySet
  }
  def reuse(scope : PersistentScope, sym : Symbol) = {
    var e = scope.lookupEntry(sym.name)
    var delete = List[Symbol]()
    while (e != null && e.sym != sym) {
      if (false && !e.sym.rawInfo.isComplete) {

        delete = e.sym :: delete
      }
      e = scope.lookupNextEntry(e)
    }
    delete.foreach(scope.unlink)
    if (e != null && e.sym == sym) {

      val list = reuseMap.get(scope) match {
        case Some(list) => list
        case None =>
          val list = new ListBuffer[Symbol]
        reuseMap(scope) = list; list
      }
      check(!sym.isPackage, "" +sym)
      import symtab.Flags._
      // if def is abstract, will only unlink its name
      if (sym.isGetter) {
        val setter = scope lookup nme.getterToSetter(sym.name)
        if (setter != NoSymbol && setter.isSetter) {
          list += setter
          scope unlink setter
          //Console.println("RS-UNLINK: " + setter)
        }
      } else if (sym.hasGetter) {
        e = scope lookupEntry nme.getterName(sym.name)
        while (e != null && !e.sym.isGetter && (!e.sym.hasFlag(ACCESSOR) || e.sym.accessed != sym)) {
          e = scope lookupNextEntry e
        }
        if (e != null && check(e.sym.accessed == sym, "accessed" + e.sym.accessed +" vs. " + sym) && check(!e.sym.isSetter, "setter: " + e.sym)) {
          val getter = e.sym
          check(e.sym.accessed == sym && !e.sym.isSetter, e.sym.toString)
          list += getter
          scope unlink getter
          //Console.println("RS-UNLINK: " + getter)
          e = scope lookupEntry nme.getterToSetter(getter.name)
          while (e != null && !e.sym.isSetter) e = scope lookupNextEntry e
          if (e != null) {
            check(getter.accessed == sym, "" + getter + " vs. " + sym)
            val setter = e.sym
            list += setter
            scope unlink setter
            //Console.println("RS-UNLINK: " + setter)
          }
        }
      } else if (sym.hasFlag(Flags.LAZY)) {
        val getter = sym.lazyAccessor
        if (getter != NoSymbol) {
          list += getter
          scope unlink getter
        }
      }
      //Console.println("RS-UNLINK: " + sym)
      list += sym
      scope unlink sym // clear from scope.
    }
  }
  private def reuse(scope : PersistentScope) : PersistentScope = {
    if (currentClient.makeNoChanges) return scope
    val buf = new ListBuffer[Symbol]
    scope.toList.foreach{sym =>
      if (false && sym.hasFlag(Flags.CASE) && sym.hasFlag(Flags.SYNTHETIC)) {
        check(sym != null, "")
      } else {
        buf += sym
        scope unlink sym
      }
    }
    if (!buf.isEmpty) {

      reuseMap.get(scope) match {
      case Some(buf0) => buf.foreach(buf0.+=)
      case None => reuseMap(scope) = buf
      }
    }
    scope
  }

  def reloadSource(file : AbstractFile) = {
    if (!currentClient.makeNoChanges)
      topDefs removeKey file match {
        case None => ;
        case Some(symbols) =>
          symbols.foreach{
            sym =>
              def f(sym : Symbol) = sym.owner.info.decls match {
                case scope : PersistentScope => reuse(scope, (sym))
                case scope =>
                  check(false, scope + " is not persistent")
              }
            if (sym.isModuleClass) {
              if (check(sym.name.isTypeName,"") && sym.hasRawInfo)
                if (sym.linkedModuleOfClass != NoSymbol) f(sym.linkedModuleOfClass)
            } else {
              if (check(sym.name.isTypeName, ""))
                f(sym)
            }
          }
      }
  }

  override def attachSource(clazz : ClassSymbol, file : io.AbstractFile) = {
    topDefs(file) += clazz
    super.attachSource(clazz, file)
  }
  def finishTyping = {
    val clear = reuseMap.toList
    reuseMap.clear
    clear.foreach{
    case (scope,old) => old.foreach{
    case NoSymbol =>
    case sym =>
      // note that we didn't unlink them
      val scope0 = scope
      Console.println("RECYCLE: " + sym + ":" + sym.id + " in " + sym.owner); // + " " + scope0 + " " + scope0.key);
      scope0.invalidate(sym.name)
    }}
    reuseMap.clear
    tracedTypes.toList.foreach{case (sym,oldType) =>
      if (sym.rawInfo != NoType && !sym.rawInfo.isComplete) {
        Console.println("XXX uncompleted: " + sym)
      }
      val syminfo = try {
        sym.info
      } catch {
        case e => check(false, ""+e); NoType
      }

      val resetType = syminfo == NoType || hasError(syminfo)
      if (!resetType && !compareTypes(syminfo, oldType,Nil)(sym => tracedTypes.get(sym) match {
      case None => syminfo
      case Some(oldType) => oldType
      })) (trackedTypes.removeKey(sym) match {
      case Some(set) => set.foreach(_.changed)
      case None =>
      })
      if (resetType) {

        sym.setInfo(oldType) // restore old good type.
      }
    }
    tracedTypes.clear
  }
  def oldTypeFor(sym : Symbol) = tracedTypes.get(sym) match {
  case Some(tpe) => tpe
  case None => NoType
  }

  private def compare0(newP : Any, oldP : Any, syms : List[Symbol])(implicit oldType : Symbol => Type) : Boolean = ((newP,oldP) match {
  case (newP:AnyRef,oldP:AnyRef) if newP eq oldP => true
  case (newP:Type,oldP:Type) => compareTypes(newP,oldP, syms)
  case (newS:Symbol,oldS:Symbol) if compareSyms(newS,oldS,syms) => true
  case (newL:List[a],oldL:List[b]) =>
    var va = newL; var vb = oldL
    while (!va.isEmpty && !vb.isEmpty) {
      if (!compare0(va.head,vb.head,syms)) return false
      va = va.tail; vb = vb.tail
    }
    va.isEmpty && vb.isEmpty
  case (newS:Scope,oldS:Scope) =>
    val set = new LinkedHashSet[Symbol]
    set ++= newS.toList
    oldS.toList.forall{oldS => if (!set.remove(oldS)) {
      var other = newS.lookupEntry(oldS.name)
      while (other != null && !compareTypes(other.sym.info,oldType(oldS), syms))
        other = newS.lookupNextEntry(other)
      other != null
    } else true}
  case (newP,oldP) => newP == oldP
  })
  private def compareSyms(newS : Symbol, oldS : Symbol, syms : List[Symbol])(implicit oldType : Symbol => Type) =
    if (oldS eq newS) {
      if (syms.contains(oldS)) true
      else {
        compareTypes(newS.info, oldType(oldS), newS :: syms)
      }
    } else {
      if (syms.contains(oldS) && syms.contains(newS)) true
      else newS.name == oldS.name && newS.owner == oldS.owner && newS.flags == oldS.flags &&
        compareTypes(newS.info,oldType(oldS), newS :: oldS :: syms)
    }

  def hasError(infoA : Type) : Boolean = {
    if (infoA == ErrorType) return true
    infoA match {
    case MethodType(args,ret) => hasError(ret) || infoA.paramTypes.exists(hasError)
    case PolyType(params,ret) => hasError(ret)
    case TypeBounds(lo,hi) => hasError(lo) || hasError(hi)
    case TypeRef(pre,_,args) => hasError(pre) || args.exists(hasError)
    case _ => false
    }
  }
  def compareTypes(newInfo : Type, oldInfo : Type, syms : List[Symbol])(implicit oldType : Symbol => Type) : Boolean = (newInfo eq oldInfo) || (newInfo.getClass == oldInfo.getClass && ((newInfo,oldInfo) match {
  case (newInfo:ThisType,oldInfo:ThisType) if compare0(newInfo.typeSymbol,oldInfo.typeSymbol,syms) => true
  case (newInfo:Product, oldInfo:Product) =>
    (0 until newInfo.productArity).forall(i =>
      compare0(newInfo.productElement(i), oldInfo.productElement(i),syms))
  }))

  trait HasClients {
    def record(client : ScopeClient, name : Name) : Unit
    def record(client : Function1[PersistentScope,Unit]) : Unit
    def invalidate(from : PersistentScope, name : Name) : Unit
  }

  trait ReallyHasClients extends HasClients {
    private var clients : Map = null
    private var anyClients : LinkedHashSet[Function1[PersistentScope,Unit]] = null
    private class Map extends LinkedHashMap[Int,LinkedHashSet[ScopeClient]] {
      override def default(hash : Int) = {
        val set = new LinkedHashSet[ScopeClient]
        this(hash) = set; set
      }
    }
    def record(client : ScopeClient, name : Name) : Unit =
      client.addTo({
        if (clients eq null) clients = new Map
        clients(name.start)
      })
    def record(client : Function1[PersistentScope,Unit]) = {
      if (anyClients == null) anyClients = new LinkedHashSet[Function1[PersistentScope,Unit]]
      anyClients += client
    }

    override def invalidate(from : PersistentScope, name : Name) : Unit = {
      if (clients ne null) clients.removeKey(name.start) match {
      case Some(clients) => clients.foreach(_.changed)
      case None =>
      }
      if (anyClients != null) {
        var c = anyClients
        anyClients = null
        c.foreach(_.apply(from))
      }
    }
  }


  class PersistentScope(val key : AnyRef, val owner : HasClients) extends HookedScope(null) {
    override def record(client : ScopeClient, name : Name) =
      owner.record(client, name)
    override def invalidate(name : Name) : Unit = owner.invalidate(this,name)
    override def enter(symbol : Symbol) : Symbol = {
      if (currentClient.makeNoChanges) { // might have unpickles.
        return if (lookupEntry(symbol.name) == null)
          super.enter(symbol)
        else symbol
      }
      def finish(symbol : Symbol) = {
        if (symbol.isTypeSkolem) {

        }
        if (symbol.owner.isPackageClass && !symbol.isPackageClass && symbol.sourceFile != null) {

          topDefs(symbol.sourceFile) += (symbol match {
            case symbol : ClassSymbol => symbol
            case symbol : ModuleSymbol => symbol.moduleClass.asInstanceOf[ClassSymbol]
          })
        }
        super.enter(symbol)
      }
      def nuke(existing: Symbol) : Unit = {
        if (existing.isMonomorphicType) existing.resetFlag(Flags.MONOMORPHIC)
        assert(!existing.isPackage)
        existing.setAnnotations(Nil) // reset annotations, we don't look at these.
        if (existing.isModuleClass) {
          //Console.println("NUKE_N: " + existing + " " + existing.id)
        } else {
          existing.setInfo(if (symbol.hasRawInfo) symbol.rawInfo else NoType)
        }
        if (existing.isModule && existing.moduleClass != NoSymbol){
          //Console.println("NUKE_0: " + existing + " " + existing.id)
          //Console.println("NUKE_1: " + existing.moduleClass + " " + existing.moduleClass.id)
          existing.moduleClass.setInfo(if (symbol.moduleClass.hasRawInfo) symbol.moduleClass.rawInfo else NoType)
        }
      }

      def reuse(existing : Symbol) : Symbol = {
        def record(existing : Symbol) = if (existing.hasRawInfo &&
          existing.rawInfo.isComplete && existing.rawInfo != NoType && !hasError(existing.rawInfo)) {
          tracedTypes(existing) = existing.info
        }
        record(existing)
        nuke(existing)
        if (existing.pos == NoPosition) {

        }

        finish(existing)
      }
      val symX = lookup(symbol.name)
      if (symX != NoSymbol) {
        if (symX == symbol) return (symX)
        if (!symbol.hasRawInfo && symX.hasRawInfo && symX.rawInfo.isComplete &&
            symbol.pos.isInstanceOf[TrackedPosition] && symX.pos.isInstanceOf[TrackedPosition] &&
            symbol.pos == symX.pos) compatible(symX, symbol) match {
        case NotCompatible => // do nothing
        case code@GoResult(existing0) =>
          val existing = existing0
          if (code.isInstanceOf[Updated]) {
            invalidate(existing.name)
          }
          nuke(existing)
          return (existing)
        }
      }
      if (symbol == NoSymbol) return symbol
      // catch double defs.
      record(currentClient, symbol.name)

      // Martin: I changed rest of methods to avoid Iterator.remove
      val buf = reuseMap(this)
      if (buf contains symbol) {
        buf -= symbol
        finish(symbol)
      } else buf find { existing =>
        if (existing.hasFlag(symtab.Flags.SYNTHETIC) && existing.name == symbol.name) true
        else (symbol.pos,existing.pos) match {
          case (apos : TrackedPosition, bpos : TrackedPosition) => apos == bpos
          case (apos : OffsetPosition , bpos : OffsetPosition) => apos == bpos
          case _ => existing.name == symbol.name
        }
      } match {
        case Some(existing) =>
          if (check(existing != NoSymbol,"")) {
            val oldName = existing.name
            compatible(existing, symbol) match {
              case NotCompatible =>

              case code@GoResult(existing0) =>
                buf -= existing
                if (code.isInstanceOf[Updated]) {
                  invalidate(oldName)
                  invalidate(existing0.name)
                }
                return (reuse(existing0))
            }
          }
        case None =>
      }
      invalidate(symbol.name)
      return finish(symbol)
    }
  }
  private val tops = new LinkedHashMap[OffsetPosition,Symbol]

  protected def compatible(existing : Symbol, symbol : Symbol) : Result = {
    import scala.tools.nsc.symtab.Flags._
    if (existing.hasRawInfo && symbol.hasRawInfo) {


    }


    if (existing.getClass != symbol.getClass) (existing,symbol) match {
    case (existing:TypeSkolem,symbol:TypeSymbol) =>
      val other = existing.deSkolemize
      return if (!other.isSkolem)
        compatible(other,symbol)
      else NotCompatible
    case _ => return NotCompatible
    }
    if (existing.isGetter != symbol.isGetter) return NotCompatible
    if (existing.isSetter != symbol.isSetter) return NotCompatible
    if (existing.owner != symbol.owner) return NotCompatible
    if (existing.name != symbol.name || existing.name.length != symbol.name.length) {
      val ret = (!existing.name.toString.contains('$') &&
        !symbol.name.toString.contains('$') &&
          !(existing hasFlag SYNTHETIC) && !(symbol hasFlag SYNTHETIC) && {
        existing.name.isTypeName == symbol.name.isTypeName &&
          nme.isSetterName(existing.name) == nme.isSetterName(symbol.name) &&
            nme.isLocalName(existing.name) == nme.isLocalName(symbol.name)
      })
      if (!ret) return NotCompatible
    }
    // because module var shares space with monomorphic.
    if (existing.isModuleVar != symbol.isModuleVar) return NotCompatible
    if ((existing.flags|LOCKED|INTERFACE|MONOMORPHIC|DEFERRED|ABSTRACT|PRIVATE|PROTECTED|FINAL|SEALED|CASE|SYNTHETIC) !=
        (symbol.  flags|LOCKED|INTERFACE|MONOMORPHIC|DEFERRED|ABSTRACT|PRIVATE|PROTECTED|FINAL|SEALED|CASE|SYNTHETIC)) {
      return NotCompatible
    }
    if (((existing.flags&(MONOMORPHIC|INTERFACE)) != 0) ||
        ((symbol  .flags&(MONOMORPHIC|INTERFACE)) != 0)) {

    }
    val ret = (existing.owner == symbol.owner || {
      existing.owner.name == symbol.owner.name && // why????
        (existing.owner.name == nme.ANON_FUN_NAME||symbol.owner.name == nme.ANON_FUN_NAME) &&
          existing.owner.pos == symbol.owner.pos
    })
    if (!ret) return NotCompatible
    existing.setPos(symbol.pos) // not significant for updating purposes.
    if ((existing.privateWithin != symbol.privateWithin ||
        existing.name != symbol.name || ((existing.flags|LOCKED|MONOMORPHIC|INTERFACE) != (symbol.flags|LOCKED|MONOMORPHIC|INTERFACE)))) {
      existing.name = (symbol.name)
      // don't reset the monomorphic bit until we reset the type.
      existing.flags = symbol.flags
      existing.privateWithin = symbol.privateWithin
      return new Updated(existing)
    }
    return new Compatible(existing)
  }
  protected object CompatibleResult {
    abstract class Result {
      def map(symbol : Symbol) : Result = this
    }
    case object NotCompatible extends Result
    case class GoResult(val symbol : Symbol) extends Result {
    }
    class Compatible(override val symbol : Symbol) extends GoResult(symbol) {
      override def map(symbol : Symbol) = new Compatible(symbol)
    }
    class Updated(override val symbol : Symbol) extends GoResult(symbol) {
      override def map(symbol : Symbol) = new Updated(symbol)
    }
  }

  private class DefInfo extends ReallyHasClients {
    var ref : scala.ref.WeakReference[Symbol] = _
    var scopes : List[(PersistentScope)] = Nil
    def scope(kind : ScopeKind) = scopes.find(_.key == kind) match {
    case Some(scope) => scope
    case None =>
      val scope = new PersistentScope(kind,this)
      check(scope.key == kind, ""+scope.key + " " + scope.toString)
      scopes = (scope) :: scopes
      scope
    }
  }

  private val defMap = new WeakHashMap[Symbol,DefInfo] {
    override def default(clazz : Symbol) = {
      val ref = new scala.ref.WeakReference(clazz)
      val info = new DefInfo
      this(clazz) = info
      info.ref = ref
      info
    }
  }
  override def newClassScope(clazz : Symbol) = {
    newDefScope0({
      if (clazz.isModuleClass && !clazz.isPackageClass) {

        clazz
      } else if (clazz.isModule && !clazz.isPackage) {

        clazz.moduleClass
      } else clazz
    }, ClassKind)
  }
  private lazy val ClassKind = allocateScopeKind("class")
  private def newDefScope0(sym : Symbol, key : ScopeKind) = reuse(defMap(sym).scope(key))

  override def recycle(sym : Symbol) = sym.pos match {
  case pos : TrackedPosition => pos.recycle(sym)
  case _ => super.recycle(sym)
  }
  override def newLocalDummy(clazz : Symbol, pos : util.Position) =
    recycle(super.newLocalDummy(clazz,pos)).asInstanceOf[TermSymbol]

  def newScope(pos : Position, key : (ScopeKind,AnyRef), old : Option[Scope]) : Scope = pos match {
  case pos : TrackedPosition => pos.scopeFor(key)
  case _ if old.isEmpty => newScope(null : ScopeEntry)
  case _ => super.scopeFor(old.get, null,key._1)
  }

  private def scopeFor00(tree : Tree, old : Option[Scope], kind : ScopeKind) = (tree,tree.symbol) match {
  case (_,null|NoSymbol) => newScope(tree.pos, (kind,tree.getClass), (old))
  case (tree : DefTree, sym) => newDefScope0((sym),kind) // indexed by symbol
  case _ => newScope(tree.pos, (kind,tree.getClass), old)
  }

  override def scopeFor(old : Scope, tree : Tree, kind : ScopeKind) = scopeFor00(tree, Some(old), kind)
  override def scopeFor(tree : Tree, kind : ScopeKind) = scopeFor00(tree, None, kind)
  override def newScope(initElements : ScopeEntry) : Scope = {
    object owner extends ReallyHasClients
    new PersistentScope(null, owner)
  }
  override def newPackageScope(depends0 : PackageScopeDependMap) : PackageScope = {
    object owner extends ReallyHasClients
    object myPackageScope extends PersistentScope(null, owner) with PackageScope {
      val depends = depends0
    }
    myPackageScope
  }


  override def newTempScope : Scope = new TemporaryScope
  private class TemporaryScope extends HookedScope(null) {
    override def hashCode = toList.map(_.hashCode).foldLeft(0)(_ + _)
    override def equals(that : Any) = that match {
    case that : TemporaryScope if this eq that => true
    case that : TemporaryScope => // do a brute force comparison
      val l0 = this.toList
      val l1 = that.toList
      l0.size == l1.size && l0.forall(l1.contains)
    case _ => false
    }
  }
  private class ThrowAwayScope(decls : List[Symbol]) extends HookedScope(null:ScopeEntry) {
    decls.foreach(d => enter(d))
  }
  override def newThrowAwayScope(decls : List[Symbol]) : Scope= new ThrowAwayScope(decls)

  private val trackedTypes = new LinkedHashMap[Symbol,LinkedHashSet[ScopeClient]] {
    override def default(sym : Symbol) = {
      val set = new LinkedHashSet[ScopeClient]
      this(sym) = set; set
    }
  }
  // trace symbols whose types are watched!
  private val tracedTypes = new LinkedHashMap[Symbol,Type]

  override def trackTypeIDE(sym : Symbol): Boolean = if (sym != NoSymbol && !sym.isPackageClass && !sym.isPackage) {
    // will wind up watching a lot of stuff!
    currentClient.addTo(trackedTypes(sym))
    super.trackTypeIDE(sym)
  } else super.trackTypeIDE(sym)
  override def mkConstantType(value: Constant): ConstantType = {
    super.mkConstantType(Constant(value.value match {
    case _ : Int => 0 : Int
    case _ : Long => 0 : Long
    case _ : Byte => 0 : Byte
    case _ : Char => 0 : Char
    case _ : Short => 0 : Short
    case _ : Float => 0 : Float
    case _ : Double => 0 : Double
    case _ : String => "string"
    case _ : scala.Symbol => Symbol("symbol")
    case value => value
    }))
  }
  // mostly intellisense hacks.
  override def verifyAndPrioritize[T](verify : Symbol => Symbol)(pt : Type)(f : => T) : T = {
    try {
    currentClient.verifyAndPrioritize(verify)(pt)(f)
    } catch {case e : Error=>
      throw e
    }
  }
  override def compare(sym : Symbol, name : Name) = {
    val client = currentClient
    sym.info.decls match {
    case scope : HookedScope =>
      scope.record(client, name)
    case _ =>
    }
    client.notify(name, sym)
    super.compare(sym, name)
  }
  override def notifyImport(what : Name, container : Type, from : Name, to : Name) : Unit = {
    super.notifyImport(what, container, from, to)
    // sanity checking.
    if ((container eq null) ||
        (what      eq null) ||
        (from      eq null) ||
        (currentClient eq null)) return
    val from0 = if (what.isTypeName) from.toTypeName else from.toTermName
    val result = container.member(from0)
    if (result != NoSymbol)
      currentClient.notify(what, result)
  }

  object lightDuplicator extends Transformer {
    override val treeCopy = new StrictTreeCopier
  }
  // make the trees less detailed.
  override def sanitize(tree : Tree) : Tree = lightDuplicator.transform(tree match {
  case Template(_,vdef,_) => Template(Nil, sanitize(vdef).asInstanceOf[ValDef], Nil)
  case PackageDef(pid, _) => PackageDef(pid, Nil)
  case DefDef(mods, _, _, _, _:TypeTree, _) => DefDef(NoMods, nme.ERROR, Nil, Nil, TypeTree(), Literal(()))
  case DefDef(mods, _, _, _, restpt, _) => DefDef(NoMods, nme.ERROR, Nil, Nil, sanitize(restpt), Literal(()))
  case ValDef(_, _, _ : TypeTree, _) => ValDef(NoMods, nme.ERROR, TypeTree(), EmptyTree)
  case ValDef(_, _, restpt, _) => ValDef(NoMods, nme.ERROR, sanitize(restpt), EmptyTree)
  case ModuleDef(_, _, impl) =>
    ModuleDef(NoMods, nme.ERROR, sanitize(impl).asInstanceOf[Template])
  case ClassDef(_, _, tparams, impl) =>
    ClassDef(NoMods, nme.ERROR.toTypeName, Nil, sanitize(impl).asInstanceOf[Template])
  case DocDef(_, definition) => sanitize(definition)
  case CaseDef(x, y, z) => CaseDef(Literal(()), EmptyTree, Literal(()))
  case Block(_, _) => Block(Nil, Literal(()))
  case Function(vparams,body) =>
    Function(vparams.map(sanitize).asInstanceOf[List[ValDef]],Literal(()))
  case _ => tree
  }).setPos(tree.pos)

}

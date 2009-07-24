/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc
package symtab

trait Scopes {
  self: SymbolTable =>

  def entryIterator(e: ScopeEntry): Iterator[ScopeEntry] =
    if (e == null) Iterator.empty else e.entryIterator

  def bucketIterator(e: ScopeEntry): Iterator[ScopeEntry] =
    if (e == null) Iterator.empty else e.bucketIterator

  class ScopeEntry(val sym: Symbol, val owner: Scope) {
    /** the next entry in the hash bucket
     */
    var tail: ScopeEntry = null

    /** the next entry in this scope
     */
    var next: ScopeEntry = null

    class ScopeEntryIterator(f: ScopeEntry => ScopeEntry) extends Iterator[ScopeEntry] {
      private var buf: ScopeEntry = ScopeEntry.this
      def hasNext = buf != null
      def next = {
        val res = buf
        buf = f(buf)
        res
      }
    }

    def bucketIterator: Iterator[ScopeEntry] = new ScopeEntryIterator(_.tail)
    def entryIterator: Iterator[ScopeEntry] = new ScopeEntryIterator(_.next)

    override def hashCode(): Int = sym.name.start
    override def toString(): String = sym.toString()
  }

  /**
   *  @param sym   ...
   *  @param owner ...
   *  @return      ...
   */
  def newScopeEntry(sym: Symbol, owner: Scope): ScopeEntry = {
    val e = new ScopeEntry(sym, owner)
    e.next = owner.elems
    owner.elems = e
    e
  }

  object NoScopeEntry extends ScopeEntry(NoSymbol, null)

  /**
   *  @param initElems ...
   *  @return          ...
   */
  def newScope(initElems: ScopeEntry): Scope = new NormalScope(initElems)

  final def newScope: Scope = newScope(null: ScopeEntry)

  trait PackageScopeDependMap {
    def createDepend(from : Symbol, name : Name) : Unit
  }

  def newPackageScope(depends0 : PackageScopeDependMap) : PackageScope = {
    object MyPackageScope extends NormalScope(null : ScopeEntry) with PackageScope {
      val depends = depends0
    }
    MyPackageScope
  }

  def newTempScope = newScope(null : ScopeEntry)
  class ScopeKind(name : String) { override def toString = name }
  def allocateScopeKind(name : String) = new ScopeKind(name)
  lazy val Constructor0ScopeKind : ScopeKind = allocateScopeKind("constructors0")
  lazy val Constructor1ScopeKind : ScopeKind = allocateScopeKind("constructors1")
  lazy val InnerScopeKind : ScopeKind = allocateScopeKind("inner")
  lazy val FinishWithScopeKind : ScopeKind = allocateScopeKind("finishWith")
  lazy val TypeSigScopeKind : ScopeKind = allocateScopeKind("typeSig")
  lazy val PolyTypeCompleterScopeKind : ScopeKind = allocateScopeKind("polyType")
  lazy val CompoundTreeScopeKind : ScopeKind = allocateScopeKind("compoundTree")
  lazy val FreshArgScopeKind : ScopeKind = allocateScopeKind("freshArgs")
  lazy val LabelScopeKind : ScopeKind = allocateScopeKind("label")
  lazy val TypedCasesScopeKind : ScopeKind = allocateScopeKind("typedCases")
  lazy val TypedDefScopeKind : ScopeKind = allocateScopeKind("typedDef")
  //lazy val ParentTypesScopeKind : ScopeKind = allocateScopeKind("parentType")
  lazy val TypedScopeKind : ScopeKind = allocateScopeKind("typed")
  // have to sometimes use constructor depth unfortunately.
  case class ParentTypesScopeKind(clazz : Symbol) extends ScopeKind("parentType") { override def toString = super.toString + "-" + clazz }
  case class BlockScopeKind(depth : Int) extends ScopeKind("block") { override def toString = super.toString + "-" + depth }

  def newClassScope(clazz : Symbol) = newScope // for use in ClassInfoType creation
  def scopeFor(             tree : Tree, kind : ScopeKind) : Scope = newScope
  def scopeFor(old : Scope, tree : Tree, kind : ScopeKind) : Scope = newScope(old)

  final def newScope(base: Scope) : Scope = newScope(base.elems)
  final def newScope(decls: List[Symbol]) : Scope = {
    val ret = newScope
    decls foreach ret.enter
    ret
  }
  def newThrowAwayScope(decls : List[Symbol]) : Scope = newScope(decls)
  // for symbols that don't exist in scopes!
  def recycle(sym : Symbol) : Symbol = sym
  def newLocalDummy(clazz : Symbol, pos : util.Position) = clazz.newLocalDummy(pos)

  private class NormalScope(initElems: ScopeEntry) extends Scope(initElems)

  trait PackageScope extends Scope {
    val depends : PackageScopeDependMap
    override def lookupEntryWithContext(name : Name)(from : Symbol) = {
      if (from != NoSymbol && depends != null) {
        depends.createDepend(from,name)
      }
      super.lookupEntryWithContext(name)(from)
    }
    override def lookupWithContext(name : Name)(from : Symbol) = {
      if (from != NoSymbol && depends != null) {
        depends.createDepend(from,name)
      }
      super.lookupWithContext(name)(from)
    }
  }

  abstract class Scope(initElems: ScopeEntry) {

    var elems: ScopeEntry = initElems

    /** The number of times this scope is neted in another
     */
    private var nestinglevel = 0

    /** the hash table
     */
    private var hashtable: Array[ScopeEntry] = null
    private def hashtableIterator(index: Int): Iterator[ScopeEntry] =
      if (hashtable == null) Iterator.empty
      else bucketIterator(hashtable(index))

    /** a cache for all elements, to be used by symbol iterator.
     */
    private var elemsCache: List[Symbol] = null

    /** size and mask of hash tables
     *  todo: make hashtables grow?
     */
    private val HASHSIZE = 0x80
    private val HASHMASK = 0x7f

    /** the threshold number of entries from which a hashtable is constructed.
     */
    private val MIN_HASH = 8

    if (size >= MIN_HASH) createHash()

    def this() = this(null: ScopeEntry)

    def this(base: Scope) = {
      this(base.elems)
      nestinglevel = base.nestinglevel + 1
    }

    def this(decls: List[Symbol]) = {
      this()
      decls foreach enter
    }

    /** Returns a new scope with the same content as this one. */
    def cloneScope: Scope = {
      val clone = newScope
      this.toList foreach (clone enter _)
      clone
    }
    /* clear the contents of this scope */
    def clear = {
      elems = null
      elemsCache = null
      hashtable = null
    }


    /** is the scope empty? */
    def isEmpty: Boolean = elems eq null

    /** the number of entries in this scope */
    def size: Int = entryIterator(elems).length

    /** enter a scope entry
     *
     *  @param e ...
     */
    def enter(e: ScopeEntry) {
      elemsCache = null
      if (hashtable ne null) {
        val i = e.sym.name.start & HASHMASK
        elems.tail = hashtable(i)
        hashtable(i) = elems
      } else if (size >= MIN_HASH) {
        createHash()
      }
    }

    /** enter a symbol
     *
     *  @param sym ...
     */
    def enter(sym: Symbol) : Symbol = { enter(newScopeEntry(sym, this)); sym }

    /** enter a symbol, asserting that no symbol with same name exists in scope
     *
     *  @param sym ...
     */
    def enterUnique(sym: Symbol) {
      assert(lookup(sym.name) == NoSymbol)
      enter(sym)
    }

    private def createHash() {
      hashtable = new Array[ScopeEntry](HASHSIZE)
      enterInHash(elems)
    }

    private def enterInHash(e: ScopeEntry) {
      if (e ne null) {
        enterInHash(e.next)
        val i = e.sym.name.start & HASHMASK
        e.tail = hashtable(i)
        hashtable(i) = e
      }
    }

    def rehash(sym: Symbol, newname: Name) {
      if (hashtable ne null) {
        val index = sym.name.start & HASHMASK
        var e1 = hashtable(index)
        var e: ScopeEntry = null
        if (e1 != null) {
          if (e1.sym == sym) {
            hashtable(index) = e1.tail
            e = e1
          } else {
            while (e1.tail != null && e1.tail.sym != sym) e1 = e1.tail
            if (e1.tail != null) {
              e = e1.tail
              e1.tail = e.tail
            }
          }
        }
        if (e != null) {
          val newindex = newname.start & HASHMASK
          e.tail = hashtable(newindex)
          hashtable(newindex) = e
        }
      }
    }

    /** remove entry
     *
     *  @param e ...
     */
    def unlink(e: ScopeEntry) {
      if (elems == e) {
        elems = e.next
      } else {
        var e1 = elems
        while (e1.next != e) e1 = e1.next
        e1.next = e.next
      }
      if (hashtable ne null) {
        val index = e.sym.name.start & HASHMASK
        var e1 = hashtable(index)
        if (e1 == e) {
          hashtable(index) = e.tail
        } else {
          while (e1.tail != e) e1 = e1.tail;
          e1.tail = e.tail
        }
      }
      elemsCache = null
    }

    /** remove symbol */
    def unlink(sym: Symbol) {
      var e = lookupEntry(sym.name)
      while (e ne null) {
        if (e.sym == sym) unlink(e);
        e = lookupNextEntry(e)
      }
    }

    /** lookup a symbol
     *
     *  @param name ...
     *  @return     ...
     */
    def lookup(name: Name): Symbol = {
      val e = lookupEntry(name)
      if (e eq null) NoSymbol else e.sym
    }
    /** Can lookup symbols and trace who the client is.
     */
    def lookupEntryWithContext(name : Name)(from : Symbol) : ScopeEntry = lookupEntry(name)
    def lookupWithContext(name : Name)(from : Symbol) : Symbol = lookup(name)

    /** lookup a symbol entry matching given name.
     *  @note from Martin: I believe this is a hotspot or will be one
     *  in future versions of the type system. I have reverted the previous
     *  change to use iterators as too costly.
     */
    def lookupEntry(name: Name): ScopeEntry = {
      var e: ScopeEntry = null
      if (hashtable ne null) {
        e = hashtable(name.start & HASHMASK)
        while ((e ne null) && e.sym.name != name) {
          e = e.tail
        }
      } else {
        e = elems
        while ((e ne null) && e.sym.name != name) {
          e = e.next
        }
      }
      e
    }

    /** lookup next entry with same name as this one
     *  @note from Martin: I believe this is a hotspot or will be one
     *  in future versions of the type system. I have reverted the previous
     *  change to use iterators as too costly.
     */
    def lookupNextEntry(entry: ScopeEntry): ScopeEntry = {
      var e = entry
      if (hashtable ne null)
        do { e = e.tail } while ((e ne null) && e.sym.name != entry.sym.name)
      else
        do { e = e.next } while ((e ne null) && e.sym.name != entry.sym.name);
      e
    }

    /** Return all symbols as a list in the order they were entered in this scope.
     */
    def toList: List[Symbol] = {
      if (elemsCache eq null) {
        elemsCache = Nil
        var e = elems
        while ((e ne null) && e.owner == this) {
          elemsCache = e.sym :: elemsCache
          e = e.next
        }
      }
      elemsCache
    }

    /** Return all symbols as an interator in the order they were entered in this scope.
     */
    def iterator: Iterator[Symbol] = toList.iterator

    @deprecated("use `iterator'") def elements = iterator

    def filter(p: Symbol => Boolean): Scope =
      if (!(toList forall p)) newScope(toList filter p) else this

    def mkString(start: String, sep: String, end: String) =
      toList.map(_.defString).mkString(start, sep, end)

    override def toString(): String = mkString("{\n  ", ";\n  ", "\n}")

    /** Return the nesting level of this scope, i.e. the number of times this scope
     *  was nested in another */
    def nestingLevel = nestinglevel

    def invalidate(name : Name) = {}
  }

  /** The empty scope (immutable).
   */
  object EmptyScope extends Scope {
    override def enter(e: ScopeEntry) {
      throw new Error("EmptyScope.enter")
    }
  }

  /** The error scope.
   */
  class ErrorScope(owner: Symbol) extends Scope(null: ScopeEntry) {
    override def lookupEntry(name: Name): ScopeEntry = {
      def errorScope =
        if (name.isTermName) owner newErrorValue name
        else owner newErrorClass name

      super.lookupEntry(name) match {
        case NoScopeEntry   => enter(errorScope) ; super.lookupEntry(name)
        case e              => e
      }
    }
  }
}


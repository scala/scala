/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.symtab

trait Scopes requires SymbolTable {

  class ScopeEntry(val sym: Symbol, val owner: Scope) {
    /** hook to notify IDE that new symbol has been added to this scope */
    owner.enter00(sym);


    /** the next entry in the hash bucket
     */
    var tail: ScopeEntry = _

    /** the next entry in this scope
     */
    var next: ScopeEntry = null

    override def hashCode(): int = sym.name.start
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
  final def newScope(base: Scope) : Scope = newScope(base.elems)
  final def newScope(decls: List[Symbol]) : Scope = {
    val ret = newScope
    decls.foreach(d => ret.enter(d))
    ret
  }

  private class NormalScope(initElems: ScopeEntry) extends Scope(initElems)

  abstract class Scope(initElems: ScopeEntry)  {

    var elems: ScopeEntry = initElems

    /** hook for IDE
     */
    protected def enter0(sym : Symbol) : Unit = {}
    private[Scopes] def enter00(sym : Symbol) = enter0(sym);

    /** The number of times this scope is neted in another
     */
    private var nestinglevel = 0

    /** the hash table
     */
    private var hashtable: Array[ScopeEntry] = null

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

    if (size >= MIN_HASH) createHash

    def this() = this(null: ScopeEntry)

    def this(base: Scope) = {
      this(base.elems)
/*
      if (base.hashtable ne null) {
        this.hashtable = new Array[ScopeEntry](HASHSIZE)
        System.arraycopy(base.hashtable, 0, this.hashtable, 0, HASHSIZE)
      }
*/
      nestinglevel = base.nestinglevel + 1
    }

    def this(decls: List[Symbol]) = {
      this()
      decls foreach enter
    }

    /** Returns a new scope with the same content as this one. */
    def cloneScope: Scope = {
      val clone = newScope
      this.toList foreach clone.enter
      clone
    }

    /** is the scope empty? */
    def isEmpty: boolean = elems eq null

    /** the number of entries in this scope */
    def size: int = {
      var s = 0
      var e = elems
      while (e ne null) {
        s = s + 1
        e = e.next
      }
      s
    }

    /** enter a scope entry
     *
     *  @param e ...
     */
    def enter(e: ScopeEntry): unit = {
      elemsCache = null
      if (hashtable ne null) {
        val i = e.sym.name.start & HASHMASK
        elems.tail = hashtable(i)
        hashtable(i) = elems
      } else if (size >= MIN_HASH) {
        createHash
      }
    }

    /** enter a symbol
     *
     *  @param sym ...
     */
    def enter(sym: Symbol): unit = enter(newScopeEntry(sym, this))

    /** enter a symbol, asserting that no symbol with same name exists in scope
     *
     *  @param sym ...
     */
    def enterUnique(sym: Symbol): unit = {
      assert(lookup(sym.name) == NoSymbol)
      enter(sym)
    }

    private def createHash: unit = {
      hashtable = new Array[ScopeEntry](HASHSIZE)
      enterInHash(elems)
    }

    private def enterInHash(e: ScopeEntry): unit =
      if (e ne null) {
        enterInHash(e.next)
        val i = e.sym.name.start & HASHMASK
        e.tail = hashtable(i)
        hashtable(i) = e
      }

    /** remove entry
     *
     *  @param e ...
     */
    def unlink(e: ScopeEntry): unit = {
      if (elems == e) {
        elems = e.next
      } else {
        var e1 = elems
        while (e1.next != e) e1 = e1.next;
        e1.next = e.next
      }
      if (hashtable ne null) {
        var e1 = hashtable(e.sym.name.start & HASHMASK)
        if (e1 == e) {
          hashtable(e.sym.name.start & HASHMASK) = e.tail
        } else {
          while (e1.tail != e) e1 = e1.tail;
          e1.tail = e.tail
        }
      }
      elemsCache = null
    }

    /** remove symbol */
    def unlink(sym: Symbol): unit = {
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

    /** lookup a symbol entry matching given name.
     *
     *  @param name ...
     *  @return     ...
     */
    def lookupEntry(name: Name): ScopeEntry = {
      var e: ScopeEntry = null
      if (false & (hashtable ne null)) {
        e = hashtable(name.start & HASHMASK)
        while ((e ne null) && e.sym.name != name) e = e.tail;
      } else {
        e = elems
        while ((e ne null) && e.sym.name != name) {
          e = e.next;
        }
      }
      e
    }

    /** lookup next entry with same name as this one */
    def lookupNextEntry(entry: ScopeEntry): ScopeEntry = {
      var e = entry
      if (hashtable ne null) //debug
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
    def elements: Iterator[Symbol] = toList.elements

    def filter(p: Symbol => boolean): Scope =
      if (!(toList forall p)) newScope(toList filter p) else this

    def mkString(start: String, sep: String, end: String) =
      toList.map(.defString).mkString(start, sep, end)

    override def toString(): String = mkString("{\n  ", ";\n  ", "\n}")

    /** Return the nesting level of this scope, i.e. the number of times this scope
     *  was nested in another */
    def nestingLevel = nestinglevel
  }

  /** The empty scope (immutable).
   */
  object EmptyScope extends Scope {
    override def enter(e: ScopeEntry): unit =
      throw new Error("EmptyScope.enter")
  }

  /** The error scope.
   */
  class ErrorScope(owner: Symbol) extends Scope(null: ScopeEntry) {
    override def lookupEntry(name: Name): ScopeEntry = {
      val e = super.lookupEntry(name)
      if (e != NoSymbol) e
      else {
        enter(if (name.isTermName) owner.newErrorValue(name)
              else owner.newErrorClass(name))
        super.lookupEntry(name)
      }
    }
  }
}


/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package internal

trait Scopes extends api.Scopes { self: SymbolTable =>

  class ScopeEntry(val sym: Symbol, val owner: Scope) {
    /** the next entry in the hash bucket
     */
    var tail: ScopeEntry = null

    /** the next entry in this scope
     */
    var next: ScopeEntry = null

    override def hashCode(): Int = sym.name.start
    override def toString(): String = sym.toString()
  }

  /**
   *  @param sym   ...
   *  @param owner ...
   *  @return      ...
   */
  private def newScopeEntry(sym: Symbol, owner: Scope): ScopeEntry = {
    val e = new ScopeEntry(sym, owner)
    e.next = owner.elems
    owner.elems = e
    e
  }

  object Scope {
    def unapplySeq(decls: Scope): Some[Seq[Symbol]] = Some(decls.toList)
  }

  /** Note: constructor is protected to force everyone to use the factory methods newScope or newNestedScope instead.
   *  This is necessary because when run from reflection every scope needs to have a 
   *  SynchronizedScope as mixin. 
   */
  class Scope protected[Scopes] (initElems: ScopeEntry = null) extends Iterable[Symbol] {
    
    protected[Scopes] def this(base: Scope) = {
      this(base.elems)
      nestinglevel = base.nestinglevel + 1
    }

    private[scala] var elems: ScopeEntry = initElems

    /** The number of times this scope is nested in another
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

    if (size >= MIN_HASH) createHash()

    /** Returns a new scope with the same content as this one. */
    def cloneScope: Scope = newScopeWith(this.toList: _*)

    /** is the scope empty? */
    override def isEmpty: Boolean = elems eq null

    /** the number of entries in this scope */
    override def size: Int = {
      var s = 0
      var e = elems
      while (e ne null) {
        s += 1
        e = e.next
      }
      s
    }

    /** enter a scope entry
     *
     *  @param e ...
     */
    protected def enter(e: ScopeEntry) {
      elemsCache = null
      if (hashtable ne null)
        enterInHash(e)
      else if (size >= MIN_HASH)
        createHash()
    }

    private def enterInHash(e: ScopeEntry): Unit = {
      val i = e.sym.name.start & HASHMASK
      e.tail = hashtable(i)
      hashtable(i) = e
    }

    /** enter a symbol
     *
     *  @param sym ...
     */
    def enter(sym: Symbol): Symbol = { enter(newScopeEntry(sym, this)); sym }

    /** enter a symbol, asserting that no symbol with same name exists in scope
     *
     *  @param sym ...
     */
    def enterUnique(sym: Symbol) {
      assert(lookup(sym.name) == NoSymbol, (sym.fullLocationString, lookup(sym.name).fullLocationString))
      enter(sym)
    }

    private def createHash() {
      hashtable = new Array[ScopeEntry](HASHSIZE)
      enterAllInHash(elems)
    }

    private def enterAllInHash(e: ScopeEntry, n: Int = 0) {
      if (e ne null) {
        if (n < maxRecursions) {
          enterAllInHash(e.next, n + 1)
          enterInHash(e)
        } else {
          var entries: List[ScopeEntry] = List()
          var ee = e
          while (ee ne null) {
            entries = ee :: entries
            ee = ee.next
          }
          entries foreach enterInHash
        }
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

    /** Returns an iterator yielding every symbol with given name in this scope.
     */
    def lookupAll(name: Name): Iterator[Symbol] = new Iterator[Symbol] {
      var e = lookupEntry(name)
      def hasNext: Boolean = e ne null
      def next(): Symbol = { val r = e.sym; e = lookupNextEntry(e); r }
    }

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
    override def toList: List[Symbol] = {
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

    /** Return the nesting level of this scope, i.e. the number of times this scope
     *  was nested in another */
    def nestingLevel = nestinglevel

    /** Return all symbols as an iterator in the order they were entered in this scope.
     */
    def iterator: Iterator[Symbol] = toList.iterator

/*
    /** Does this scope contain an entry for `sym`?
     */
    def contains(sym: Symbol): Boolean = lookupAll(sym.name) contains sym

    /** A scope that contains all symbols of this scope and that also contains `sym`.
     */
    def +(sym: Symbol): Scope =
      if (contains(sym)) this
      else {
        val result = cloneScope
        result enter sym
        result
      }

    /** A scope that contains all symbols of this scope except `sym`.
     */
    def -(sym: Symbol): Scope =
      if (!contains(sym)) this
      else {
        val result = cloneScope
        result unlink sym
        result
      }
*/
    override def foreach[U](p: Symbol => U): Unit = toList foreach p

    override def filter(p: Symbol => Boolean): Scope =
      if (!(toList forall p)) newScopeWith(toList filter p: _*) else this

    override def mkString(start: String, sep: String, end: String) =
      toList.map(_.defString).mkString(start, sep, end)

    override def toString(): String = mkString("Scope{\n  ", ";\n  ", "\n}")

  }

  /** Create a new scope */
  def newScope: Scope = new Scope()
  
  /** Create a new scope nested in another one with which it shares its elements */
  def newNestedScope(outer: Scope): Scope = new Scope(outer)

  /** Create a new scope with given initial elements */
  def newScopeWith(elems: Symbol*): Scope = {
    val scope = newScope
    elems foreach scope.enter
    scope
  }

  /** Create new scope for the members of package `pkg` */
  def newPackageScope(pkgClass: Symbol): Scope = newScope

  /** Transform scope of members of `owner` using operation `op`
   *  This is overridden by the reflective compiler to avoid creating new scopes for packages
   */
  def scopeTransform(owner: Symbol)(op: => Scope): Scope = op


  /** The empty scope (immutable).
   */
  object EmptyScope extends Scope {
    override def enter(e: ScopeEntry) {
      abort("EmptyScope.enter")
    }
  }

  /** The error scope.
   */
  class ErrorScope(owner: Symbol) extends Scope

  private final val maxRecursions = 1000

}


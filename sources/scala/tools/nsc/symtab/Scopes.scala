package scala.tools.nsc.symtab;

abstract class Scopes: SymbolTable {

  abstract class ScopeEntry(val sym: Symbol, val owner: Scope) {

    /** the next entry in the hash bucket
     */
    var tail: ScopeEntry = _;

    /** the next entry in this scope
     */
    var next: ScopeEntry = null;

    override def hashCode(): int = sym.name.start;
    override def toString(): String = sym.toString();
  }

  def newScopeEntry(sym: Symbol, owner: Scope) = new ScopeEntry(sym, owner) {
    next = owner.elems;
    owner.elems = this;
  }

  object NoScopeEntry extends ScopeEntry(NoSymbol, null);

  private val emptyIterator: Iterator[ScopeEntry] = Iterator.empty;

  private class EntryIterator(init: ScopeEntry, godeep: boolean) extends Iterator[ScopeEntry] {
    private var e: ScopeEntry = init;
    val owner = init.owner;
    def hasNext: boolean = e != null && (godeep || e.owner == owner)
    def next: ScopeEntry = {
      val result = e;
      if (e != null)
        if (hashtable != null)
	  do { e = e.tail } while (e != null && e.sym.name != name)
	else
	  do { e = e.next } while (e != null && e.sym.name != name);
      result
    }
  }

  class Scope(initElems: ScopeEntry)  {

    var elems: ScopeEntry = initElems;

    /** the hash table
     */
    private var hashtable: Array[ScopeEntry] = null;

    /** a cache for all elements, to be used by symbol iterator.
     */
    private var elemsCache: List[Symbol] = null;

    /** size and mask of hash tables
     *  todo: make hashtables grow?
     */
    private val HASHSIZE = 0x80;
    private val HASHMASK = 0x7f;

    /** the threshold number of entries from which a hashtable is constructed.
     */
    private val MIN_HASH = 8;

    if (size >= MIN_HASH) createHash;

    def this() = this(null: ScopeEntry);

    def this(base: Scope) = {
      this(base.elems);
      if (base.hashtable != null) {
	this.hashtable = new Array[ScopeEntry](HASHSIZE);
        System.arraycopy(base.hashtable, 0, this.hashtable, 0, HASHSIZE);
      }
    }

    def this(members: List[Symbol]) = {
      this();
      members foreach enter
    }

    /** Returns a new scope with the same content as this one. */
    def cloneScope: Scope = {
      val clone = new Scope();
      this.toList foreach clone.enter;
      clone
    }

    /** is the scope empty? */
    def isEmpty: boolean = elems == null;

    /** the number of entries in this scope */
    def size: int = {
      var s = 0;
      var e = elems;
      while (e != null) {
        s = s + 1;
        e = e.next
      }
      s
    }

    /** enter a scope entry
     */
    def enter(e: ScopeEntry): unit = {
      elems = e;
      elemsCache = null;
      if (hashtable != null) {
	val i = e.sym.name.start & HASHMASK;
	elems.tail = hashtable(i);
	hashtable(i) = elems;
      } else if (size >= MIN_HASH) {
	createHash;
      }
    }

    /** enter a symbol
     */
    def enter(sym: Symbol): unit = {
      enter(newScopeEntry(sym, this));
    }

    private def createHash: unit = {
      hashtable = new Array[ScopeEntry](HASHSIZE);
      enterInHash(elems);
    }

    private def enterInHash(e: ScopeEntry): unit = {
      if (e != null) {
	enterInHash(e.next);
	val i = e.sym.name.start & HASHMASK;
	e.tail = hashtable(i);
	hashtable(i) = e;
      }
    }

    /** remove entry
     */
    def unlink(e: ScopeEntry): unit = {
      if (elems == e) {
	elems = e.next;
      } else {
	var e1 = elems;
	while (e1.next != e) e1 = e1.next;
	e1.next = e.next;
      }
      if (hashtable != null) {
	var e1 = hashtable(e.sym.name.start & HASHMASK);
	if (e1 == e) {
	  hashtable(e.sym.name.start & HASHMASK) = e.tail;
	} else {
	  while (e1.tail != e) e1 = e1.tail;
	  e1.tail = e.tail;
	}
      }
      elemsCache = null
    }

    /** remove symbol */
    def unlink(sym: Symbol): unit =
      for (val e <- lookupAllEntries(sym.name, true))
        if (e.sym == sym) unlink(e);

    /** lookup a symbol
     */
    def lookup(name: Name): Symbol = {
      val e = lookupEntry(name);
      if (e == null) NoSymbol else e.sym;
    }

    /** lookup a symbol entry matching given name
     */
    def lookupEntry(name: Name): ScopeEntry = {
      var e: ScopeEntry = null;
      if (hashtable != null) {
	e = hashtable(name.start & HASHMASK);
	while (e != null && e.sym.name != name) e = e.tail;
      } else {
	e = elems;
	while (e != null && e.sym.name != name) e = e.next;
      }
      e
    }

    /** Iterates through all symbol entries matching given name.
     *  Iteration is from last declared to first declared.
     */
    def lookupAllEntries(name: Name, godeep: boolean): Iterator[ScopeEntry] = {
      val e = lookupEntry(name);
      if (e == null) emptyIterator else new EntryIterator(e, godeep);
    }

    /** Iterates through all symbols matching given name.
     *  Iteration is from last declared to first declared.
     */
    def lookupAll(name: Name, godeep: boolean): Iterator[Symbol] =
      lookupAllEntries(name, godeep) map (.sym);

    /** Return all symbols as a list in the order they were entered in this scope.
     */
    def toList: List[Symbol] = {
      if (elemsCache == null) {
        elemsCache = Nil;
        var e = elems;
        while (e != null) {
          elemsCache = e.sym :: elemsCache;
          e = e.next
        }
      }
      elemsCache
    }

    override def toString(): String =
      toList.map(.defString).mkString("{\n  ", ";\n  ", "\n}");
  }

  /** The empty scope (immutable).
   */
  object EmptyScope extends Scope {
    override def enter(e: ScopeEntry): unit =
      throw new Error("EmptyScope.enter");
  }

  /** The error scope.
   */
  class ErrorScope(owner: Symbol) extends Scope(null: ScopeEntry) {
    override def lookupEntry(name: Name): ScopeEntry = {
      val e = super.lookupEntry(name);
      if (e != NoSymbol) e
      else {
        enter(if (name.isTermName) owner.newErrorValue(name)
              else owner.newErrorClass(name));
        super.lookupEntry(name);
      }
    }
  }
}


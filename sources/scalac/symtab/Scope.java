 /*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.symtab;

import scalac.util.*;
import scalac.ApplicationError;

public class Scope {

    public static abstract class SymbolIterator {
	public abstract boolean hasNext();
	public abstract Symbol next();
    }

    /** A symbol iterator that returns all alternatives of an overloaded symbol
     *  instead of the overloaded symbol itself.
     */
    public static class UnloadIterator extends SymbolIterator {
        private SymbolIterator iterator;
        private Symbol[] alternatives;
        private int index;

        public UnloadIterator(SymbolIterator iterator) {
            this.iterator = iterator;
            this.alternatives = null;
            this.index = -1;
        }

        public boolean hasNext() {
            return index >=  0 || iterator.hasNext();
        }
        public Symbol next() {
            if (index >= 0) {
                Symbol symbol = alternatives[index++];
                if (index == alternatives.length) {
                    alternatives = null;
                    index = -1;
                }
                return symbol;
            } else {
                Symbol symbol = iterator.next();
                switch (symbol.type()) {
                case OverloadedType(Symbol[] alts, _):
                    alternatives = alts;
                    index = 0;
                    return next();
                default:
                    return symbol;
                }
            }
        }
    }

    public static class Entry {

	/** the absent entry
	 */
	public static final Entry NONE = new Entry();

	/** the symbol of the entry (this is the symbol containing the name)
	 */
	public Symbol sym;

	/** the next entry in the hash bucket
	 */
	private Entry tail;

	/** the next entry in this scope
	 */
	private Entry next;

	/** The owner of the entry;
	 */
	public final Scope owner;

	public Entry(Symbol sym, Scope owner) {
	    this.sym = sym;
	    this.owner = owner;
	    this.next = owner.elems;
	    if (sym == null) throw new ApplicationError();
	    owner.elems = this;
	}

	private Entry() {
	    this.sym = Symbol.NONE;
            this.owner = null;
	}

	public Entry setSymbol(Symbol sym) {
	    this.sym = sym;
	    owner.elemsCache = null;
	    return this;
	}

	public int hashCode() {
	    return sym.name.index;
	}

	public String toString() {
	    return sym.toString();
	}
    }

    /** all elements of this scope
     */
    private Entry elems;

    /** the hash table
     */
    private Entry[] hashtable;

    /** a cache for all elements, to be used by symbol iterator.
     */
    private Symbol[] elemsCache = null;

    /** size and mask of hash tables
     *  todo: make hashtables grow?
     */
    private final int HASHSIZE = 0x80;
    private final int HASHMASK = 0x7f;

    /** the threshold number of entries from which a hashtable is constructed.
     */
    private final int MIN_HASH = 6;

    /** construct a new name space
     */
    public Scope() {
	this.elems = Entry.NONE;
    }

    public Scope(Entry elems) {
	this.elems = elems;
	if (size() >= MIN_HASH) createHash();
    }

    public Scope(Scope base) {
	this.elems = base.elems;
	if (base.hashtable != null) {
	    this.hashtable = new Entry[HASHSIZE];
	    for (int i = 0; i < HASHSIZE; i++)
		hashtable[i] = base.hashtable[i];
	}
    }

    public Scope(Symbol[] members) {
	this();
	for (int i = 0; i < members.length; i++)
	    enter(members[i]);
    }

    /** Returns a new scope with the same content as this one. */
    public Scope cloneScope() {
        int size = 0;
        Scope clone = new Scope();
        for (Entry e = elems; e != Entry.NONE; e = e.next, size++)
            new Entry(e.sym, clone);
        if (size >= MIN_HASH) clone.createHash();
        return clone;
    }

    /** is the scope empty?
     */
    public boolean isEmpty() {
	return elems == Entry.NONE;
    }

    /** the number of entries in this scope
     */
    int size() {
	int s = 0;
	for (Entry e = elems; e != Entry.NONE; e = e.next) s++;
	return s;
    }

    public Scope enter(Entry e) {
	elems = e;
	elemsCache = null;
	if (hashtable != null) {
	    int i = e.sym.name.index & HASHMASK;
	    elems.tail = hashtable[i];
	    hashtable[i] = elems;
	} else if (size() >= MIN_HASH) {
	    createHash();
	}
	return this;
    }

    /** enter a symbol
     */
    public Scope enter(Symbol sym) {
	// assert !sym.isConstructor();
	return enter(new Entry(sym, this));
    }
    public final Scope enterNoHide(Symbol sym) {
        assert lookupEntry(sym.name) == Entry.NONE:
            sym + " hides " + lookup(sym.name);
        return enter(sym);
    }

    public Scope enterOrOverload(Symbol sym) {
	Entry e = lookupEntry(sym.name);
	if (e.owner == this/* && (sym.flags & Modifiers.PRIVATE) == 0*/) {
	    e.setSymbol(e.sym.overloadWith(sym));
	    return this;
	} else {
	    return enter(sym);
	}
    }

    private void createHash() {
	hashtable = new Entry[HASHSIZE];
	for (int i = 0; i < HASHSIZE; i++)
	    hashtable[i] = Entry.NONE;
	enterInHash(elems);
    }

    private void enterInHash(Entry e) {
	if (e != Entry.NONE) {
	    enterInHash(e.next);
	    int i = e.sym.name.index & HASHMASK;
	    e.tail = hashtable[i];
	    hashtable[i] = e;
	}
    }

    /** remove entry
     */
    public void unlink(Entry e) {
	if (elems == e) {
	    elems = e.next;
	} else {
	    Entry e1 = elems;
	    while (e1.next != e) e1 = e1.next;
	    e1.next = e.next;
	}
	if (hashtable != null) {
	    Entry e1 = hashtable[e.sym.name.index & HASHMASK];
	    if (e1 == e) {
		hashtable[e.sym.name.index & HASHMASK] = e.tail;
	    } else {
		while (e1.tail != e) e1 = e1.tail;
		e1.tail = e.tail;
	    }
	}
	elemsCache = null;
    }

    public boolean contains(Symbol sym) {
        Entry e = lookupEntry(sym.name);
        if (e.sym == sym) return true;
        switch (e.sym.type()) {
        case OverloadedType(Symbol[] alts, _):
            for (int i = 0; i < alts.length; i++)
                if (alts[i] == sym) return true;
        }
        return false;
    }

    /** lookup a symbol
     */
    public Symbol lookup(Name name) {
	return lookupEntry(name).sym;
    }

    /** lookup a symbol entry.
     */
    public Entry lookupEntry(Name name) {
	Entry e;
	if (hashtable != null) {
	    e = hashtable[name.index & HASHMASK];
	    while (e != Entry.NONE && e.sym.name != name) e = e.tail;
	} else {
	    e = elems;
	    while (e != Entry.NONE && e.sym.name != name) e = e.next;
	}
	return e;
    }

    /** return all symbols as an array,
     *  in the order they were entered in this scope.
     */
    public Symbol[] elements() {
	if (elemsCache == null) {
	    int s = size();
	    elemsCache = new Symbol[s];
	    for (Entry e = elems; e != Entry.NONE; e = e.next)
		elemsCache[--s] = e.sym;
	}
	return elemsCache;
    }

    /** return all symbols as an iterator,
     *  in the order they were entered in this scope.
     */
    public SymbolIterator iterator() { return new MySymbols(); }

    public SymbolIterator iterator(boolean unload) {
        SymbolIterator iterator = iterator();
        return unload ? new UnloadIterator(iterator) : iterator;
    }

    class MySymbols extends SymbolIterator {

	private int index;
	MySymbols() {
	    elements();
	    index = 0;
	}

	public boolean hasNext() {
	    return index < elemsCache.length;
	}

	public Symbol next() {
	    return elemsCache[index++];
	}
    }

    public String toString() {
        return new SymbolTablePrinter().printScope(this).toString();
    }

    public static Scope EMPTY = new Scope();
}


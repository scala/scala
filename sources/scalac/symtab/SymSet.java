/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.symtab;

/** Sets of symbols, implemented as binary trees.
 */
public class SymSet {
    private SymSet l, r;
    private Symbol sym;

    public SymSet() {}

    private SymSet(Symbol sym, SymSet l, SymSet r) {
	this.sym = sym; this.l = l; this.r = r;
    }

    /** Union of this set and `{sym}'.
     */
    public SymSet incl(Symbol sym) {
	if (this == EMPTY) {
	    return new SymSet(sym, EMPTY, EMPTY);
	} else if (sym == this.sym) {
	    return this;
	} else if (less(sym, this.sym)) {
	    return new SymSet(this.sym, l.incl(sym), r);
	} else {
	    assert less(this.sym, sym);
	    return new SymSet(this.sym, l, r.incl(sym));
	}
    }

    public SymSet excl(Symbol sym) {
	if (sym == this.sym) {
	    if (l == EMPTY) return r;
	    if (r == EMPTY) return l;
	    SymSet m = r;
	    if (m.l != EMPTY) {
		SymSet p;
		do {
		    p = m;
		    m = m.l;
		} while (m.l != EMPTY);
		p.l = m.r;
		m.r = r;
	    }
	    m.l = l;
	    return m;
	} else if (less(sym, this.sym)) {
	    return new SymSet(this.sym, l.excl(sym), r);
	} else {
	    assert less(this.sym, sym);
	    return new SymSet(this.sym, l, r.excl(sym));
	}
    }

    /** Is `sym' an element of this set?
     */
    public boolean contains(Symbol sym) {
	if (this == EMPTY) {
	    return false;
	} else if (sym == this.sym) {
	    return true;
	} else if (less(sym, this.sym)) {
	    return l.contains(sym);
	} else {
	    assert less(this.sym, sym);
	    return r.contains(sym);
	}
    }

    /** The number of elements in ths set.
     */
    public int size() {
	if (this == EMPTY) {
	    return 0;
	} else {
	    return 1 + l.size() + r.size();
	}
    }

    /** Copy elements of this set into `ss', starting at index `from'.
     *  Return index one past last element copied.
     */
    public int copyToArray(Symbol[] ss, int from) {
	if (this == EMPTY) {
	    return from;
	} else {
	    from = l.copyToArray(ss, from);
	    ss[from] = sym;
	    return r.copyToArray(ss, from + 1);
	}
    }

    /** Return all elements of this set as an array.
     */
    public Symbol[] toArray() {
	int s = size();
	if (s == 0) return Symbol.EMPTY_ARRAY;
	Symbol[] ss = new Symbol[s];
	copyToArray(ss, 0);
	return ss;
    }

    /** The empty set.
     */
    public final static SymSet EMPTY = new SymSet();

    private boolean less(Symbol s1, Symbol s2) {
        return s1.isLess(s2);
    }
}

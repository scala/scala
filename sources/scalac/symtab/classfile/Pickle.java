/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.symtab.classfile;

import scala.tools.util.Position;
import java.util.HashMap;
import java.io.*;
import scalac.Global;
import scalac.ApplicationError;
import scalac.atree.AConstant;
import scalac.util.*;
import scalac.symtab.*;
import Symbol.*;
import Type.*;

public class Pickle implements Kinds, Modifiers, EntryTags {

    final static boolean debug = false;

/***************************************************
 * Symbol table attribute format: see EntryTags.java
 */
    public byte[] bytes;
    private int bp;

    private Name rootname;
    private Symbol rootowner;
    private HashMap index;
    private Object[] entries;
    private int ep;

    /** Write symbol table info for root.
     *  root must be either a module or a class.
     */
    public Pickle() {
	index = new HashMap();
	entries = new Object[256];
	ep = 0;
    }

    /** Pickle all symbols descending from `root'.
     */
    public void add(Symbol root) {
	if (!root.isExternal()) {
	    if (Global.instance.debug) System.out.println("pickling " + root);
	    if (index.get(root) == null) {
		this.rootname = root.name.toTermName();
		this.rootowner = root.owner();
		putSymbol(root);
	    }
	}
    }

    /** Finalize pickler with given fullname.
     */
    public void pickle() {
	bytes = new byte[4096];
	bp = 0;
	writeAttr();
	this.index = null;
	this.entries = null;
    }

    /** The number of elements defined in `bytes'.
     */
    public int size() {
	return bp;
    }

/* **************************************************
 * Phase 1: Build entry table
 ************************************************* */

    /** Is root in symbol.owner*?
     */
    private boolean isLocal(Symbol sym) {
	return
	    sym.name.toTermName() == rootname && sym.owner() == rootowner
	    ||
	    sym.isConstructor() && isLocal(sym.constructorClass())
	    ||
	    (sym.kind != NONE && isLocal(sym.owner()));
    }

    /** Store entry `e' in index at next available position unless it it
     *  already there. Return true iff entry is new.
     */
    private boolean putEntry(Object e) {
	Integer n = (Integer) index.get(e);
	if (n == null) {
	    //System.out.println("entry " + e);//DEBUG
	    if (ep == entries.length) {
		Object[] entries1 = new Object[ep * 2];
		System.arraycopy(entries, 0, entries1, 0, ep);
		entries = entries1;
	    }
	    entries[ep] = e;
	    index.put(e, new Integer(ep));
	    ep++;
	    return true;
	} else {
	    return false;
	}
    }

    /** Store symbol in index. If symbol is local, also store
     *  everything it refers to.
     */
    private void putSymbol(Symbol sym) {
	if (putEntry(sym)) {
	    if (debug) System.out.println("put " + sym);
	    if (isLocal(sym)) {
		putEntry(sym.name);
		putSymbol(sym.isConstructor() ? sym.constructorClass() : sym.owner());
		switch (sym.kind) {
		case TYPE:
		    if (sym.isViewBounded()) putType(sym.vuBound());
		    else putType(sym.info());
		    putType(sym.loBound());
		    break;
		case ALIAS:
		    putType(sym.info());
		    putSymbol(sym.allConstructors());
		    break;
		case CLASS:
		    putType(sym.info());
                    if (sym.isModuleClass()) putSymbol(sym.sourceModule());
		    putType(sym.typeOfThis());
		    putSymbol(sym.allConstructors());
		    for (Scope.SymbolIterator it = sym.members().iterator();
			 it.hasNext();)
			putSymbol(it.next());
		    break;
		case VAL:
		    putType(sym.info());
		    if (sym.isConstructor() &&
			sym == sym.constructorClass().allConstructors())
			putSymbol(sym.constructorClass());
		    else if (sym.isModule())
			putSymbol(sym.moduleClass());
		    break;
		default:
		    throw new ApplicationError();
		}
	    } else if (sym.kind != NONE) {
		putEntry(sym.isModuleClass() || sym.isRoot() ? sym.name.toTermName() : sym.name);
		if (!sym.owner().isRoot())
		    putSymbol(sym.owner());
	    }
	}
    }

    private void putSymbols(Symbol[] syms) {
	for (int i = 0; i < syms.length; i++)
	    putSymbol(syms[i]);
    }

    /** Store type and everythig it refers to in index.
     */
    private void putType(Type tp) {
	if (putEntry(tp)) {
	    switch (tp) {
	    case NoType:
		break;
	    case NoPrefix:
		putSymbol(Symbol.NONE);
                // !!! code above is usefull for compatibility
                // !!! nothing would be better line
		break;
	    case ThisType(Symbol sym):
		putSymbol(sym);
		break;
	    case SingleType(Type pre, Symbol sym):
		putType(pre);
		putSymbol(sym);
		break;
	    case ConstantType(Type base, AConstant value):
		putType(base);
                putConstant(value);
		break;
	    case TypeRef(Type pre, Symbol sym, Type[] args):
		putType(pre);
		putSymbol(sym);
		putTypes(args);
		break;
	    case CompoundType(Type[] parents, Scope members):
                putSymbol(tp.symbol());
		putTypes(parents);
		break;
	    case MethodType(Symbol[] vparams, Type result):
		putType(result);
		for (int i = 0; i < vparams.length; i++) {
		    Type ptype = vparams[i].type();
		    putType(ptype);
		    int pflags = vparams[i].flags;
		    if ((pflags & (REPEATED | DEF)) != 0)
			putEntry(new FlagsAndType(encodeFlags(pflags), ptype));
		}
		break;
	    case PolyType(Symbol[] tparams, Type result):
		putType(result);
		putSymbols(tparams);
		break;
	    case OverloadedType(Symbol[] alts, Type[] alttypes):
		for (int i = 0; i < alts.length; i++) alts[i].flags |= ALTERNATIVE;
		putSymbols(alts);
		putTypes(alttypes);
		break;
	    default:
		throw new ApplicationError();
	    }
	}
    }

    private void putTypes(Type[] tps) {
	for (int i = 0; i < tps.length; i++)
	    putType(tps[i]);
    }

    private void putConstant(AConstant constant) {
        if (putEntry(constant)) {
            switch (constant) {
            case STRING(String value):
                putEntry(Name.fromString(value));
                return;
            }
        }
    }

/* **************************************************
 * Phase 2: Write byte array
 ************************************************* */

    private void resizeTo(int size) {
	byte[] bytes1 = new byte[size];
	System.arraycopy(bytes, 0, bytes1, 0, bp);
	bytes = bytes1;
    }

    /** Write a byte of data
     */
    private void writeByte(int b) {
	if (bp == bytes.length) resizeTo(bytes.length * 2);
	bytes[bp++] = (byte)b;
	if (debug) System.out.print(b + " ");
    }

    /** Write a natural number in big endian format, base 128.
     *  All but the last digits have bit 0x80 set.
     */
    private void writeNat(int x) {
	int y = x >>> 7;
	if (y != 0) writeNatPrefix(y);
	writeByte(x & 0x7f);
    }

    private void writeNatPrefix(int x) {
	int y = x >>> 7;
	if (y != 0) writeNatPrefix(y);
	writeByte((x & 0x7f) | 0x80);
    }

    /** Write a natural number at `pos'
     *  If number is more than one byte, shift rest of array to make space.
     */
    private void patchNat(int pos, int x) {
	bytes[pos] = (byte) (x & 0x7f);
	int y = x >>> 7;
	if (y != 0) patchNatPrefix(pos, y);
    }

    private void patchNatPrefix(int pos, int x) {
	writeByte(0);
	System.arraycopy(bytes, pos, bytes, pos+1, bp - (pos+1));
	bytes[pos] = (byte) ((x & 0x7f) | 0x80);
	int y = x >>> 7;
	if (y != 0) patchNatPrefix(pos, y);
    }

    /** Write a long number in signed big endian format, base 256.
     */
    private void writeLong(long x) {
	long y = x >> 8;
	long z = x & 0xff;
	if (-y != z >> 7) writeLong(y);
	writeByte((int) z);
    }

    /** Write a reference to object, i.e., the object's number in the index.
     */
    private void writeRef(Object ref) {
	Integer i = (Integer) index.get(ref);
	assert i != null : ref + " " + ref.getClass();
	writeNat(i.intValue());
    }

    private void writeRefs(Object[] es) {
	for (int i = 0; i < es.length; i++) writeRef(es[i]);
    }

    /** Write a name entry. Names are stored in Utf8 format.
     */
    private void writeName(Name name) {
	writeByte(name.isTermName() ? TERMname : TYPEname);
	writeByte(0); // space for length
        byte[] ascii = name.toAsciiUnsafe();
	while (bp + ascii.length > bytes.length) resizeTo(bytes.length * 2);
        System.arraycopy(ascii, 0, bytes, bp, ascii.length);
	if (debug) System.out.print(name);
	bp = bp + ascii.length;
    }

    /** Write a symbol entry.
     */
    private void writeSymbol(Symbol sym) {
	if (debug) System.out.println("write " + sym);
	if (isLocal(sym)) {
	    switch (sym.kind) {
	    case TYPE:
		writeByte(TYPEsym);
		break;
	    case ALIAS:
		writeByte(ALIASsym);
		break;
	    case CLASS:
		writeByte(CLASSsym);
		break;
	    case VAL:
		writeByte(VALsym);
		break;
	    default:
		throw new ApplicationError();
	    }
	    writeByte(0); // space for length
	    writeRef(sym.name);
	    writeRef(sym.isConstructor() ? sym.constructorClass() : sym.owner());
	    writeNat(sym.flags);
	    switch (sym.kind) {
	    case TYPE:
		if (sym.isViewBounded()) writeRef(sym.vuBound());
		else writeRef(sym.info());
		writeRef(sym.loBound());
		break;
	    case ALIAS:
		writeRef(sym.info());
		writeRef(sym.allConstructors());
		break;
	    case CLASS:
		writeRef(sym.info());
                if (sym.isModuleClass()) writeRef(sym.sourceModule());
		writeRef(sym.typeOfThis());
		writeRef(sym.allConstructors());
		break;
	    case VAL:
		writeRef(sym.info());
		if (sym.isConstructor() &&
		    sym == sym.constructorClass().allConstructors())
		    writeRef(sym.constructorClass());
		else if (sym.isModule())
		    writeRef(sym.moduleClass());
		break;
	    default:
		throw new ApplicationError();
	    }
	} else if (sym.kind == NONE) {
	    writeByte(NONEsym);
	    writeByte(0); // space for length
	} else {
	    if (sym.isModuleClass() || sym.isRoot()) {
		writeByte(EXTMODCLASSref);
		writeByte(0); // space for length
		writeRef(sym.name.toTermName());
	    } else {
		writeByte(EXTref);
		writeByte(0); // space for length
		assert !sym.isConstructor() : sym;
		writeRef(sym.name);
	    }
	    if (!sym.owner().isRoot())
		writeRef(sym.owner());
	}
	sym.flags &= ~ALTERNATIVE;
    }

    /** Write a type entry.
     */
    private void writeType(Type tp) {
	switch (tp) {
	case NoType:
	    writeByte(NOtpe);
	    writeByte(0); // space for length
	    break;
	case NoPrefix:
	    writeByte(THIStpe);
	    writeByte(0); // space for length
	    writeRef(Symbol.NONE);
            // !!! code above is usefull for compatibility
            // !!! following code would be better line:
	    // !!! writeByte(NOpre);
	    // !!! writeByte(0); // space for length
	    break;
	case ThisType(Symbol sym):
	    writeByte(THIStpe);
	    writeByte(0); // space for length
	    writeRef(sym);
	    break;

	case SingleType(Type pre, Symbol sym):
	    writeByte(SINGLEtpe);
	    writeByte(0); // space for length
	    writeRef(pre);
	    writeRef(sym);
	    break;

	case ConstantType(Type base, AConstant value):
	    writeByte(CONSTANTtpe);
	    writeByte(0); // space for length
	    writeRef(base);
	    writeRef(value);
	    break;

	case TypeRef(Type pre, Symbol sym, Type[] args):
	    writeByte(TYPEREFtpe);
	    writeByte(0); // space for length
	    writeRef(pre);
	    writeRef(sym);
	    writeRefs(args);
	    break;

	case CompoundType(Type[] parents, Scope members):
	    writeByte(COMPOUNDtpe);
	    writeByte(0); // space for length
            writeRef(tp.symbol());
	    writeRefs(parents);
	    break;

	case MethodType(Symbol[] vparams, Type result):
	    writeByte(METHODtpe);
	    writeByte(0); // space for length
	    writeRef(result);
	    for (int i = 0; i < vparams.length; i++) {
		Type ptype = vparams[i].type();
		int pflags = vparams[i].flags;
		if ((pflags & (REPEATED | DEF)) != 0)
		    writeRef(new FlagsAndType(encodeFlags(pflags), ptype));
		else
		    writeRef(ptype);
	    }
	    break;

	case PolyType(Symbol[] tparams, Type result):
	    writeByte(POLYtpe);
	    writeByte(0); // space for length
	    writeRef(result);
	    writeRefs(tparams);
	    break;

	case OverloadedType(Symbol[] alts, Type[] alttypes):
	    writeByte(OVERLOADEDtpe);
	    writeByte(0); // space for length
	    writeRefs(alts);
	    writeRefs(alttypes);
	    break;

	default:
	    throw new ApplicationError();
	}
    }

    private void writeFlagsAndType(FlagsAndType ft) {
	writeByte(FLAGGEDtpe);
	writeByte(0); // space for length
	writeNat(ft.flags);
	writeRef(ft.type);
    }

    /** Write a constant entry.
     */
    private void writeConstant(AConstant constant) {
        switch (constant) {
        case UNIT:
	    writeByte(LITERALunit);
	    writeByte(0); // space for length
            return;
        case BOOLEAN(boolean value):
	    writeByte(LITERALboolean);
	    writeByte(0); // space for length
	    writeByte(value ? 1 : 0);
            return;
        case BYTE(byte value):
	    writeByte(LITERALbyte);
	    writeByte(0); // space for length
	    writeLong(value);
            return;
        case SHORT(short value):
	    writeByte(LITERALshort);
	    writeByte(0); // space for length
	    writeLong(value);
            return;
        case CHAR(char value):
	    writeByte(LITERALchar);
	    writeByte(0); // space for length
	    writeLong(value);
            return;
        case INT(int value):
	    writeByte(LITERALint);
	    writeByte(0); // space for length
	    writeLong(value);
            return;
        case LONG(long value):
	    writeByte(LITERALlong);
	    writeByte(0); // space for length
	    writeLong(value);
            return;
        case FLOAT(float value):
	    writeByte(LITERALfloat);
	    writeByte(0); // space for length
	    writeLong(Float.floatToIntBits(value));
            return;
        case DOUBLE(double value):
	    writeByte(LITERALdouble);
	    writeByte(0); // space for length
	    writeLong(Double.doubleToLongBits(value));
            return;
        case STRING(String value):
	    writeByte(LITERALstring);
	    writeByte(0); // space for length
	    writeRef(Name.fromString(value));
            return;
        case NULL:
	    writeByte(LITERALnull);
	    writeByte(0); // space for length
            return;
        case ZERO:
	    writeByte(LITERALzero);
	    writeByte(0); // space for length
            return;
        default:
            throw Debug.abort("unknown case", constant);
        }
    }

    private void writeEntry(Object e) {
	int startpos = bp;
	if (e instanceof Symbol)
	    writeSymbol((Symbol) e);
	else if (e instanceof Type)
	    writeType((Type) e);
	else if (e instanceof Name)
	    writeName((Name) e);
	else if (e instanceof FlagsAndType)
	    writeFlagsAndType((FlagsAndType) e);
        else if (e instanceof AConstant)
            writeConstant((AConstant)e);
	else
	    throw new ApplicationError(e);
	patchNat(startpos + 1, bp - (startpos + 2));
    }

    private void writeAttr() {
	writeNat(ep);
	for (int i = 0; i < ep; i++) {
	    if (debug) System.out.print(i + "," + bp + ": ");
	    writeEntry(entries[i]);
	    if (debug) System.out.print("(" + entries[i] + ")");
	    if (debug) System.out.println();
	}
    }

    private static int encodeFlags(int flags) {
	int n = 0;
	if ((flags & REPEATED) != 0) n |= REPEATEDflag;
	if ((flags & DEF) != 0) n |= DEFflag;
	return n;
    }

    static class FlagsAndType {
	int flags;
	Type type;
	FlagsAndType(int flags, Type type) {
	    this.flags = flags;
	    this.type = type;
	}
	public boolean equals(Object other) {
	    if (other instanceof FlagsAndType) {
		FlagsAndType that = (FlagsAndType) other;
		return this.type.equals(that.type) &&
		    this.flags == that.flags;
	    } else {
		return false;
	    }
	}
	public int hashCode() {
	    return 37 + ((flags * 41) ^ type.hashCode());
	}
    }
}


/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.symtab.classfile;

import java.util.HashMap;
import scalac.*;
import scalac.util.*;
import ch.epfl.lamp.util.Position;
import scalac.symtab.*;
import Symbol.*;
import Type.*;

public class UnPickle implements Kinds, Modifiers, EntryTags {

/***************************************************
 * Symbol table attribute format:
 *   Symtab         = nentries_Nat {Entry}
 *   Entry          = TERMNAME len_Nat NameInfo
 *                  | CONSTRNAME len_Nat NameInfo
 *                  | TYPENAME len_Nat NameInfo
 *                  | NONEsym len_Nat
 *                  | TYPEsym len_Nat SymbolInfo lobound_Ref
 *                  | ALIASsym len_Nat SymbolInfo
 *                  | CLASSsym len_Nat SymbolInfo thistype_Ref constrsym_Ref
 *                  | MODULEsym len_Nat SymbolInfo classsym_Ref
 *                  | VALsym len_Nat SymbolInfo [classsym_Ref]
 *                  | EXTsym len_Nat name_Ref [owner_Ref]
 *                  | NOtpe len_Nat
 *                  | THIStpe len_Nat sym_Ref
 *                  | SINGLEtpe len_Nat type_Ref sym_Ref
 *                  | TYPEREFtpe len_Nat type_Ref sym_Ref {targ_Ref}
 *                  | COMPOUNDtpe len_Nat sym_Ref {tpe_Ref} {sym_Ref}
 *                  | METHODtpe len_Nat tpe_Ref {tpe_Ref}
 *                  | POLYTtpe len_Nat tpe_Ref {sym_Ref}
 *                  | OVERLOADEDtpe len_Nat {sym_Ref} {tpe_Ref}
 *                  | FLAGGEDtype len_Nat flags_Nat tpe_Ref
 *   SymbolInfo     = name_Ref owner_Ref flags_Nat info_Ref
 *   NameInfo       = <character sequence of length len_Nat in Utf8 format>
 *   Ref            = Nat
 *
 *   len is remaining length after `len'.
 */
    static final boolean debug = false;

    Symbol classroot;
    Symbol moduleroot;
    byte[] bytes;
    int bp;
    Name sourceName;
    int[] index;
    Object[] entries;
    int paramFlags;

    UnPickle(Symbol root, byte[] data, Name sourceName) {
	assert root.rawInfoAt(Symbol.FIRST_ID) instanceof LazyType;
	//System.out.println("unpickle " + root);//DEBUG
	this.classroot = root;
	this.moduleroot = classroot.module();
	this.bytes = data;
	this.bp = 0;
	this.sourceName = sourceName;
	index = new int[readNat()];
	for (int i = 0; i < index.length; i++) {
	    index[i] = bp;
	    bp++;
	    bp = readNat() + bp;
	}
	entries = new Object[index.length];
	if (debug) {
	    for (int i = 0; i < index.length; i++) {
		System.out.print(i + "," + index[i] + ": ");
		bp = index[i];
		System.out.print(readByte() + " ");
		int len = readNat();
		System.out.print(len + " ");
		for (int j = 0; j < len; j++)
		    System.out.print(readByte() + " ");
		System.out.println();
	    }
	}
	for (int i = 0; i < index.length; i++) {
	    if (isSymbolEntry(i)) getSymbol(i);
	}
	if (root.rawInfoAt(Symbol.FIRST_ID) instanceof LazyType)
	    throw new BadSignature(this, "it does not define " + root);
    }

    Type setOwner(Type tp, Symbol owner) {
	switch (tp) {
	case PolyType(Symbol[] tparams, Type restpe):
	    Type restpe1 = setOwner(restpe, owner);
	    if (restpe1 == restpe) return tp;
	    else return Type.PolyType(tparams, restpe1);
	case MethodType(Symbol[] params, Type restpe):
	    Symbol[] params1 = params;
	    for (int i = 0; i < params.length; i++) {
		if (params[i].owner() != owner) {
		    if (params1 == params && params[i].owner() != Symbol.NONE) {
			params1 = new Symbol[params.length];
			System.arraycopy(params, 0, params1, 0, i);
		    }
		    params1[i].setOwner(owner);
		}
	    }
	    Type restpe1 = setOwner(restpe, owner);
	    if (params1 == params && restpe1 == restpe) return tp;
	    else return Type.MethodType(params1, restpe1);
	default:
	    return tp;
	}
    }

    int readByte() {
	return bytes[bp++];
    }

    int readNat() {
	int b;
	int x = 0;
	do {
	    b = readByte();
	    x = (x << 7) + (b & 0x7f);
	} while ((b & 0x80) != 0);
	return x;
    }

    boolean isTypeEntry(int i) {
	int tag = bytes[index[i]];
	return (firstTypeTag <= tag && tag <= lastTypeTag);
    }

    boolean isSymbolEntry(int i) {
	int tag = bytes[index[i]];
	return (firstSymTag <= tag && tag <= lastSymTag);
    }

    Name getName(int n) {
	if (entries[n] == null) {
	    int savedBp = bp;
	    bp = index[n];
	    int tag = bytes[bp++];
	    int len = readNat();
	    Name name = Name.fromAscii(bytes, bp, len);
	    switch (tag) {
	    case TERMname  : entries[n] = name; break;
	    case CONSTRname: entries[n] = name.toConstrName(); break;
	    case TYPEname  : entries[n] = name.toTypeName(); break;
	    default: throw new BadSignature(this);
	    }
	    bp = savedBp;
	}
	return (Name) entries[n];
    }

    Name readNameRef() {
	return getName(readNat());
    }

    Symbol getSymbol(int n) {
	if (entries[n] == null) {
	    int savedBp = bp;
	    bp = index[n];
	    int tag = bytes[bp++];
	    int end = readNat() + bp;
	    Symbol sym;
	    Symbol owner;
	    switch (tag) {
	    case NONEsym:
		entries[n] = sym = Symbol.NONE;
		break;
	    case EXTsym:
		Name name = readNameRef();
		if (bp == end) {
		    owner = Global.instance.definitions.ROOT_CLASS;
		} else {
		    assert bp < end;
		    owner = readSymbolRef();
		}
		if (name.length() == 0 && owner == Symbol.NONE) {
		    entries[n] = sym = Global.instance.definitions.ROOT_CLASS;
		} else {
		    entries[n] = sym = owner.info().lookupNonPrivate(name);
		}
		if (sym.kind == NONE) {
		    throw new BadSignature(this,
			"reference " + name + " of " + owner +
			" refers to nonexisting symbol.");
		}
		break;
	    default:
		assert isSymbolEntry(n) : n;
		Name name = readNameRef();
		owner = readSymbolRef();
		if (entries[n] == null) {
		    int flags = readNat();
		    int inforef = readNat();
		    switch (tag) {
		    case TYPEsym:
			entries[n] = sym = new AbsTypeSymbol(
			    Position.NOPOS, name, owner, flags);
			sym.setInfo(getType(inforef));
			sym.setLoBound(readTypeRef());
			break;

		    case ALIASsym:
			entries[n] = sym = new TypeSymbol(
			    ALIAS, Position.NOPOS, name, owner, flags);
			sym.setInfo(getType(inforef));
			break;

		    case CLASSsym:
			entries[n] = sym = new ClassSymbol(
			    Position.NOPOS, name, owner, flags);
			if (name == classroot.name && owner == classroot.owner()) {
			    sym.copyTo(classroot);
			    entries[n] = sym = classroot;
			}
			sym.setInfo(getType(inforef));
			sym.setTypeOfThis(readTypeRef());
			Symbol constructor = readSymbolRef(); //will install itself!
			break;

		    case MODULEsym:
			entries[n] = sym = TermSymbol.newModule(
			    Position.NOPOS, name, owner, flags, (ClassSymbol) readSymbolRef());
			if (name == moduleroot.name && owner == moduleroot.owner()) {
			    sym.copyTo(moduleroot);
			    entries[n] = sym = moduleroot;
			}
			sym.setInfo(getType(inforef));
			break;

		    case VALsym:
			if (bp < end) {
			    assert name.isConstrName();
			    Symbol clazz = readSymbolRef();
			    entries[n] = sym = clazz.constructor();
			    TermSymbol.newConstructor(clazz, flags).copyTo(sym);
			} else {
			    entries[n] = sym = new TermSymbol(
				Position.NOPOS, name, owner, flags);
			}
			sym.setInfo(setOwner(getType(inforef), sym));
			break;

		    default:
			throw new BadSignature(this);
		    }

		    if (owner.kind == CLASS &&
			!(sym == classroot ||
			  sym.kind == CLASS && (sym.flags & MODUL) != 0 ||
			  (sym.isPrimaryConstructor() &&
			   (sym.primaryConstructorClass().flags & MODUL) != 0)))
			owner.members().enter(sym);
		}
	    }
	    bp = savedBp;
	}
	return (Symbol) entries[n];
    }

    Symbol readSymbolRef() {
	return getSymbol(readNat());
    }

    Symbol[] readSymbolRefs(int end) {
	return readSymbolRefs(0, end);
    }

    Symbol[] readSymbolRefs(int nread, int end) {
	if (bp == end) {
	    return new Symbol[nread];
	} else {
	    assert bp < end;
	    int ref = readNat();
	    if (isSymbolEntry(ref)) {
		Symbol s = getSymbol(ref);
		Symbol[] ss = readSymbolRefs(nread+1, end);
		ss[nread] = s;
		return ss;
	    } else {
		return new Symbol[nread];
	    }
	}
    }

    Type getType(int n) {
	if (entries[n] == null) {
	    int savedBp = bp;
	    bp = index[n];
	    int tag = bytes[bp++];
	    int end = readNat() + bp;
	    Type tpe;
	    switch (tag) {
	    case NOtpe:
		tpe = Type.NoType;
		break;
	    case THIStpe:
		tpe = Type.ThisType(readSymbolRef());
		break;
	    case SINGLEtpe:
		tpe = Type.singleType(readTypeRef(), readSymbolRef());
		break;
	    case TYPEREFtpe:
		tpe = Type.TypeRef(
		    readTypeRef(), readSymbolRef(), readTypeRefs(end));
		break;
	    case COMPOUNDtpe:
		Symbol clazz = readSymbolRef();
		Type[] parents = readTypeRefs(end);
		Scope members = new Scope(readSymbolRefs(end));
		tpe = Type.compoundType(parents, members, clazz);
		break;
	    case METHODtpe:
		Type restype = readTypeRef();
		int bp1 = bp;
		Type[] argtypes = readTypeRefs(end);
		int[] flags = new int[argtypes.length];
		bp = bp1;
		readFlags(flags);
		Symbol[] params = new Symbol[argtypes.length];
		for (int i = 0; i < argtypes.length; i++) {
		    params[i] = new TermSymbol(
			Position.NOPOS, Name.fromString("$" + i),
			Symbol.NONE, PARAM | flags[i]);
		    params[i].setInfo(argtypes[i]);
		}
		tpe = Type.MethodType(params, restype);
		break;
	    case POLYtpe:
		Type restype = readTypeRef();
		tpe = Type.PolyType(readSymbolRefs(end), restype);
		break;
	    case OVERLOADEDtpe:
		Symbol[] alts = readSymbolRefs(end);
		Type[] alttypes = readTypeRefs(end);
		for (int i = 0; i < alts.length; i++)
		    alttypes[i] = setOwner(alttypes[i], alts[i]);
		tpe = Type.OverloadedType(alts, alttypes);
		break;
	    case FLAGGEDtpe:
		paramFlags = getFlags(readNat());
		return readTypeRef();
	    default:
		throw new BadSignature(this);
	    }
	    entries[n] = tpe;
	    bp = savedBp;
	}
	return (Type) entries[n];
    }

    Type readTypeRef() {
	return getType(readNat());
    }

    Type[] readTypeRefs(int end) {
	return readTypeRefs(0, end);
    }

    Type[] readTypeRefs(int nread, int end) {
	if (bp == end) {
	    return new Type[nread];
	} else {
	    assert bp < end;
	    int ref = readNat();
	    if (isTypeEntry(ref)) {
		Type t = getType(ref);
		Type[] ts = readTypeRefs(nread + 1, end);
		ts[nread] = t;
		return ts;
	    } else {
		return new Type[nread];
	    }
	}
    }

    void readFlags(int[] flags) {
	for (int i = 0; i < flags.length; i++)
	    flags[i] = getFlags(readNat());
    }

    int getFlags(int n) {
	int savedBp = bp;
	bp = index[n];
	int tag = bytes[bp++];
	int end = readNat() + bp;
	int flags = (tag == FLAGGEDtpe) ? decodeFlags(readNat()) : 0;
	bp = savedBp;
	return flags;
    }

    private static int decodeFlags(int n) {
	int flags = 0;
	if ((n & COVARflag) != 0) flags |= COVARIANT;
	if ((n & CONTRAVARflag) != 0) flags |= CONTRAVARIANT;
	if ((n & REPEATEDflag) != 0) flags |= REPEATED;
	if ((n & DEFflag) != 0) flags |= DEF;
	return flags;
    }

    public static class BadSignature extends java.lang.Error {
	public BadSignature(UnPickle outer, String msg) {
	    super("symbol data " + outer.sourceName +
		  " could not be loaded; " + msg);
	}
	public BadSignature(UnPickle outer) {
	    this(outer, "malformed signature at " + outer.bp);
	}
    }
}


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
 * Symbol table attribute format: see EntryTags.java
 */
    static final boolean debug = true;

    Symbol classroot;
    Symbol moduleroot;
    byte[] bytes;
    int bp;
    Name sourceName;
    int[] index;
    Object[] entries;
    int paramFlags;
    Global global;

    UnPickle(Symbol root, byte[] data, Name sourceName) {
	global = Global.instance;
	assert root.rawInfoAt(Symbol.FIRST_ID) instanceof LazyType;
	if (root.isConstructor()) {
	    this.classroot = root.primaryConstructorClass();
	    this.moduleroot = classroot.module();
	} else if (root.isType()) {
	    this.classroot = root;
	    this.moduleroot = classroot.module();
	} else {
	    this.moduleroot = root;
	    this.classroot = root.owner().lookup(root.name.toTypeName());
	}
	if (global.debug)
	    global.log(
		"unpickle " + root + " " + classroot + " " + moduleroot + " " +
		moduleroot.moduleClass() + moduleroot.moduleClass().constructor());
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
	if (global.debug) {
	    global.log("length: " + index.length);
	    for (int i = 0; i < index.length; i++) {
		System.out.print(i + "," + index[i] + ": ");
		bp = index[i];
		int tag = readByte();
		System.out.print(tag + " ");
		int len = readNat();
		System.out.print(len + " ");
		if (tag == TERMname || tag == TYPEname || tag == CONSTRname)
		    System.out.print(
			SourceRepresentation.ascii2string(bytes, bp, len));
		else
		    for (int j = 0; j < len; j++)
			System.out.print(readByte() + " ");
		System.out.println();
	    }
	}
	for (int i = 0; i < index.length; i++) {
	    if (isSymbolEntry(i)) getSymbol(i);
	}
	if (global.debug) global.log("unpickled " + root);//debug
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
	    if (params.length > 0 &&
		params[0].owner() != owner && params[0].owner() != Symbol.NONE) {
		params1 = new Symbol[params.length];
		for (int i = 0; i < params.length; i++)
		    params1[i] = params[i].cloneSymbol().setOwner(owner);
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

    String decode(Name name) {
	if (name.isTypeName()) return "type " + NameTransformer.decode(name);
	else if (name.isConstrName()) return "constructor " + NameTransformer.decode(name);
	else return "value " + NameTransformer.decode(name);
    }

    Symbol extRef(Symbol owner, Name name, boolean modclass) {
	if (name.length() == 0 && owner == Symbol.NONE) {
	    return Global.instance.definitions.ROOT_CLASS;
	} else if (modclass) {
	    assert name.isTypeName() : name;
	    Symbol module = extRef(owner, name.toTermName(), false);
	    switch (module.type()) {
	    case OverloadedType(Symbol[] alts, _):
		for (int i = 0; i < alts.length; i++)
		    if (alts[i].isModule()) module = alts[i];
	    }
	    assert module.isModule();
	    return module.moduleClass();
	} else {
	    return owner.info().lookup(name);
	}
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
	    case EXTMODCLASSsym:
		Name name = readNameRef();
		if (bp == end) {
		    owner = Global.instance.definitions.ROOT_CLASS;
		} else {
		    assert bp < end;
		    owner = readSymbolRef();
		}
		entries[n] = sym = extRef(owner, name, tag == EXTMODCLASSsym);
		if (sym.kind == NONE) {
		    if (global.debug)
			global.log(owner.info().members().toString());//debug
		    throw new BadSignature(this,
			"reference " + decode(name) + " of " + owner +
			" refers to nonexisting symbol.");
		}
		break;
	    default:
		assert isSymbolEntry(n) : n;
		Name name = readNameRef();
		if (global.debug)
		    global.log("reading " + name + " at " + n);//debug
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
			Symbol clr = ((flags & MODUL) == 0) ? classroot
			    : moduleroot.moduleClass();
			if (name == clr.name && owner == clr.owner()) {
			    if (global.debug) global.log("overwriting " + clr);
			    sym.copyTo(clr);
			    entries[n] = sym = clr;
			}
			sym.setInfo(getType(inforef));
			sym.setTypeOfThis(readTypeRef());
			Symbol constructor = readSymbolRef(); //will install itself!
			break;

		    case VALsym:
			Symbol cf = Symbol.NONE;
			if (bp < end) {
			    ClassSymbol clazz = (ClassSymbol) readSymbolRef();
			    if (name.isConstrName()) {
				entries[n] = sym = clazz.constructor();
				if (global.debug)
				    global.log("overwriting " + sym);//debug
				TermSymbol.newConstructor(clazz, flags).copyTo(sym);
				if (clazz.isCaseClass()) {
				    cf = new TermSymbol(
					Position.NOPOS, name.toTermName(), owner, flags | CASE);
				    if (name.toTypeName() == classroot.name && owner == moduleroot.owner()) {
					if (global.debug)
					    global.log("overwriting " + moduleroot);//debug
					cf.copyTo(moduleroot);
					cf = moduleroot;
				    } else {
					owner.info().members().enterOrOverload(cf);
				    }
				}
			    } else {
				assert (flags & MODUL) != 0 : name;
				entries[n] = sym = TermSymbol.newModule(
				    Position.NOPOS, name, owner, flags, clazz);
				if (name == moduleroot.name && owner == moduleroot.owner()) {
				    if (global.debug)
					global.log("overwriting " + moduleroot);//debug
				    sym.copyTo(moduleroot);
				    entries[n] = sym = moduleroot;
				}
			    }
			} else {
			    entries[n] = sym = new TermSymbol(
				Position.NOPOS, name, owner, flags);
			}
			Type tp = getType(inforef);
			sym.setInfo(setOwner(tp, sym));
			if (cf.kind != NONE) cf.setInfo(setOwner(tp, cf));
			break;

		    default:
			throw new BadSignature(this);
		    }

		    if (owner.kind == CLASS && !noEnter(sym)) {
			if (global.debug)
			    global.log("entering " + sym + ":" + sym.type() + " in " + owner);//debug
			owner.info().members().enterOrOverload(sym);
		    }
		}
	    }
	    bp = savedBp;
	}
	return (Symbol) entries[n];
    }
    //where
    private boolean noEnter(Symbol sym) {
	return
	    sym == classroot ||
	    sym == moduleroot ||
	    sym.isModuleClass() ||
	    sym.isPrimaryConstructor() && noEnter(sym.primaryConstructorClass());
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
	    int bp0 = bp;
	    int ref = readNat();
	    if (isSymbolEntry(ref)) {
		Symbol s = getSymbol(ref);
		Symbol[] ss = readSymbolRefs(nread+1, end);
		ss[nread] = s;
		return ss;
	    } else {
		bp = bp0;
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
		Symbol sym = readSymbolRef();
		tpe = (sym.kind == NONE) ? Type.localThisType
		    : Type.ThisType(sym);
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
		tpe = Type.compoundType(parents, new Scope(), clazz);
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
		int bp0 = bp;
		Symbol[] alts = readSymbolRefs(end);
		int bp1 = bp;
		Type[] alttypes = readTypeRefs(end);
		assert alts.length == alttypes.length
		    : alts.length + "!=" + alttypes.length +
		    " at " + bp0 + "/" + bp1 + "/" + bp;
		for (int i = 0; i < alts.length; i++)
		    alttypes[i] = setOwner(alttypes[i], alts[i]);
		tpe = Type.OverloadedType(alts, alttypes);
		break;
	    case FLAGGEDtpe:
		readNat(); // skip flags
		tpe = readTypeRef();
		break;
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
	    assert bp < end : bp + ">" + end;
	    int bp0 = bp;
	    int ref = readNat();
	    if (isTypeEntry(ref)) {
		Type t = getType(ref);
		Type[] ts = readTypeRefs(nread + 1, end);
		ts[nread] = t;
		return ts;
	    } else {
		bp = bp0;
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


/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.symtab.classfile;

import java.util.HashMap;
import java.io.IOException;
import java.io.PrintStream;
import scala.tools.util.AbstractFile;
import scala.tools.util.Position;
import scalac.*;
import scalac.atree.AConstant;
import scalac.util.*;
import scalac.symtab.*;
import Symbol.*;
import Type.*;

public class UnPickle implements Kinds, Modifiers, EntryTags, TypeTags {

/***************************************************
 * Symbol table attribute format: see EntryTags.java
 */
    static final boolean debug = true;

    public static void parse(Global global, AbstractFile file, Symbol root)
        throws IOException
    {
        try {
            parse(global, file.read(), root);
        } catch (BadSignature exception) {
            throw new IOException("symbol file '" + file.getPath() + "' "
                + "could not be loaded; " + exception.getMessage());
        }
    }

    /**
     * The root symbol must be either a module or a non-module
     * class. The unpickler initializes it. If it has a linked module
     * or class, it will also be initialized.
     */
    public static void parse(Global global, byte[] data, Symbol root)
        throws BadSignature
    {
            new UnPickle(global, data, root);
    }

    Symbol classroot;
    Symbol moduleroot;
    byte[] bytes;
    int bp;
    int[] index;
    Object[] entries;
    int paramFlags;
    final Global global;

    private UnPickle(Global global, byte[] data, Symbol root) {
	this.global = global;
        this.classroot = root.isModule() ? root.linkedClass() : root;
        this.moduleroot = root.isClassType() ? root.linkedModule() : root;
        assert classroot == null || classroot.isClassType(): Debug.show(root);
        assert moduleroot == null || moduleroot.isModule(): Debug.show(root);
	if (root != moduleroot && moduleroot != null) {
	    moduleroot.moduleClass().setInfo(Type.NoType);
	}
	if (global.debug) global.log(
            "unpickle " + root + " " + classroot + " " + moduleroot
            + (moduleroot != null ? " " + moduleroot.moduleClass() : ""));
	this.bytes = data;
	this.bp = 0;
	index = new int[readNat()];
	for (int i = 0; i < index.length; i++) {
	    index[i] = bp;
	    bp++;
	    bp = readNat() + bp;
	}
	entries = new Object[index.length];

	if (global.debug) print(System.out);

	for (int i = 0; i < index.length; i++) {
	    if (isSymbolEntry(i)) getSymbol(i);
	}
	if (global.debug) global.log("unpickled " + root + ":" + root.rawInfo());//debug
	if (!classroot.isInitialized() && !moduleroot.isInitialized())
	    throw new BadSignature(this, "it does not define " + root);
        // the isModule test is needed because moduleroot may become
        // the case class factory method of classroot
	if (moduleroot != null && moduleroot.isModule() && moduleroot.moduleClass().type() == Type.NoType) {
	    moduleroot.setInfo(Type.NoType);
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

    long readLong(int n) {
	long x = 0;
	for (int i = 0; i < n; i++) {
	    x = (x << 8) + (readByte() & 0xff);
	}
	int leading = 64 - (n * 8);
	return x << leading >> leading;
    }

    boolean isTypeEntry(int i) {
	int tag = bytes[index[i]];
	return (firstTypeTag <= tag && tag <= lastTypeTag) || tag == NOpre;
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
	else return "value " + NameTransformer.decode(name);
    }

    /*
    void enterSymbol(Symbol sym) {
	Symbol owner = sym.owner();
	if (owner.kind == CLASS &&
	    !sym.isConstructor() && !sym.isModuleClass()) {
	    Scope scope = owner.info().members();
	    scope.enterOrOverload(sym);
	}
    }
    */

    void enterSymbol(Symbol sym) {
	/*
	if (global.debug) {
	    global.log("entering " + sym + ":" + sym.type() + " in " + sym.owner());//debug
	    if (sym.kind == CLASS)
		global.log("primconstr = " + sym.primaryConstructor());
	}
	*/
	if ((sym.flags & ALTERNATIVE) != 0) {
	    sym.flags &= ~ALTERNATIVE;
	} else {
	    Symbol owner = sym.owner();
	    if (owner.kind == CLASS &&
		!sym.isConstructor() && !sym.isModuleClass()) {
		Scope scope = owner.info().members();
		Symbol other = scope.lookup(sym.name);
		if (other == Symbol.NONE) {
		    scope.enter(sym);
		} else {
		    assert sym == other
			: "double enter: " + other + ":" + other.rawFirstInfo() + "," + sym + ":" + sym.rawFirstInfo();
		}
	    }
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
	    case EXTref:
	    case EXTMODCLASSref:
		Name name = readNameRef();
		if (bp == end) {
		    owner = global.definitions.ROOT_CLASS;
		} else {
		    assert bp < end;
		    owner = readSymbolRef();
		}
		if (name == Names.ROOT && owner == Symbol.NONE) {
		    sym = global.definitions.ROOT_CLASS;
                    if (tag == EXTref) sym = sym;
                    // !!! line above is usefull for the transition
                    // !!! after some time, replace it by the following line:
                    // !!! assert tag != EXTref;
		} else {
		    sym = owner.info().lookup(name);
		    if (tag == EXTMODCLASSref) {
			/*
			if (sym.kind == VAL)
			    switch (sym.type()) {
			    case OverloadedType(Symbol[] alts, _):
				for (int i = 0; i < alts.length; i++)
				    if (alts[i].isModule()) sym = alts[i];
			    }
			*/
			assert sym.isModule();
			sym = sym.moduleClass();
		    }
		}
		entries[n] = sym;
		if (sym.kind == NONE) {
		    if (global.debug)
			global.log(owner.info().members().toString());
		    throw new BadSignature(this,
			"reference " + decode(name) + " of " + owner +
			" refers to nonexisting symbol.");
		}
		break;
	    default:
		assert isSymbolEntry(n) : n;
		Name name = readNameRef();
		if (global.debug)
		    global.log("reading " + name + " at " + n);
		owner = readSymbolRef();
		if (entries[n] == null) {
		    int flags = readNat();
		    int inforef = readNat();
		    switch (tag) {
		    case TYPEsym:
			entries[n] = sym = owner.newAbstractType(
			    Position.NOPOS, flags, name);
			if ((flags & VIEWBOUND) != 0) {
			    sym.setInfo(global.definitions.ANY_TYPE());
			    sym.setVuBound(getType(inforef, sym));
			} else {
			    sym.setInfo(getType(inforef, sym));
			}
			sym.setLoBound(readTypeRef(sym));
			break;

		    case ALIASsym:
			entries[n] = sym = owner.newTypeAlias(
			    Position.NOPOS, flags, name);
			sym.setInfo(getType(inforef, sym));
			Symbol constr = readSymbolRef();
			break;

		    case CLASSsym:
                        if ((flags & MODUL) != 0) {
                            Symbol modulesym = readSymbolRef();
                            entries[n] = sym = modulesym.moduleClass();
                            sym.flags = flags;
                        } else if (classroot != null && name == classroot.name && owner == classroot.owner()) {
			    if (global.debug)
                                global.log("overwriting " + classroot);
			    entries[n] = sym = classroot;
			    sym.flags = flags;
                        } else {
                            entries[n] = sym = owner.newClass(
                                Position.NOPOS, flags, name);
                        }
			sym.setInfo(getType(inforef, sym));
			sym.setTypeOfThis(readTypeRef(sym));
			Symbol constr = readSymbolRef();
			assert constr == sym.allConstructors();
			break;

		    case VALsym:
			if (moduleroot != null && name == moduleroot.name && owner == moduleroot.owner()) {
			    if (global.debug)
				global.log("overwriting " + moduleroot);
			    entries[n] = sym = moduleroot;
                            sym.flags = flags;
                        } else if ((flags & MODUL) != 0) {
                            entries[n] = sym = owner.newModule(
                                Position.NOPOS, flags, name);
                        } else if (name == Names.CONSTRUCTOR) {
                            Symbol tsym = bp < end ? readSymbolRef() : null;
                            if (tsym == null) {
                                entries[n] = sym = owner.newConstructor(
                                    Position.NOPOS, flags);
                            } else {
                                entries[n] = sym = tsym.allConstructors();
                                sym.flags = flags;
                            }
                        } else {
                            entries[n] = sym = owner.newTerm(
                                Position.NOPOS, flags, name);
                        }
                        if (sym.isModule()) {
                            Symbol clasz = readSymbolRef();
                            assert clasz == sym.moduleClass(): Debug.show(sym);
                        }
			Type owntype = getType(inforef, sym);
			sym.setInfo(owntype);
			break;

		    default:
			throw new BadSignature(this);
		    }

		    enterSymbol(sym);
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

    Type getType(int n, Symbol owner) {
        Type tpe = (Type)entries[n];
	if (tpe == null) {
	    int savedBp = bp;
	    bp = index[n];
	    int tag = bytes[bp++];
	    int end = readNat() + bp;
	    switch (tag) {
	    case NOtpe:
		tpe = Type.NoType;
		break;
	    case NOpre:
		tpe = Type.NoPrefix;
		break;
	    case THIStpe:
		Symbol sym = readSymbolRef();
		tpe = (sym.kind == NONE) ? Type.NoPrefix : Type.ThisType(sym);
                // !!! code above is usefull for the transition
                // !!! after some time, replace it by the following line:
		// !!! tpe = Type.ThisType(readSymbolRef());
		break;
	    case SINGLEtpe:
                Type prefix = readTypeRef(owner);
                Symbol symbol = readSymbolRef();
		tpe = symbol.isRoot() ? symbol.thisType() : Type.singleType(prefix, symbol);
                // !!! code above is usefull for the transition
                // !!! after some time, replace it by the following line:
		// !!! tpe = Type.singleType(readTypeRef(), readSymbolRef());
		break;
	    case CONSTANTtpe:
		Type base = readTypeRef(owner);
		AConstant value = readConstantRef();
		tpe = new Type.ConstantType(base, value);
		break;
	    case TYPEREFtpe:
		tpe = Type.newTypeRefUnsafe( // !!!
		    readTypeRef(owner), readSymbolRef(), readTypeRefs(end, owner));
		break;
	    case COMPOUNDtpe:
		Symbol clazz = readSymbolRef();
		Type[] parents = readTypeRefs(end, owner);
                tpe = Type.compoundType(parents, new Scope(), clazz);
		break;
	    case METHODtpe:
		Type restype = readTypeRef(owner);
		int bp1 = bp;
		Type[] argtypes = readTypeRefs(end, owner);
		int[] flags = new int[argtypes.length];
		bp = bp1;
		readFlags(flags);
		Symbol[] params = new Symbol[argtypes.length];
		for (int i = 0; i < argtypes.length; i++) {
                    Name name = Name.fromString("$" + i);
		    params[i] = owner.newVParam(
                        Position.NOPOS, flags[i], name, argtypes[i]);
		}
		tpe = Type.MethodType(params, restype);
		break;
	    case POLYtpe:
		Type restype = readTypeRef(owner);
		tpe = Type.PolyType(readSymbolRefs(end), restype);
		break;
	    case OVERLOADEDtpe:
		int bp0 = bp;
		Symbol[] alts = readSymbolRefs(end);
		int bp1 = bp;
		Type[] alttypes = readTypeRefs(end, alts);
		assert alts.length == alttypes.length
		    : alts.length + "!=" + alttypes.length +
		    " at " + bp0 + "/" + bp1 + "/" + bp;
		tpe = Type.OverloadedType(alts, alttypes);
		break;
	    case FLAGGEDtpe:
		readNat(); // skip flags
		tpe = readTypeRef(owner);
		break;
	    default:
		throw new BadSignature(this);
	    }
	    if (tag != METHODtpe) entries[n] = tpe;
	    bp = savedBp;
	}
	return tpe;
    }

    Type readTypeRef(Symbol owner) {
	return getType(readNat(), owner);
    }

    Type[] readTypeRefs(int end, Symbol owner) {
	return readTypeRefs(0, end, owner, null);
    }

    Type[] readTypeRefs(int end, Symbol[] owners) {
	return readTypeRefs(0, end, null, owners);
    }

    Type[] readTypeRefs(int nread, int end, Symbol owner, Symbol[] owners) {
        assert (owner != null) ^ (owners != null);
	if (bp == end) {
	    return new Type[nread];
	} else {
	    assert bp < end : bp + ">" + end;
	    int bp0 = bp;
	    int ref = readNat();
	    if (isTypeEntry(ref)) {
		Type t = getType(ref, owner != null ? owner : owners[nread]);
		Type[] ts = readTypeRefs(nread + 1, end, owner, owners);
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
	if ((n & REPEATEDflag) != 0) flags |= REPEATED;
	if ((n & DEFflag) != 0) flags |= DEF;
	return flags;
    }

    AConstant readConstant() {
	int tag = bytes[bp++];
        int len = readNat();
        switch (tag) {
        case LITERALunit:
            return AConstant.UNIT;
        case LITERALboolean:
            return AConstant.BOOLEAN(readByte() == 0 ? false : true);
        case LITERALbyte:
            return AConstant.BYTE((byte)readLong(len));
        case LITERALshort:
            return AConstant.SHORT((short)readLong(len));
        case LITERALchar:
            return AConstant.CHAR((char)readLong(len));
        case LITERALint:
            return AConstant.INT((int)readLong(len));
        case LITERALlong:
            return AConstant.LONG(readLong(len));
        case LITERALfloat:
            return AConstant.FLOAT(Float.intBitsToFloat((int)readLong(len)));
        case LITERALdouble:
            return AConstant.DOUBLE(Double.longBitsToDouble(readLong(len)));
        case LITERALstring:
            return AConstant.STRING(readNameRef().toString());
        case LITERALnull:
            return AConstant.NULL;
        case LITERALzero:
            return AConstant.ZERO;
        default:
            throw Debug.abort("illegal tag: " + tag);
        }
    }

    AConstant readConstantRef() {
        int n = readNat();
	int savedBp = bp;
	bp = index[n];
        AConstant constant = readConstant();
        bp = savedBp;
        return constant;
    }

    public static class BadSignature extends java.lang.Error {
	public BadSignature(UnPickle outer, String msg) {
	    super(msg);
	}
	public BadSignature(UnPickle outer) {
	    this(outer, "malformed signature at " + outer.bp);
	}
    }

// --- print symbl files -------------------------------------------------

    private static String tag2string(int tag) {
	switch (tag) {
	case TERMname: return "TERMname";
	case TYPEname: return "TYPEname";
	case NONEsym: return "NONEsym";
	case TYPEsym: return "TYPEsym";
	case ALIASsym: return "ALIASsym";
	case CLASSsym: return "CLASSsym";
	case VALsym: return "VALsym";
	case EXTref: return "EXTref";
	case EXTMODCLASSref: return "EXTMODCLASSref";
	case NOtpe: return "NOtpe";
	case THIStpe: return "THIStpe";
	case SINGLEtpe: return "SINGLEtpe";
	case TYPEREFtpe: return "TYPEREFtpe";
	case CONSTANTtpe: return "CONSTANTtpe";
	case COMPOUNDtpe: return "COMPOUNDtpe";
	case METHODtpe: return "METHODtpe";
	case POLYtpe: return "POLYtpe";
	case OVERLOADEDtpe: return "OVERLOADEDtpe";
	case UNBOXEDtpe: return "UNBOXEDtpe";
        case UNBOXEDARRAYtpe: return "UNBOXEDARRAYtpe";
	case FLAGGEDtpe: return "FLAGGEDtpe";
	case ERRORtpe: return "ERRORtpe";
        case LITERALunit: return "LITERALunit";
        case LITERALboolean: return "LITERALboolean";
        case LITERALbyte: return "LITERALbyte";
        case LITERALshort: return "LITERALshort";
        case LITERALchar: return "LITERALchar";
        case LITERALint: return "LITERALint";
        case LITERALlong: return "LITERALlong";
        case LITERALfloat: return "LITERALfloat";
        case LITERALdouble: return "LITERALdouble";
        case LITERALstring: return "LITERALstring";
        case LITERALnull: return "LITERALnull";
        case LITERALzero: return "LITERALzero";
	default: return "***BAD TAG***(" + tag + ")";
	}
    }

    private void print(PrintStream out) {
	out.println("symbl attribute for " + classroot + ":");
	for (int i = 0; i < index.length; i++) {
	    out.print(i + "," + index[i] + ": ");
	    bp = index[i];
	    int tag = readByte();
	    out.print(tag2string(tag));
	    int len = readNat();
	    int end = len + bp;
	    out.print(" " + len);
	    switch (tag) {
	    case TERMname:
	    case TYPEname:
		String name = SourceRepresentation.ascii2string(bytes, bp, len);
		out.print(" " + SourceRepresentation.escape(name));
		bp = end;
		break;
	    case NONEsym:
		break;
	    case TYPEsym:
	    case ALIASsym:
	    case CLASSsym:
	    case VALsym:
		out.print(" " + readNat()); //name
		out.print(" " + readNat()); //owner
		out.print(" " + Integer.toHexString(readNat())); //flags
		out.print(" " + readNat()); //type
		break;
	    case FLAGGEDtpe:
		out.print(" " + Integer.toHexString(readNat())); //flags
	    }
	    while (bp < end) out.print(" " + readNat());
	    out.println();
	}
    }
}


/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.symtab.classfile;

import ch.epfl.lamp.util.Position;
import scalac.*;
import scalac.atree.AConstant;
import scalac.symtab.*;
import scalac.util.*;
import java.util.*;

public class AttributeParser implements ClassfileConstants {

    /** the classfile input buffer
     */
    protected AbstractFileReader in;

    /** the constant pool
     */
    protected ConstantPool pool;

    protected ClassfileParser parser;

    /** constructor
     */
    public AttributeParser(AbstractFileReader in, ConstantPool pool, ClassfileParser parser) {
        this.in = in;
        this.pool = pool;
        this.parser = parser;
    }

	/** convert an attribute name into an attribute id
	 */
    public int nameToId(Name name) {
        if (name == SOURCEFILE_N)
            return SOURCEFILE_ATTR;
        if (name == SYNTHETIC_N)
            return SYNTHETIC_ATTR;
        if (name == DEPRECATED_N)
            return DEPRECATED_ATTR;
        if (name == CODE_N)
            return CODE_ATTR;
        if (name == EXCEPTIONS_N)
            return EXCEPTIONS_ATTR;
        if (name == CONSTANT_VALUE_N)
            return CONSTANT_VALUE_ATTR;
        if (name == LINE_NUM_TABLE_N)
            return LINE_NUM_TABLE_ATTR;
        if (name == LOCAL_VAR_TABLE_N)
            return LOCAL_VAR_TABLE_ATTR;
        if (name == INNERCLASSES_N)
            return INNERCLASSES_ATTR;
        if (name == META_N)
            return META_ATTR;
        if (name == SCALA_N)
            return SCALA_ATTR;
        if (name == JACO_N)
            return JACO_ATTR;
        if (name == BRIDGE_N)
            return BRIDGE_ATTR;
        if (name == SIG_N)
            return SIG_ATTR;
        return BAD_ATTR;
    }

    /** skip all attributes.
     */
    public void skipAttributes() {
        char    nattr = in.nextChar();
        for (int i = 0; i < nattr; i++) {
            in.skip(2);
            in.skip(in.nextInt());
        }
    }

    /** read all attributes associated with symbol 'sym' which are
     *  contained in 'attrs'.
     */
    public Symbol readAttributes(Symbol def, Type type, int attrs) {
        char    nattr = in.nextChar();
        for (int i = 0; i < nattr; i++) {
            Name attrName = pool.getName(in.nextChar());
            int attr = nameToId(attrName);
            int attrLen = in.nextInt();
            if ((attrs & attr) == 0) {
                //System.out.println("# skipping " + attrName + " of " + def);
                in.skip(attrLen);
            } else {
                //System.out.println("# reading " + attrName + " of " + def);
                readAttribute(def, type, attr, attrLen);
            }
        }
        return def;
    }

    /** read a single attribute 'attr' for symbol 'sym' with type 'type'.
     */
    public void readAttribute(Symbol sym, Type type, int attr, int attrLen) {
        switch (attr) {
        // class attributes
            case SCALA_ATTR:
                new UnPickle(sym, in.nextBytes(attrLen), Name.fromString(in.path));
                return;
            case INNERCLASSES_ATTR:
                int n = in.nextChar();
                //System.out.println(sym + " has " + n + " innerclass entries");
                for (int i = 0; i < n; i++) {
                    int inner = in.nextChar();
                    if (inner == 0) { in.skip(6); return; }
                    int outer = in.nextChar();
                    if (outer == 0) { in.skip(4); return; }
                    int name = in.nextChar();
                    if (name == 0) { in.skip(2); return; }
                    int flags = in.nextChar();
                    if ((flags & JAVA_ACC_STATIC) == 0) return;
                    if ((flags & (JAVA_ACC_PUBLIC | JAVA_ACC_PROTECTED)) == 0) return;
                    if (pool.getClass(outer) != sym) return;
                    Symbol alias = sym.dualClass().newTypeAlias(
                        Position.NOPOS,
                        0,
                        pool.getName(name).toTypeName(),
                        parser.make.classType(pool.getClass(inner)));
                    parser.statics.enterNoHide(alias);
                }
                //in.skip(attrLen);
                return;
        // method attributes
            case CODE_ATTR:
                in.skip(attrLen);
                return;
            case EXCEPTIONS_ATTR:
                //int nexceptions = in.nextChar();
                //Type[] thrown = new Type[nexceptions];
                //for (int j = 0; j < nexceptions; j++)
                //    thrown[j] = make.classType(reader.readClassName(in.nextChar()));
                //((MethodType)def.type).thrown = thrown;
                in.skip(attrLen);
                return;
            case LINE_NUM_TABLE_ATTR:
                in.skip(attrLen);
                return;
            case LOCAL_VAR_TABLE_ATTR:
                in.skip(attrLen);
                return;
        // general attributes
            case SYNTHETIC_ATTR:
                sym.flags |= Modifiers.SYNTHETIC;
                return;
            case BRIDGE_ATTR:
                sym.flags |= Modifiers.BRIDGE;
                return;
            case DEPRECATED_ATTR:
                sym.flags |= Modifiers.DEPRECATED;
                return;
            case CONSTANT_VALUE_ATTR:
            	AConstant constant = pool.getConstantValue(in.nextChar());
                switch (constant) {
                case INT(int value):
                    Definitions definitions = parser.global.definitions;
                    Symbol base = sym.getType().symbol();
                    if (base == definitions.INT_CLASS) break;
                    if (base == definitions.CHAR_CLASS)
                        constant = AConstant.CHAR((char)value);
                    else if (base == definitions.SHORT_CLASS)
                        constant = AConstant.SHORT((short)value);
                    else if (base == definitions.BYTE_CLASS)
                        constant = AConstant.BYTE((byte)value);
                    else
                        constant = AConstant.BOOLEAN(value != 0);
                }
            	sym.setInfo(parser.make.constantType(constant));
                return;
            case META_ATTR:
                //System.out.println("parsing meta data for " + sym);
                String meta = pool.getString(in.nextChar()).trim();
                sym.setInfo(
                    new MetaParser(meta, tvars, sym, type).parse());
                return;
       		case JACO_ATTR:
       		    // this attribute is present in all PiCo generated classfiles
       		    int mods = in.nextChar();
     			mods |= (in.nextChar() << 16);
     			boolean algebraicClass = (mods & 0x00100000) != 0;
     			boolean caseClass = (mods & 0x00200000) != 0;
     			if (caseClass)
     			    sym.flags |= Modifiers.CASE | Modifiers.JAVA;
				in.skip(attrLen - 4);
                return;
            default:
                in.skip(attrLen);
                return;
        }
    }


    Scope tvars = new Scope();

	/** a parser class for parsing meta type information in classfiles
	 *  generated by pico.
	 */
    class MetaParser {
        Symbol owner;
        StringTokenizer scanner;
        Type defaultType;
        String token;
        Scope tvars;
        Scope locals;

        MetaParser(String meta, Scope tvars, Symbol owner, Type defaultType) {
            //System.out.println("meta = " + meta);
            this.scanner = new StringTokenizer(meta, "()[], \t<;", true);
            this.defaultType = defaultType;
            this.owner = owner;
            this.tvars = tvars;
        }

        private Symbol getTVar(String name) {
            return getTVar(name, parser.c.primaryConstructor());
        }

        private Symbol getTVar(String name, Symbol owner) {
            if (name.startsWith("?")) {
                Symbol s = ((locals != null) ? locals : tvars)
                        .lookup(Name.fromString(name).toTypeName());
                if (s != Symbol.NONE)
                        return s;
                else if (locals != null) {
                                        s = tvars.lookup(Name.fromString(name).toTypeName());
                                        if (s != Symbol.NONE)
                                return s;
                }
                s = owner.newTParam(
                        Position.NOPOS,
                        0,
                        Name.fromString(token).toTypeName(),
                        parser.make.anyType());
                        tvars.enter(s);
                return s;
            } else
                return Symbol.NONE;
        }

        private String nextToken() {
            do {
                token = scanner.nextToken().trim();
            } while (token.length() == 0);
            return token;
        }

        protected Type parse() {
            if (scanner.hasMoreTokens()) {
                nextToken();
                if (!scanner.hasMoreTokens())
                    return defaultType;
                if ("class".equals(token))
                    return parseMetaClass();
                if ("method".equals(token))
                    return parseMetaMethod();
                if ("field".equals(token))
                    return parseMetaField();
                if ("constr".equals(token))
                    return parseConstrField();
            }
            return defaultType;
        }

        protected Type parseMetaClass() {
            nextToken();
            //System.out.println("parse meta class " + token);//DEBUG
            if ("[".equals(token)) {
                try {
                    Vector syms = new Vector();
                    do {
                        nextToken();
                        int vflag = 0;
                        if (token.equals("+")) {
                            nextToken();
                            vflag = Modifiers.COVARIANT;
                        } else if (token.equals("-")) {
                            nextToken();
                            vflag = Modifiers.CONTRAVARIANT;
                        }
                        assert token.startsWith("?");
                        Symbol s = getTVar(token);
                        if (s == Symbol.NONE)
                            return defaultType;
                        s.flags |= vflag;
                        nextToken();
                        //System.out.println("new var " + s + ", " + token);//DEBUG
                        if (token.equals("<")) {
                            nextToken();
                            s.setInfo(parseType());
                        }
                        syms.add(s);
                    } while (token.equals(","));
                    assert "]".equals(token);
                    nextToken();
                    Symbol[] smbls = (Symbol[])syms.toArray(new Symbol[syms.size()]);
                    //System.out.println("*** " + syms);//DEBUG
                    Type clazztype = Type.appliedType(
                        parser.ctype, Symbol.type(smbls));
                    Symbol constr = parser.c.primaryConstructor();
                    switch (constr.rawInfo()) {
                    case MethodType(Symbol[] vparams, _):
                        constr.setInfo(
                            Type.PolyType(
                                smbls, Type.MethodType(vparams, clazztype)));
                        break;
                    default:
                        throw new ApplicationError(constr.rawInfo());
                    }
                } catch (NoSuchElementException e) {
                }
            }
            Type res = defaultType;
            if ("extends".equals(token)) {
                Vector basetpes = new Vector();
                do {
                    nextToken();
                    basetpes.add(parseType());
                } while (token.equals("with"));
                switch (defaultType) {
                    case CompoundType(_, Scope scope):
                        res = Type.compoundType(
                            (Type[])basetpes.toArray(new Type[basetpes.size()]),
                            scope,
                            defaultType.symbol());
                }
            }
            assert ";".equals(token);
            return res;
        }

        protected Type parseType() {
            String name = token;
            Symbol s = getTVar(name);
            nextToken();
            if (s != Symbol.NONE)
                return s.type();
            Type clazztype = parser.make.classType(Name.fromString(name)).unalias();
            if (token.equals("[")) {
                Vector types = new Vector();
                do {
                    nextToken();
                    types.add(parseType());
                } while (token.equals(","));
                assert "]".equals(token);
                nextToken();
                Type[] args = new Type[types.size()];
                types.toArray(args);
                return Type.appliedType(clazztype, args);
            } else {
                return clazztype;
            }
        }

        protected Type parseMetaMethod() {
            locals = new Scope();
            try {
                nextToken();
                Symbol[] smbls = null;
                //System.out.println("parse meta method " + token);
                if ("[".equals(token)) {
                    Vector syms = new Vector();
                    do {
                        nextToken();
                        if ("]".equals(token))
                            break;
                        assert token.startsWith("?");
                        Symbol s = owner.newTParam(
                                        Position.NOPOS,
                                        0,
                                        Name.fromString(token).toTypeName(),
                                        parser.make.anyType());
                                        locals.enter(s);
                        nextToken();
                        if (token.equals("<")) {
                            nextToken();
                            s.setInfo(parseType());
                        }
                        syms.add(s);
                    } while (token.equals(","));
                    assert "]".equals(token);
                    nextToken();
                    smbls = (Symbol[])syms.toArray(new Symbol[syms.size()]);
                }
                if ("(".equals(token)) {
                    int i = 0;
                    Vector params = new Vector();
                    do {
                        nextToken();
                        if (")".equals(token))
                            break;
                        int flags = 0;
                        if ("def".equals(token)) {
                            nextToken();
                            flags |= Modifiers.DEF;
                        }
                        params.add(owner.newVParam(
                            Position.NOPOS,
                            flags,
                            Name.fromString("x" + (i++))).setInfo(parseType()));
                        //System.out.println("  + " + token);
                    } while (token.equals(","));
                    assert ")".equals(token);
                    nextToken();
                    //System.out.println("+++ method " + token);
                    Type restpe = parseType();
                    assert ";".equals(token);
                    if (smbls == null)
                        return Type.MethodType(
                                (Symbol[])params.toArray(new Symbol[params.size()]),
                                restpe);
                    else
                        return Type.PolyType(
                                    smbls,
                                    Type.MethodType(
                                        (Symbol[])params.toArray(new Symbol[params.size()]),
                                        restpe));
                } else {
                    Type res = parseType();
                    assert ";".equals(token);
                    if (smbls == null)
                        return Type.PolyType(Symbol.EMPTY_ARRAY, res);
                    else
                        return Type.PolyType(smbls, res);
                }
            } catch (NoSuchElementException e) {
                return defaultType;
            } finally {
                locals = null;
            }
        }

        protected Type parseMetaField() {
            nextToken();
            return parseType();
        }

        protected Type parseConstrField() {
            try {
                nextToken();
                //System.out.println("+++ constr " + token);
                if ("(".equals(token)) {
                    int i = 0;
                    Vector params = new Vector();
                    do {
                        nextToken();
                        if (")".equals(token))
                            break;
                        params.add(owner.newVParam(
                            Position.NOPOS,
                            0,
                            Name.fromString("x" + (i++))).setInfo(parseType()));
                        //System.out.println("  + " + token);
                    } while (token.equals(","));
                    assert ")".equals(token);
                    nextToken();
                    assert ";".equals(token);
                    return Type.MethodType(
                        (Symbol[])params.toArray(new Symbol[params.size()]),
                        parser.ctype);
                } else {
                    assert ";".equals(token);
                    return Type.PolyType(Symbol.EMPTY_ARRAY, parser.ctype);
                }
            } catch (NoSuchElementException e) {
                return defaultType;
            }
        }
    }
}

/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.symtab.classfile;

import scala.tools.util.Position;
import scala.tools.util.AbstractFileReader;
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
    public AttributeParser(AbstractFileReader in,
                           ConstantPool pool,
                           ClassfileParser parser)
    {
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
            try {
                UnPickle.parse(parser.global, in.nextBytes(attrLen), sym);
                return;
            } catch (UnPickle.BadSignature exception) {
                throw new RuntimeException(exception);
            }
        case INNERCLASSES_ATTR:
            int n = in.nextChar();
            //System.out.println(sym + " has " + n + " innerclass entries");
            for (int i = 0; i < n; i++) {
                int inner = in.nextChar();
                if (inner == 0) { in.skip(6); continue; }
                int outer = in.nextChar();
                if (outer == 0) { in.skip(4); continue; }
                int name = in.nextChar();
                if (name == 0) { in.skip(2); continue; }
                int flags = in.nextChar();
                if ((flags & JAVA_ACC_STATIC) == 0) continue;
                if ((flags & (JAVA_ACC_PUBLIC | JAVA_ACC_PROTECTED)) == 0)
                    continue;
                if (pool.getClass(outer) != sym) continue;
                Symbol alias = sym.linkedModule().moduleClass()
                    .newTypeAlias(Position.NOPOS, 0,
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
            //  thrown[j] = make.classType(reader.readClassName(in.nextChar()));
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
            MetaParser mp = new MetaParser
                (meta, tvars, sym, type, parser.c, parser.ctype, parser.make);
            sym.setInfo(mp.parse());
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
        case SOURCEFILE_ATTR:
            String name = pool.getString(in.nextChar());
            parser.c.getOrigin().setSourceFileAttribute(name);
            parser.m.moduleClass().getOrigin().setSourceFileAttribute(name);
            return;
        default:
            in.skip(attrLen);
            return;
        }
    }

    Scope tvars = new Scope();
}

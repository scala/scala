/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.symtab.classfile;

import scala.tools.util.AbstractFile;
import scala.tools.util.Position;
import scalac.*;
import scalac.util.*;
import scalac.symtab.*;
import java.io.*;
import java.util.*;

//todo: don't keep statics module in scope.

public class ClassfileParser implements ClassfileConstants {

    static final int CLASS_ATTR  = SOURCEFILE_ATTR
                                 | INNERCLASSES_ATTR
                                 | SYNTHETIC_ATTR
                                 | DEPRECATED_ATTR
                                 | META_ATTR
                                 | SCALA_ATTR
                                 | JACO_ATTR
                                 | SIG_ATTR;
    static final int METH_ATTR   = CODE_ATTR
                                 | EXCEPTIONS_ATTR
                                 | SYNTHETIC_ATTR
                                 | DEPRECATED_ATTR
                                 | META_ATTR
                                 | SIG_ATTR
                                 | BRIDGE_ATTR;
    static final int FIELD_ATTR  = CONSTANT_VALUE_ATTR
                                 | SYNTHETIC_ATTR
                                 | DEPRECATED_ATTR
                                 | META_ATTR
                                 | SIG_ATTR;

    protected final Global global;
    protected final AbstractFileReader in;
    protected final Symbol c;
    protected final Type ctype;
    protected final JavaTypeFactory make;
    protected final ConstantPool pool;
    protected final AttributeParser attrib;
    protected final Scope locals;
    protected final Scope statics;


    private ClassfileParser(Global global, AbstractFileReader in, Symbol c, JavaTypeFactory make, ConstantPool pool) {
        this.global = global;
        this.in = in;
        this.c = c;
        this.ctype = make.classType(c);
        this.make = make;
        this.pool = pool;
        this.attrib = new AttributeParser(in, pool, this);
        this.locals = new Scope();
        this.statics = new Scope();
    }


    /** parse the classfile and throw IO exception if there is an
     *  error in the classfile structure
     */
    public static void parse(Global global, AbstractFile file, Symbol c) throws IOException {
        AbstractFileReader in = new AbstractFileReader(file);
        try {
            int magic = in.nextInt();
            if (magic != JAVA_MAGIC)
                throw new IOException("class file '" + in.path + "' "
                    + "has wrong magic number 0x" + Integer.toHexString(magic)
                    + ", should be 0x" + Integer.toHexString(JAVA_MAGIC));
            int minorVersion = in.nextChar();
            int majorVersion = in.nextChar();
            if ((majorVersion < JAVA_MAJOR_VERSION) ||
                ((majorVersion == JAVA_MAJOR_VERSION) &&
                 (minorVersion < JAVA_MINOR_VERSION)))
                throw new IOException("class file '" + in.path + "' "
                    + "has unknown version "
                    + majorVersion + "." + minorVersion
                    + ", should be less than "
                    + JAVA_MAJOR_VERSION + "." + JAVA_MINOR_VERSION);
            JavaTypeFactory make = new JavaTypeCreator(global.definitions);
            Signatures sigs = new Signatures(global, make, in);
            ConstantPool pool = new ConstantPool(in, sigs);
            int flags = in.nextChar();
            Symbol clasz = pool.getClass(in.nextChar());
            if (c != clasz)
                throw new IOException("class file '" + in.path + "' "
                    + "contains wrong class " + clasz.staticType());
            new ClassfileParser(global, in, c, make, pool).parse(flags);
        } catch (RuntimeException e) {
            if (global.debug) e.printStackTrace();
            throw new IOException("class file '" + in.path + "' is broken");
        }
    }

    protected void parse(int flags) {
        {
            // todo: correct flag transition
            c.flags = transFlags(flags);
            if ((c.flags & Modifiers.DEFERRED) != 0)
                c.flags = c.flags & ~Modifiers.DEFERRED | Modifiers.ABSTRACT;
            Type supertpe = readClassType(in.nextChar());
            Type[] basetpes = new Type[in.nextChar() + 1];
            // set info of class
            Type classInfo = Type.compoundType(basetpes, locals, c);
            c.setInfo(classInfo);
            // set info of statics class
            Symbol staticsClass = c.dualClass();
            assert staticsClass.isModuleClass(): Debug.show(staticsClass);
            Type staticsInfo = Type.compoundType(Type.EMPTY_ARRAY, statics, staticsClass);
            staticsClass.setInfo(staticsInfo);
            staticsClass.module().setInfo(make.classType(staticsClass));
            basetpes[0] = supertpe;
            for (int i = 1; i < basetpes.length; i++)
                basetpes[i] = readClassType(in.nextChar());
            int fieldCount = in.nextChar();
            for (int i = 0; i < fieldCount; i++)
                parseField();
            int methodCount = in.nextChar();
            for (int i = 0; i < methodCount; i++)
                parseMethod();

            Symbol constr = c.primaryConstructor();
            if (!constr.isInitialized()) {
                constr.setInfo(
                    Type.MethodType(Symbol.EMPTY_ARRAY, ctype));
                if ((c.flags & Modifiers.INTERFACE) == 0)
                    constr.flags |= Modifiers.PRIVATE;
            }
            attrib.readAttributes(c, classInfo, CLASS_ATTR);
            //System.out.println("dynamic class: " + c);
            //System.out.println("statics class: " + staticsClass);
            //System.out.println("module: " + c.module());
            //System.out.println("modules class: " + c.module().type().symbol());
        }
    }

    /** convert Java modifiers into Scala flags
     */
    public int transFlags(int flags) {
        int res = 0;
        if ((flags & JAVA_ACC_PRIVATE) != 0)
            res |= Modifiers.PRIVATE;
        else if ((flags & JAVA_ACC_PROTECTED) != 0)
            res |= Modifiers.PROTECTED;
        else if ((flags & JAVA_ACC_PUBLIC) == 0)
            res |= Modifiers.PRIVATE;
        if ((flags & JAVA_ACC_ABSTRACT) != 0)
            res |= Modifiers.DEFERRED;
        if ((flags & JAVA_ACC_FINAL) != 0)
            res |= Modifiers.FINAL;
        if ((flags & JAVA_ACC_INTERFACE) != 0)
            res |= Modifiers.INTERFACE | Modifiers.TRAIT | Modifiers.ABSTRACT;
        if ((flags & JAVA_ACC_SYNTHETIC) != 0)
            res |= Modifiers.SYNTHETIC;
        return res | Modifiers.JAVA;
    }

    /** read a class name and return the corresponding class type
     */
    protected Type readClassType(int i) {
        return i == 0 ? make.anyType() : make.classType(pool.getClass(i));
    }

    /** read a field
     */
    protected void parseField() {
        int jflags = in.nextChar();
        int sflags = transFlags(jflags);
        if ((jflags & JAVA_ACC_FINAL) == 0) sflags |= Modifiers.MUTABLE;
        if ((sflags & Modifiers.PRIVATE) != 0) {
            in.skip(4);
            attrib.skipAttributes();
        } else {
            Name name = pool.getName(in.nextChar());
            Symbol owner = getOwner(jflags);
            Symbol symbol = owner.newTerm(Position.NOPOS, sflags, name);
            Type type = pool.getFieldType(in.nextChar());
            symbol.setInfo(type);
            attrib.readAttributes(symbol, type, FIELD_ATTR);
            getScope(jflags).enterOrOverload(symbol);
        }
    }

    /** read a method
     */
    protected void parseMethod() {
        int jflags = in.nextChar();
        int sflags = transFlags(jflags);
        if ((jflags & JAVA_ACC_BRIDGE) != 0) sflags |= Modifiers.BRIDGE;
        if ((sflags & Modifiers.PRIVATE) != 0) {
            in.skip(4);
            attrib.skipAttributes();
        } else {
            Name name = pool.getName(in.nextChar());
            Type type = pool.getMethodType(in.nextChar());
            Symbol owner = getOwner(jflags);
            Symbol symbol;
            boolean newConstructor = false;
            if (name == CONSTR_N) {
                switch (type) {
                case MethodType(Symbol[] vparams, _):
                    type = Type.MethodType(vparams, ctype);
                    break;
                default:
                    throw Debug.abort("illegal case", type);
                }
                symbol = owner.primaryConstructor();
                if (symbol.isInitialized()) {
                    symbol = owner.newConstructor(Position.NOPOS, sflags);
                    newConstructor = true;
                } else {
                    symbol.flags = sflags;
                }
            } else {
                symbol = owner.newTerm(Position.NOPOS, sflags, name);
            }
            setParamOwners(type, symbol);
            symbol.setInfo(type);
            attrib.readAttributes(symbol, type, METH_ATTR);
            if (name != CONSTR_N) getScope(jflags).enterOrOverload(symbol);
            else if (newConstructor) owner.addConstructor(symbol);
        }
    }

    /** return the owner of a member with given java flags
     */
    private Symbol getOwner(int jflags) {
        return (jflags & JAVA_ACC_STATIC) != 0 ? c.dualClass() : c;
    }

    /** return the scope of a member with given java flags
     */
    private Scope getScope(int jflags) {
        return (jflags & JAVA_ACC_STATIC) != 0 ? statics : locals;
    }

    private void setParamOwners(Type type, Symbol owner) {
        switch (type) {
            case PolyType(Symbol[] params, Type result):
				for (int i = 0; i < params.length; i++)
					params[i].setOwner(owner);
				setParamOwners(result, owner);
				break;
			case MethodType(Symbol[] params, Type result):
				for (int i = 0; i < params.length; i++) params[i].setOwner(owner);
				setParamOwners(result, owner);
				break;
		}
    }
}

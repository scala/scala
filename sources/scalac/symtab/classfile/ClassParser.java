/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.symtab.classfile;

import scalac.*;
import scalac.symtab.*;
import scalac.util.*;
import java.io.*;


public class ClassParser extends Type.LazyType {

    /** the global compilation environment
     */
    protected Global global;

    public ClassParser(Global global) {
        this.global = global;
    }

    /** complete class symbol c by loading the class
     */
    public void complete(Symbol c) {
        //System.out.println("loading " + c);//DEBUG
        try {
            long msec = System.currentTimeMillis();
            String filename = externalizeFileName(c.fullName()) + ".class";
            AbstractFile f = global.classPath.openFile(filename);
            if (f == null)
                global.error("could not read class " + c);
            else {
                new ClassfileParser(global, new AbstractFileReader(f), c).parse();
                global.operation("loaded " + f.getPath() + " in " +
                    (System.currentTimeMillis() - msec) + "ms");
                //for (Definition e = c.locals().elems; e != null; e = e.sibling)
                //  if (e.def.kind == TYP)
                //      e.def.complete();
            }
        } catch (IOException e) {
            e.printStackTrace();
            global.error("i/o error while loading " + c);
            c.setInfo(Type.ErrorType);
        }
    }

    /** return external representation of file name s,
     *  converting '.' to File.separatorChar
     */
    public String externalizeFileName(Name n) {
        if ((n == null) || (n.length() == 0))
            return ".";
        byte[] ascii = n.toAscii();
        String s = SourceRepresentation.ascii2string(
            ascii, 0, ascii.length);
        return s.replace('.', File.separatorChar);
    }

    public Type.LazyType staticsParser(Symbol clazz) {
        return new StaticsParser(clazz);
    }

    public Type.LazyType aliasParser(Symbol alias) {
        return new AliasParser(alias);
    }

    class StaticsParser extends Type.LazyType {
        Symbol clazz;

        StaticsParser(Symbol clazz) {
            this.clazz = clazz;
        }

        public void complete(Symbol statics) {
            ClassParser.this.complete(clazz);
        }

	public String toString() {
	    return "StaticsParser(" + clazz + ")";
	}
    }

    class AliasParser extends Type.LazyType {
        Symbol alias;

        AliasParser(Symbol alias) {
            this.alias = alias;
        }

        public void complete(Symbol c) {
            try {
                long msec = System.currentTimeMillis();
                String filename = externalizeFileName(alias.fullName()) + ".class";
                AbstractFile f = global.classPath.openFile(filename);
                if (f == null)
                    global.error("could not read class " + c);
                else {
                    new ClassfileParser(global, new AbstractFileReader(f), c).parse();
                    global.operation("loaded " + f.getPath() + " in " +
                        (System.currentTimeMillis() - msec) + "ms");
                }
            } catch (IOException e) {
                e.printStackTrace();
                global.error("i/o error while loading " + c);
                c.setInfo(Type.ErrorType);
            }
        }
    }
}


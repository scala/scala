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


public class ClassParser extends SymbolLoader {

    public ClassParser(Global global) {
        super(global);
    }

    protected String doComplete(Symbol clasz) throws IOException {
        AbstractFile file = global.classPath.openFile(
            SourceRepresentation.externalizeFileName(clasz, ".class"));
        new ClassfileParser(global,new AbstractFileReader(file),clasz).parse();
        return "class file '" + file.getPath() + "'";
    }

    public Type.LazyType staticsParser(Symbol clazz) {
        return new StaticsParser(clazz);
    }

    public Type.LazyType aliasParser(Symbol alias) {
        return new AliasParser(alias);
    }

    public class StaticsParser extends Type.LazyType {
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
            Phase phase = global.currentPhase;
            global.currentPhase = global.PHASE.ANALYZER.phase();
            try {
                long msec = System.currentTimeMillis();
                String filename = SourceRepresentation.externalizeFileName(
		    alias, ".class");
                AbstractFile f = global.classPath.openFile(filename);
                if (f == null)
                    global.error("could not read class " + c);
                else {
                    new ClassfileParser(global, new AbstractFileReader(f), c).parse();
                    global.operation("loaded " + f.getPath() + " in " +
                        (System.currentTimeMillis() - msec) + "ms");
                }
            } catch (IOException e) {
                if (global.debug) e.printStackTrace();
                global.error("i/o error while loading " + c);
                c.setInfo(Type.ErrorType);
            }
            global.currentPhase = phase;
        }
    }
}


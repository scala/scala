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


public class ClassParser extends MetadataParser {

    public ClassParser(Global global) {
        super(global);
    }

    protected void doComplete(Symbol c) {
	c.owner().initialize();
	//System.out.println("loading " + c);//DEBUG
	try {
	    long msec = System.currentTimeMillis();
	    String filename = SourceRepresentation.externalizeFileName(
		c.fullName()) + ".class";
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
	    if (global.debug) e.printStackTrace();
	    global.error("i/o error while loading " + c);
	    c.setInfo(Type.ErrorType);
	}
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
		    alias.fullName()) + ".class";
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


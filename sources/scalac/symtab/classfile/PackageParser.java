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

public class PackageParser extends Type.LazyType {

    /** the global compilation environment
     */
    protected Global global;

    /** the class parser
     */
    public ClassParser classCompletion;

    public PackageParser(Global global) {
        this.global = global;
        this.classCompletion = new ClassParser(global);
    }

    /** complete package type symbol p by loading all package members
     */
    public void complete(Symbol p) {
        long msec = System.currentTimeMillis();
        Scope members = new Scope();
        String dirname = null;
        Name name = p.fullName();
        if (name.length() == 0) {
            // includeMembers(AbstractFile.open(null, "."), p, members, false);
        } else {
            dirname = externalizeFileName(name);
	    assert !dirname.startsWith("com") : p;//debug
            if (!dirname.endsWith("/"))
                dirname += "/";
        }
        String[] base = global.classPath.components();
        for (int i = 0; i < base.length; i++) {
            includeMembers(
		AbstractFile.open(base[i], dirname), p, members, dirname != null);
	}
        p.setInfo(Type.compoundType(Type.EMPTY_ARRAY, members, p));
        if (dirname == null)
            dirname = "anonymous package";
        global.operation("scanned " + dirname + " in " +
                    (System.currentTimeMillis() - msec) + "ms");
    }

    /** read directory of a classpath directory and include members
     *  in package/module scope
     */
    protected void includeMembers(AbstractFile dir, Symbol p, Scope locals,
				  boolean inclClasses) {
        if (dir == null)
            return;
        String[] filenames = null;
        try {
            if ((filenames = dir.list()) == null)
                return;
            for (int j = 0; j < filenames.length; j++) {
                String fname = filenames[j];
                if (inclClasses && fname.endsWith(".class")) {
                    Name n = Name.fromString(fname.substring(0, fname.length() - 6))
			.toTypeName();
		    ClassSymbol clazz = new ClassSymbol(n, p, classCompletion);
		    clazz.constructor().setInfo(
			classCompletion.staticsParser(clazz));
		    // enter class
		    locals.enter(clazz);
		    locals.enter(clazz.constructor());
		    // enter module, except for scala.Object class
		    // todo: why not there also?.
		    if (!(n == Names.Object.toTypeName() &&
			  p.fullName().toTermName() == Names.scala)) {
			Scope.Entry e = locals.lookupEntry(clazz.module().name);
			if (e != Scope.Entry.NONE) {
			    // we already have a package of the same name; delete it
			    locals.unlink(e);
			}
			locals.enter(clazz.module());
		    }
                } else if (fname.endsWith("/") && !fname.equals("META-INF/")) {
                    Name n = Name.fromString(fname.substring(0, fname.length() - 1));
                    if (locals.lookup(n) == Symbol.NONE) {
                        TermSymbol module = TermSymbol.newJavaPackageModule(n, p, this);
                        locals.enter(module);
                    }
                } else if (fname.endsWith(".scala")) {
                    Name n = Name.fromString(fname.substring(0, fname.length() - 6))
			.toTypeName();
                    if (locals.lookup(n) == Symbol.NONE) {
                        SourceCompleter completer = new SourceCompleter(global,
                            dir.getPath() + File.separatorChar + fname);
                        ClassSymbol clazz = new ClassSymbol(n, p, completer);
                        clazz.constructor().setInfo(completer);
			clazz.module().setInfo(completer);
                        // enter class
                        locals.enter(clazz);
			locals.enter(clazz.constructor());
			locals.enter(clazz.module());
                    }
                }
            }
        } catch (IOException e) {
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
}
